-module(dkg_hybriddkg).

-export([init/8,
         input/2, % for testing
         start/1,
         serialize/1,
         deserialize/2,
         status/1
        ]).

-export([handle_msg/3]).

-record(dkg, {
          id :: pos_integer(),
          n :: pos_integer(),
          f :: pos_integer(),
          t :: pos_integer(),
          u :: erlang_pbc:element(),
          u2 :: erlang_pbc:element(),
          shares_map = #{} :: shares_map(),
          shares_results = #{} :: shares_results(),
          shares_seen = [] :: node_set(),
          shares_acked = [] :: node_set(),
          echo_proofs = [] :: echo_proofs(),
          echo_counts = #{} :: echo_counts(),
          ready_proofs = [] :: ready_proofs(),
          ready_counts = #{} :: ready_counts(),
          leader :: pos_integer(),
          leader_cap :: pos_integer(),
          leader_changing = false :: boolean(),
          leader_vote_counts = #{} :: leader_vote_counts(),
          await_vss = false :: boolean(),
          elections_allowed = false :: boolean()
         }).

-record(serialized_dkg, {
          id :: pos_integer(),
          n :: pos_integer(),
          f :: pos_integer(),
          t :: pos_integer(),
          u :: binary(),
          u2 :: binary(),
          shares_map :: #{pos_integer() => dkg_hybridvss:serialized_vss()},
          shares_results = #{} :: serialized_shares_results(),
          shares_acked = [] :: node_set(),
          shares_seen = [] :: node_set(),
          echo_proofs = [] :: echo_proofs(),
          echo_counts = #{} :: echo_counts(),
          ready_proofs = [] :: ready_proofs(),
          ready_counts = #{} :: ready_counts(),
          leader_changing = false :: boolean(),
          leader :: pos_integer(),
          leader_cap :: pos_integer(),
          leader_vote_counts = #{} :: leader_vote_counts(),
          await_vss = false :: boolean(),
          elections_allowed = false :: boolean()
         }).

-type ready_proofs() :: [shares_ready()].
-type node_set() :: [pos_integer()].
-type echo_proofs() :: [signed_ready() | signed_echo()].
-type signed_leader_change() :: {signed_leader_change, pos_integer(), node_set(), ready_proofs() | echo_proofs()}.
-type signed_echo() :: {signed_echo, node_set()}.
-type signed_ready() :: {signed_ready, node_set()}.
-type shares_ready() :: {signed_shares_ready, dkg_commitment:readies()}.
-type identity() :: {Leader :: pos_integer(), Q :: [pos_integer()]}.
-type echo() :: {Sender :: pos_integer(), SignedEcho :: signed_echo()}.
-type ready() :: {Sender :: pos_integer(), SignedReady :: signed_ready()}.
-type echo_counts() :: #{identity() => [echo()]}.
-type ready_counts() :: #{identity() => [ready()]}.
-type leader_vote_counts() :: #{Leader :: pos_integer() => [{Sender :: pos_integer(), signed_leader_change()}]}.
-type shares_map() :: #{pos_integer() => dkg_hybridvss:vss()}.
-type serialized_shares_map() :: #{pos_integer() => dkg_hybridvss:serialized_vss()}.
-type shares_results() :: #{pos_integer() => {C :: dkg_commitment:commitment(), Si :: erlang_pbc:element()}}.
-type serialized_shares_results() :: #{pos_integer() => {C :: dkg_commitment:serialized_commitment(), Si :: binary()}}.
-type dkg() :: #dkg{}.
-type serialized_dkg() :: #serialized_dkg{}.

-export_type([dkg/0]).

%% Glossary
%% L -> Leader
%% Lnext -> LeaderCap
%% Q -> mostly contextual for recieved share info
%% Qhat -> SharesSeen
%% Qbar -> SharesAcked (by any leader)
%% R/M -> Proofs (both are verified, only the verification step cares
%%        which kind)
%% Rhat -> ReadyProofs
%% Mbar -> EchoProofs

%% upon initialization:
%%      e(L, Q) <- 0 and r(L, Q) <- 0 for every Q; Qbar <- ∅; Q <- ∅
%%      Mbar <- Rhat <- n-t-f signed lead-ch messages for leader L
%%      cnt ← 0; cntl ← 0 for all l ∈ [1, n];
%%      lcl ← 0 for each leader L; lcflag ← false
%%      Lnext ← L + n − 1
%%      for all d ∈ [1, n] do
%%          initialize extended-HybridVSS Sh protocol (Pd, τ )
init(Id, N, F, T, G1, G2, Round, Options) ->
    true = N >= (3*T + 2*F + 1),
    erlang_pbc:element_pp_init(G1),
    erlang_pbc:element_pp_init(G2),
    Callback = proplists:get_value(callback, Options, false),
    %% Don't allow elections by default. We don't have signed proofs they should
    %% occur so it's not safe to enable this in a non development context.
    Elections = proplists:get_value(elections, Options, false),
    Shares = lists:foldl(fun(E, Map) ->
                                 Share = dkg_hybridvss:init(Id, N, F, T, G1, G2, {E, Round}, Callback),
                                 Map#{E => Share}
                         end, #{}, dkg_util:allnodes(N)),

    #dkg{id = Id,
         n = N,
         f = F,
         t = T,
         u = G1,
         u2 = G2,
         leader = 1,
         leader_cap = leader_cap(1, N),
         shares_map = Shares,
         elections_allowed = Elections
        }.

%% the dgk starts to go on its own, this is here for the sake of
%% fakecast for now, since all the other protocols take input.
input(State, _) ->
    start(State).

start(DKG = #dkg{id=Id, u=G1}) ->
    #{Id := MyShares} = SharesMap = DKG#dkg.shares_map,
    Secret = erlang_pbc:element_random(erlang_pbc:element_new('Zr', G1)),
    {NewShares, {send, ToSend}} = dkg_hybridvss:input(MyShares, Secret),
    {DKG#dkg{shares_map = SharesMap#{Id => NewShares}},
     {send, dkg_util:wrap({vss, Id}, ToSend)}}.

handle_msg(DKG=#dkg{await_vss = true}, Sender, {{vss, SharesId}, SharesMsg}) ->
    case dkg_hybridvss:handle_msg(maps:get(SharesId, DKG#dkg.shares_map), Sender, SharesMsg) of
        {_Shares, ignore} ->
            {DKG, ignore};
        {NewShares, ok} ->
            {DKG#dkg{shares_map=maps:put(SharesId, NewShares, DKG#dkg.shares_map)}, ok};
        {NewShares, {send, ToSend}} ->
            {DKG#dkg{shares_map=maps:put(SharesId, NewShares, DKG#dkg.shares_map)},
             {send, dkg_util:wrap({vss, SharesId}, ToSend)}};
        {NewShares, {result, {_Session, Commitment, Si}}} ->
            NewDKG = DKG#dkg{shares_map = maps:put(SharesId, NewShares, DKG#dkg.shares_map),
                             shares_results = maps:put(SharesId, {Commitment, Si}, DKG#dkg.shares_results),
                             shares_seen = [SharesId | DKG#dkg.shares_seen],
                             %% XXX these readies are currently not signed
                             ready_proofs = [{signed_shares_ready, dkg_commitment:readies(Commitment)} | DKG#dkg.ready_proofs]
                            },
            case output_ready(NewDKG, NewDKG#dkg.shares_acked) of
                true ->
                    {Shard, VerificationKey, PublicKeyShares} = output(NewDKG, NewDKG#dkg.shares_acked),
                    {NewDKG, {result, {Shard, VerificationKey, PublicKeyShares}}};
                false ->
                    {NewDKG, ok}
            end
    end;
handle_msg(DKG=#dkg{leader = Leader}, Sender, {{vss, SharesId}, SharesMsg}) ->
    case dkg_hybridvss:handle_msg(maps:get(SharesId, DKG#dkg.shares_map), Sender, SharesMsg) of
        {_Shares, ignore} ->
            {DKG, ignore};
        {NewShares, ok} ->
            {DKG#dkg{shares_map=maps:put(SharesId, NewShares, DKG#dkg.shares_map)}, ok};
        {NewShares, {send, ToSend}} ->
            {DKG#dkg{shares_map=maps:put(SharesId, NewShares, DKG#dkg.shares_map)}, {send, dkg_util:wrap({vss, SharesId}, ToSend)}};
        {NewShares, {result, {_Session, Commitment, Si}}} ->
            %% upon (Pd, τ, out, shared, Cd , si,d , Rd ) (first time):
            %%      Qhat ← {Pd}; Rhat ← {Rd}
            %%      if |Qhat| = t + 1 and Qbar = ∅ then
            %%           if Pi = L then
            %%               send the message (L, τ, send, Qhat, Rhat) to each Pj
            %%            else
            %%               delay ← delay(T); start timer(delay)
            %% TODO 'extended' VSS should output signed ready messages

            NewDKG = DKG#dkg{shares_map = maps:put(SharesId, NewShares, DKG#dkg.shares_map),
                             shares_results = maps:put(SharesId, {Commitment, Si}, DKG#dkg.shares_results),
                             shares_seen = [SharesId | DKG#dkg.shares_seen],
                             %% XXX these readies are currently not signed
                             ready_proofs = [{signed_shares_ready, dkg_commitment:readies(Commitment)} | DKG#dkg.ready_proofs]
                            },
            case length(NewDKG#dkg.shares_seen) == NewDKG#dkg.t + 1 andalso length(NewDKG#dkg.shares_acked) == 0 of
                true ->
                    case NewDKG#dkg.id == Leader of
                        true ->
                            {NewDKG, {send, [{multicast, {send, NewDKG#dkg.shares_seen, {ready_proofs, NewDKG#dkg.ready_proofs}}}]}};
                        false ->
                            {NewDKG, start_timer}
                    end;
                false ->
                    {NewDKG, ok}
            end
    end;

%% upon a message (L, τ, send, Q, R/M) from L (first time):
%%      if verify-signature(Q, R/M) and (Qbar = ∅  or Qbar = Q) then
%%          send the message (L, τ, echo, Q)sign to each Pj
handle_msg(DKG, _Sender, {send, SharesSent, _Proof}) ->
    %% TODO verify signatures
    case length(DKG#dkg.shares_acked) == 0 orelse
        lists:usort(DKG#dkg.shares_acked) == lists:usort(SharesSent) of
        true ->
            {DKG, {send, [{multicast, {signed_echo, SharesSent}}]}};
        false ->
            {DKG, ok}
    end;

%% upon a message (L, τ, echo, Q)sign from Pm (first time):
%%      e(L,Q) ← e(L,Q) + 1
%%      if e(L,Q) = ceil((n+t+1)/2) and r(L,Q) < t + 1 then
%%          Qbar ← Q; Mbar ← ceil((n+t+1)/2) signed echo messages for Q
%%          send the message (L, τ, ready, Q)sign to each Pj
handle_msg(DKG = #dkg{id=_Id, n=N, t=T}, Sender, {signed_echo, Shares}=EchoMsg) ->
    case update_echo_counts(DKG, Sender, EchoMsg) of
        false ->
            {DKG, ok};
        {true, NewDKG} ->
            case count_echo(NewDKG, Shares) == ceil((N+T+1)/2) andalso count_ready(NewDKG, Shares) < T+1 of
                true ->
                    %% update shares_acked
                    NewSharesAcked = Shares,
                    %% update echo_proofs
                    NewProofs = get_echo(NewDKG, Shares),
                    %% send ready message
                    {NewDKG#dkg{shares_acked=NewSharesAcked, echo_proofs=NewProofs}, {send, [{multicast, {signed_ready, Shares}}]}};
                false ->
                    {NewDKG, ok}
            end
    end;

%% upon a message (L, τ, ready, Q)sign from Pm (first time):
%%      r(L,Q) ← r(L,Q) + 1
%%      if r(L,Q) = t + 1 and e(L,Q) < ceil((n+t+1)2) then
%%          Qbar ← Q; Mbar ← t + 1 signed ready messages for Q
%%          send the message (L, τ, ready, Q)sign to each Pj
%%      else if r(L,Q) = n − t − f then
%%          STOP TIMER, if any
%%          WAIT for shared output-messages for each Pd ∈ Q
%%          si ← SUM(si,d) ∀Pd ∈ Q; ∀p,q : C ← MUL(Cd)p,q ∀Pd ∈ Q
%%          output (L, τ, DKG-completed, C, si)
handle_msg(DKG = #dkg{id=_Id, n=N, t=T, f=F}, Sender, {signed_ready, Shares}=ReadyMsg) ->
    case update_ready_counts(DKG, Sender, ReadyMsg) of
        false ->
            {DKG, ok};
        {true, NewDKG} ->
            case count_ready(NewDKG, Shares) == T+1 andalso
                count_echo(NewDKG, Shares) < ceil((N+T+1)/2) of
                true ->
                    %% NOTE: shares_acked and echo_proofs are new assignments here
                    NewSharesAcked = Shares,
                    NewProofs = get_ready(NewDKG, Shares),
                    {NewDKG#dkg{shares_acked=NewSharesAcked, echo_proofs=NewProofs},
                     {send, [{multicast, {signed_ready, Shares}}]}};
                false ->
                    case count_ready(NewDKG, Shares) == N-T-F of
                        true ->
                            %% TODO stop timer
                            case output_ready(NewDKG, Shares) of
                                true ->
                                    {Shard, VerificationKey, PublicKeyShares} = output(NewDKG, Shares),
                                    {NewDKG, {result, {Shard, VerificationKey, PublicKeyShares}}};
                                false ->
                                    {NewDKG#dkg{await_vss = true}, ok}
                            end;
                            %% TODO presumably we need to check this when we get a VSS result as well?
                        false ->
                            {NewDKG, ok}
                    end
            end
    end;
handle_msg(DKG, _Sender, {signed_ready, _SharesDone}=_ReadyMsg) ->
    %% DKG received ready message from itself, what to do?
    {DKG, ok};

%% upon timeout:
%% if lcflag = false then
%%      if Q = ∅ then
%%          lcflag ← true; send msg (τ, lead-ch, L + 1, Qhat, Rhat)sign to each Pj
%%      else
%%          lcflag ← true; send msg (τ, lead-ch, L + 1, Qbar, Mbar)sign to each Pj
handle_msg(DKG=#dkg{leader_changing = false,
                    id = _Id,
                    shares_acked = SharesAcked,
                    shares_seen = SharesSeen,
                    ready_proofs = ReadyProofs,
                    echo_proofs = EchoProofs,
                    leader = CurrentLeader,
                    elections_allowed = true}, _Sender, timeout) ->
    NewDKG = DKG#dkg{leader_changing=true},

    Msg = case length(SharesAcked) == 0 of
              true ->
                  {send, [{multicast, {signed_leader_change, CurrentLeader+1, SharesSeen, {ready_proofs, ReadyProofs}}}]};
              false ->
                  {send, [{multicast, {signed_leader_change, CurrentLeader+1, SharesAcked, {echo_proofs, EchoProofs}}}]}
          end,
    {NewDKG, Msg};
handle_msg(DKG, _Sender, timeout) ->
    %% leader_changing is true
    {DKG, ok};

%% Leader-change for node Pi: session (τ ) and leader L
%%      upon a msg (τ, lead-ch, Lbar, Q, R/M)sign from Pj (first time):
%%      if Lbar > L and verify-signature(Q, R/M) then
%%          lcL ← lcL + 1
%%          Lnext ← min (Lnext , L)
%%          if R/M = R then
%%              Qhat ← Q; Rhat <- R
%%          else
%%              Qbar ← Q; Mbar ← M
%%      if (SUM(lcL) = t + f + 1 and lcflag = false) then
%%          if Qbar = ∅ then
%%              send the msg (τ, lead-ch, Lnext, Qhat, Rhat) to each Pj
%%          else
%%              send the msg (τ, lead-ch, Lnext, Qbar, Mbar) to each Pj
%%      else if (lcL = n − t − f ) then
%%          Mbar ← R <- n − t − f signed lead-ch messages for Lbar
%%          L ← Lbar; Lnext ← L − 1
%%          lcL ← 0; lcflag = false
%%          if Pi = L then
%%              if Q = ∅ then
%%                  send the message (L, τ, send, Qhat, Rhat) to each Pj
%%              else
%%                  send the message (L, τ, send, Qbar, Mbar) to each Pj
%%          else
%%              delay ← delay(T )
%%              start timer(delay)
handle_msg(DKG = #dkg{leader = Leader, t=T, n=N, f=F, leader_cap=LeaderCap, elections_allowed=true},
           Sender,
           {signed_leader_change, ProposedLeader, Shares, Proofs}=LeaderChangeMsg) when ProposedLeader > Leader ->
    %% TODO: verify the signature(Q, R/M)
    case store_leader_change(DKG, Sender, LeaderChangeMsg) of
        {true, NewDKG0} ->
            NewLeaderCap = min(LeaderCap, ProposedLeader),
            NewDKG = case Proofs of
                         {ready_proofs, ReadyProofs} ->
                             NewSharesSeen = Shares,
                             NewReadyProofs = ReadyProofs,
                             NewDKG0#dkg{shares_seen = NewSharesSeen, ready_proofs = NewReadyProofs};
                         {echo_proofs, EchoProofs} ->
                             NewDKG0#dkg{shares_acked = Shares, echo_proofs = EchoProofs}
                     end,
            case count_leader_change(ProposedLeader, NewDKG) == T+F+1 andalso not NewDKG#dkg.leader_changing of
                true ->
                    case length(NewDKG#dkg.shares_acked) == 0 of
                        true ->
                            {NewDKG, {send, [{multicast, {signed_leader_change, NewLeaderCap, NewDKG#dkg.shares_seen, {ready_proofs, NewDKG#dkg.ready_proofs}}}]}};
                        false ->
                            {NewDKG, {send, [{multicast, {signed_leader_change, NewLeaderCap, NewDKG#dkg.shares_acked, {echo_proofs, NewDKG#dkg.echo_proofs}}}]}}
                    end;
                false ->
                    LeaderChangeMsgs = maps:get(ProposedLeader, NewDKG#dkg.leader_vote_counts, []),
                    case length(LeaderChangeMsgs) == N-T-F of
                        true ->
                            NewerProofs = NewerReadyProofs = LeaderChangeMsgs,
                            NewLeader = ProposedLeader,
                            NewerLeaderCap = leader_cap(Leader, N),
                            NewLeaderChangeMap = maps:put(NewLeader, [], NewDKG#dkg.leader_vote_counts),
                            NewerDKG = NewDKG#dkg{leader_changing=false,
                                                  echo_proofs=NewerProofs,
                                                  ready_proofs=NewerReadyProofs,
                                                  leader=NewLeader,
                                                  leader_cap=NewerLeaderCap,
                                                  leader_vote_counts=NewLeaderChangeMap},
                            case DKG#dkg.id == NewLeader of
                                true ->
                                    case length(NewerDKG#dkg.shares_acked) == 0 of
                                        true ->
                                            %% choose a subset of T+1 shares we actually have here
                                            SharesProposal0 = maps:keys(NewerDKG#dkg.shares_results),
                                            {SharesProposal, _} = lists:split(T+1, SharesProposal0),
                                            {NewerDKG, {send, [{multicast, {send, SharesProposal, {ready_proofs, NewerDKG#dkg.ready_proofs}}}]}};
                                        false ->
                                            {NewerDKG, {send, [{multicast, {send, NewerDKG#dkg.shares_acked, {echo_proofs, NewerDKG#dkg.echo_proofs}}}]}}
                                    end;
                                false ->
                                    {NewerDKG, start_timer}
                            end;
                        false ->
                            {NewDKG, ok}
                    end
            end;
        false ->
            {DKG, ok}
    end;
handle_msg(DKG, _Sender, {signed_leader_change, _ProposedLeader, _, _}) ->
    {DKG, ignore};
handle_msg(DKG, _Sender, _Msg) ->
    {DKG, ignore}.

%% helper functions

output_ready(#dkg{shares_results = R0}, Shares) ->
    R = maps:with(Shares, R0),
    maps:size(R) == length(Shares).

output(DKG, Shares) ->
    OutputCommitment = output_commitment(DKG, Shares),
    PublicKeyShares = public_key_shares(DKG, OutputCommitment),
    VerificationKey = verification_key(OutputCommitment),
    Shard = shard(DKG, Shares),
    {Shard, VerificationKey, PublicKeyShares}.

-spec output_commitment(#dkg{}, node_set()) -> dkg_commitment:commitment().
output_commitment(_DKG=#dkg{shares_results=R0, u2=U2, t=T, n=N}, Shares) ->
    R = maps:with(Shares, R0),
    maps:fold(fun(_K, {Commitment, _}, Acc) ->
                      dkg_commitment:mul(Commitment, Acc)
              end, dkg_commitment:new(lists:seq(1, N), U2, T), R).

-spec public_key_shares(#dkg{}, dkg_commitment:commitment()) -> [erlang_pbc:element()].
public_key_shares(_DKG=#dkg{n=N}, OutputCommitment) ->
    [dkg_commitment:public_key_share(OutputCommitment, NodeID) || NodeID <- dkg_util:allnodes(N)].

-spec verification_key(dkg_commitment:commitment()) -> erlang_pbc:element().
verification_key(OutputCommitment) ->
    dkg_commitmentmatrix:lookup([1, 1], dkg_commitment:matrix(OutputCommitment)).

-spec shard(#dkg{}, node_set()) -> erlang_pbc:element().
shard(_DKG=#dkg{shares_results=R0, u=U}, Shares) ->
    R = maps:with(Shares, R0),
    Zero = erlang_pbc:element_set(erlang_pbc:element_new('Zr', U), 0),
    maps:fold(fun(_K, {_, Si}, Acc) ->
                        erlang_pbc:element_add(Acc, Si)
              end, Zero, R).

-spec leader_cap(pos_integer(), pos_integer()) -> pos_integer().
leader_cap(L, N) ->
    case L - 1 < 1 of
        true ->
            N;
        false ->
            L - 1
    end.

-spec count_echo(dkg(), node_set()) -> non_neg_integer().
count_echo(_DKG=#dkg{echo_counts=EchoCount, leader=Leader}, Shares0) ->
    Shares = lists:usort(Shares0),
    length(maps:get({Leader, Shares}, EchoCount, [])).

-spec get_echo(dkg(), node_set()) -> [echo()].
get_echo(_DKG=#dkg{echo_counts=EchoCount, leader=Leader}, Shares0) ->
    Shares = lists:usort(Shares0),
    maps:get({Leader, Shares}, EchoCount, []).

-spec update_echo_counts(dkg(), pos_integer(), signed_echo()) -> {true, dkg()} | false.
update_echo_counts(DKG=#dkg{echo_counts=EchoCount}, Sender, {signed_echo, Shares0}=EchoMsg) ->
    Shares = lists:usort(Shares0),
    EchoForQAndLeader = maps:get({DKG#dkg.leader, Shares}, EchoCount, []),
    case lists:keyfind(Sender, 1, EchoForQAndLeader) of
        false ->
            NewDKG = DKG#dkg{echo_counts=maps:put({DKG#dkg.leader, Shares}, [{Sender, EchoMsg} | EchoForQAndLeader], EchoCount)},
            {true, NewDKG};
        _ ->
            %% already have this echo
            false
    end.

-spec count_ready(dkg(), node_set()) -> non_neg_integer().
count_ready(_DKG=#dkg{ready_counts=ReadyCounts, leader=Leader}, Shares0) ->
    Shares = lists:usort(Shares0),
    length(maps:get({Leader, Shares}, ReadyCounts, [])).

-spec get_ready(dkg(), node_set()) -> [ready()].
get_ready(_DKG=#dkg{ready_counts=ReadyCounts, leader=Leader}, Shares0) ->
    Shares = lists:usort(Shares0),
    maps:get({Leader, Shares}, ReadyCounts, []).

-spec update_ready_counts(dkg(), pos_integer(), signed_ready()) -> {true, dkg()} | false.
update_ready_counts(DKG=#dkg{ready_counts=ReadyCounts}, Sender, {signed_ready, Shares0}=ReadyMsg) ->
    Shares = lists:usort(Shares0),
    ReadyForSharesAndLeader = maps:get({DKG#dkg.leader, Shares}, ReadyCounts, []),
    case lists:keyfind(Sender, 1, ReadyForSharesAndLeader) of
        false ->
            NewDKG = DKG#dkg{ready_counts=maps:put({DKG#dkg.leader, Shares}, [{Sender, ReadyMsg} | ReadyForSharesAndLeader], ReadyCounts)},
            {true, NewDKG};
        _ ->
            %% already have this echo
            false
    end.

-spec count_leader_change(pos_integer(), dkg()) -> non_neg_integer().
count_leader_change(NextLeader, DKG) ->
    L = maps:get(NextLeader, DKG#dkg.leader_vote_counts, []),
    length(L).

-spec store_leader_change(dkg(), pos_integer(), signed_leader_change()) -> {true, dkg()} | false.
store_leader_change(DKG, Sender, {signed_leader_change, ProposedLeader, _, _}=LeaderChangeMsg) ->
    L = maps:get(ProposedLeader, DKG#dkg.leader_vote_counts, []),
    case lists:keyfind(Sender, 1, L) of
        false ->
            NewLCM = maps:put(ProposedLeader, lists:keystore(Sender, 1, L, {Sender, LeaderChangeMsg}), DKG#dkg.leader_vote_counts),
            {true, DKG#dkg{leader_vote_counts=NewLCM}};
        _ ->
            false
    end.

-spec serialize(dkg()) -> serialized_dkg().
serialize(#dkg{id=Id,
               n=N,
               f=F,
               t=T,
               u=U,
               u2=U2,
               shares_map=SharesMap,
               shares_results=SharesResults,
               shares_acked=SharesAcked,
               shares_seen=SharesSeen,
               ready_proofs=ReadyProofs,
               echo_proofs=Echo_Proofs,
               echo_counts=EchoCount,
               ready_counts=ReadyCounts,
               leader_changing=LeaderChanging,
               leader=Leader,
               leader_cap=LeaderCap,
               leader_vote_counts=LeaderVoteCounts,
               await_vss=AwaitVSS}) ->
    #serialized_dkg{id=Id,
                    n=N,
                    f=F,
                    t=T,
                    u=erlang_pbc:element_to_binary(U),
                    u2=erlang_pbc:element_to_binary(U2),
                    shares_map=serialize_shares_map(SharesMap),
                    shares_results=serialize_shares_results(SharesResults),
                    shares_seen=SharesSeen,
                    shares_acked=SharesAcked,
                    ready_proofs=ReadyProofs,
                    echo_proofs=Echo_Proofs,
                    echo_counts=EchoCount,
                    ready_counts=ReadyCounts,
                    leader_changing=LeaderChanging,
                    leader=Leader,
                    leader_cap=LeaderCap,
                    leader_vote_counts=LeaderVoteCounts,
                    await_vss=AwaitVSS}.

-spec deserialize(serialized_dkg(), erlang_pbc:element()) -> dkg().
deserialize(#serialized_dkg{id=Id,
                            n=N,
                            f=F,
                            t=T,
                            u=SerializedU,
                            u2=SerializedU2,
                            shares_map=SerializedSharesMap,
                            shares_results=SerializedSharesResults,
                            shares_acked=SharesAcked,
                            shares_seen=SharesSeen,
                            ready_proofs=ReadyProofs,
                            echo_proofs=Echo_Proofs,
                            echo_counts=EchoCount,
                            ready_counts=ReadyCounts,
                            leader_changing=LeaderChanging,
                            leader=Leader,
                            leader_cap=LeaderCap,
                            %% XXX: Only one element is enough?
                            %% presumably we need to generate U and U2 again to deserialize? Not sure...
                            leader_vote_counts=LeaderVoteCounts,
                            await_vss=AwaitVSS}, Element) ->
    #dkg{id=Id,
         n=N,
         f=F,
         t=T,
         u=erlang_pbc:binary_to_element(Element, SerializedU),
         u2=erlang_pbc:binary_to_element(Element, SerializedU2),
         shares_map=deserialize_shares_map(SerializedSharesMap, Element),
         shares_results=deserialize_shares_results(SerializedSharesResults, Element),
         shares_acked=SharesAcked,
         shares_seen=SharesSeen,
         ready_proofs=ReadyProofs,
         echo_proofs=Echo_Proofs,
         echo_counts=EchoCount,
         ready_counts=ReadyCounts,
         leader_changing=LeaderChanging,
         leader=Leader,
         leader_cap=LeaderCap,
         leader_vote_counts=LeaderVoteCounts,
         await_vss=AwaitVSS}.

-spec serialize_shares_map(shares_map()) -> serialized_shares_map().
serialize_shares_map(SharesMap) ->
    maps:fold(fun(K, Shares, Acc) ->
                      maps:put(K, dkg_hybridvss:serialize(Shares), Acc)
              end, #{}, SharesMap).

-spec deserialize_shares_map(serialized_shares_map(), erlang_pbc:element()) -> shares_map().
deserialize_shares_map(SerializedSharesMap, Element) ->
    maps:fold(fun(K, Shares, Acc) ->
                      maps:put(K, dkg_hybridvss:deserialize(Shares, Element), Acc)
              end, #{}, SerializedSharesMap).

-spec serialize_shares_results(shares_results()) -> serialized_shares_results().
serialize_shares_results(SharesResults) ->
    maps:fold(fun(K, {C, Si}, Acc) ->
                      maps:put(K, {dkg_commitment:serialize(C), erlang_pbc:element_to_binary(Si)}, Acc)
              end, #{}, SharesResults).

-spec deserialize_shares_results(serialized_shares_results(), erlang_pbc:element()) -> shares_results().
deserialize_shares_results(SerializedSharesResults, U) ->
    maps:fold(fun(K, {C, Si}, Acc) ->
                      maps:put(K, {dkg_commitment:deserialize(C, U), erlang_pbc:binary_to_element(U, Si)}, Acc)
              end, #{}, SerializedSharesResults).

-spec status(dkg()) -> map().
status(DKG) ->
    #{id => DKG#dkg.id,
      shares_map => maps:map(fun(_K, Shares) -> dkg_hybridvss:status(Shares) end, DKG#dkg.shares_map),
      shares_results_from => maps:keys(DKG#dkg.shares_results),
      echoes_required => ceil(( DKG#dkg.n + DKG#dkg.t + 1 )/2),
      echoes_detail => maps:map(fun(_K, Echoes) ->
                                        lists:foldl(fun({Sender, {signed_echo, Shares}}, Acc) ->
                                                            [#{sender => Sender,
                                                               echo_counts => count_echo(DKG, Shares),
                                                               echo => lists:sort([X || {X, _} <- get_echo(DKG, Shares)])} | Acc]
                                                    end, [], Echoes)
                                end, DKG#dkg.echo_counts),
      readies_details => maps:map(fun(_K, Readies) ->
                                          lists:foldl(fun({Sender, {signed_ready, Shares}}, Acc) ->
                                                              [#{sender => Sender,
                                                                 ready_counts => count_ready(DKG, Shares),
                                                                 ready => lists:sort([X || {X, _} <- get_ready(DKG, Shares)])} | Acc]
                                                      end, [], Readies)
                                  end, DKG#dkg.ready_counts),
      readies_required => (DKG#dkg.t + 1),
      leader_changing => DKG#dkg.leader_changing,
      leader => DKG#dkg.leader,
      leader_cap => DKG#dkg.leader_cap}.
