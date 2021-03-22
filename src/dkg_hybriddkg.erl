-module(dkg_hybriddkg).

-export([init/6,
         input/2, % for testing
         start/1,
         serialize/1,
         deserialize/3,
         deserialize/4,
         status/1,
         default_commitment_cache_fun/1
        ]).

-export([handle_msg/3]).

-record(dkg, {
          id :: pos_integer(),
          n :: pos_integer(),
          f :: pos_integer(),
          t :: pos_integer(),
          shares_map = #{} :: shares_map(),
          shares_results = #{} :: shares_results(),
          shares_seen = [] :: node_set(),
          shares_acked = [] :: node_set(),
          echo_proofs = [] :: echo_proofs(),
          vss_ready_proofs = [] :: vss_ready_proofs(),
          echo_counts = #{} :: echo_counts(),
          ready_counts = #{} :: ready_counts(),
          leader :: pos_integer(),
          leader_cap :: pos_integer(),
          leader_changing = false :: boolean(),
          leader_vote_counts = #{} :: leader_vote_counts(),
          await_vss = false :: boolean(),
          elections_allowed = false :: boolean(),
          commitment_cache_fun :: fun()
         }).

-type node_set() :: [pos_integer()].
-type echo_proofs() :: [signed_ready() | signed_echo()].
-type signed_leader_change() :: {signed_leader_change, pos_integer(), vss_ready_proofs()} | {signed_leader_change, pos_integer(), node_set(), echo_proofs()}.
-type signed_echo() :: {signed_echo, node_set()}.
-type signed_ready() :: {signed_ready, node_set()}.
-type vss_ready_proofs() :: [{VSSId :: pos_integer(), dkg_commitment:ready_proofs()}].
-type identity() :: {Leader :: pos_integer(), Q :: [pos_integer()]}.
-type echo() :: {Sender :: pos_integer(), SignedEcho :: signed_echo()}.
-type ready() :: {Sender :: pos_integer(), SignedReady :: signed_ready()}.
-type echo_counts() :: #{identity() => [echo()]}.
-type ready_counts() :: #{identity() => [ready()]}.
-type leader_vote_counts() :: #{Leader :: pos_integer() => [{Sender :: pos_integer(), signed_leader_change()}]}.
-type shares_map() :: #{pos_integer() => dkg_hybridvss:vss()}.
-type serialized_shares_map() :: #{pos_integer() => #{atom() => binary() | map()}}.
-type shares_results() :: #{pos_integer() => {C :: dkg_commitment:commitment(), Si :: tc_fr:fr()}}.
-type dkg() :: #dkg{}.

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
-spec init(Id :: pos_integer(), N :: pos_integer(), F :: pos_integer(), T :: non_neg_integer(),
           Round :: binary(), Options :: [{atom(), term()} | atom]) ->
    #dkg{}.
init(Id, N, F, T, Round, Options) ->
    true = N >= (3*T + 2*F + 1),
    Callback = proplists:get_value(callback, Options, false),
    %% XXX Don't allow elections by default. We don't have signed proofs they should
    %% occur so it's not safe to enable this in a non development context.
    Elections = proplists:get_value(elections, Options, false),
    %% these will cause an exception if not present in the options list
    {signfun, SignFun} = lists:keyfind(signfun, 1, Options),
    {verifyfun, VerifyFun} = lists:keyfind(verifyfun, 1, Options),
    CCacheFun = proplists:get_value(commitment_cache_fun, Options, fun default_commitment_cache_fun/1),
    Shares = lists:foldl(fun(E, Map) ->
                                 Share = dkg_hybridvss:init(Id, N, F, T, {E, Round}, Callback, SignFun, VerifyFun, CCacheFun),
                                 Map#{E => Share}
                         end, #{}, dkg_util:allnodes(N)),

    #dkg{id = Id,
         n = N,
         f = F,
         t = T,
         leader = 1,
         leader_cap = leader_cap(1, N),
         shares_map = Shares,
         elections_allowed = Elections,
         commitment_cache_fun=CCacheFun
        }.

%% the dgk starts to go on its own, this is here for the sake of
%% fakecast for now, since all the other protocols take input.
input(State, _) ->
    start(State).

start(DKG = #dkg{id=Id}) ->
    #{Id := MyShares} = SharesMap = DKG#dkg.shares_map,
    Secret = rand:uniform(trunc(math:pow(2, 64))),
    {NewShares, {send, ToSend}} = dkg_hybridvss:input(MyShares, Secret),
    {DKG#dkg{shares_map = SharesMap#{Id => NewShares}},
     {send, dkg_util:wrap({vss, Id}, ToSend)}}.

handle_msg(DKG=#dkg{await_vss = true}, Sender, {{vss, SharesId}, SharesMsg}) ->
    case is_chosen_vss(SharesId, DKG) of
        true ->
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
                                     shares_results = maps:put(SharesId, {dkg_commitment:serialize(Commitment), tc_fr:serialize(Si)}, DKG#dkg.shares_results),
                                     shares_seen = [SharesId | DKG#dkg.shares_seen]
                                    },
                    case output_ready(NewDKG, NewDKG#dkg.shares_acked) of
                        true ->
                            KeyShare = output(NewDKG, NewDKG#dkg.shares_acked),
                            {NewDKG, {result, KeyShare}};
                        false ->
                            {NewDKG, ok}
                    end
            end;
        false ->
            {DKG, ignore}
    end;
handle_msg(DKG=#dkg{leader = Leader}, Sender, {{vss, SharesId}, SharesMsg}) ->
    case is_chosen_vss(SharesId, DKG) of
        true ->
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

                    NewDKG = DKG#dkg{shares_map = maps:put(SharesId, NewShares, DKG#dkg.shares_map),
                                     shares_results = maps:put(SharesId, {dkg_commitment:serialize(Commitment), tc_fr:serialize(Si)}, DKG#dkg.shares_results),
                                     shares_seen = [SharesId | DKG#dkg.shares_seen]
                                    },
                    case length(NewDKG#dkg.shares_seen) == NewDKG#dkg.t + 1 andalso length(NewDKG#dkg.shares_acked) == 0 of
                        true ->
                            case NewDKG#dkg.id == Leader of
                                true ->
                                    {NewDKG, {send, [{multicast, {send, {vss_ready_proofs,
                                                                         [ {SId, dkg_commitment:ready_proofs(C)} || {SId, {C, _}} <- maps:to_list(NewDKG#dkg.shares_results)]
                                                                        }
                                                                 }
                                                     }
                                                    ]
                                             }};
                                false ->
                                    {NewDKG, start_timer}
                            end;
                        false ->
                            {NewDKG, ok}
                    end
            end;
        false ->
            {DKG, ignore}
    end;

%% upon a message (L, τ, send, Q, R/M) from L (first time):
%%      if verify-signature(Q, R/M) and (Qbar = ∅  or Qbar = Q) then
%%          send the message (L, τ, echo, Q)sign to each Pj
handle_msg(DKG = #dkg{leader=Leader}, Leader, {send, Proofs}) ->
    case verify_proofs(Proofs, DKG) of
        {true, SharesSent} ->
            case length(DKG#dkg.shares_acked) == 0 orelse
                 lists:usort(DKG#dkg.shares_acked) == lists:usort(SharesSent) of
                true ->
                    {DKG, {send, [{multicast, {signed_echo, SharesSent}}]}};
                false ->
                    {DKG, ok}
            end;
        false ->
            %% got a bad proof!
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
                                    KeyShare = output(NewDKG, Shares),
                                    {NewDKG, {result, KeyShare}};
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
                    echo_proofs = EchoProofs,
                    vss_ready_proofs = ReadyProofs,
                    leader = CurrentLeader,
                    elections_allowed = true}, _Sender, timeout) ->
    NewDKG = DKG#dkg{leader_changing=true},

    Msg = case length(SharesAcked) == 0 of
              true ->
                  {send, [{multicast, {signed_leader_change, CurrentLeader+1, {vss_ready_proofs, ReadyProofs}}}]};
              false ->
                  {send, [{multicast, {signed_leader_change, CurrentLeader+1, {echo_proofs, SharesAcked, EchoProofs}}}]}
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
           {signed_leader_change, ProposedLeader, Proofs}=LeaderChangeMsg) when ProposedLeader > Leader ->
    %% TODO: verify the signature(Q, R/M)
    case store_leader_change(DKG, Sender, LeaderChangeMsg) of
        {true, NewDKG0} ->
            NewLeaderCap = min(LeaderCap, ProposedLeader),
            NewDKG = case Proofs of
                         {vss_ready_proofs, ReadyProofs} ->
                             {NewSharesSeen, _} = lists:unzip(ReadyProofs),
                             NewReadyProofs = ReadyProofs,
                             NewDKG0#dkg{shares_seen = NewSharesSeen, vss_ready_proofs = NewReadyProofs};
                         {echo_proofs, Shares, EchoProofs} ->
                             NewDKG0#dkg{shares_acked = Shares, echo_proofs = EchoProofs}
                     end,
            case count_leader_change(ProposedLeader, NewDKG) == T+F+1 andalso not NewDKG#dkg.leader_changing of
                true ->
                    case length(NewDKG#dkg.shares_acked) == 0 of
                        true ->
                            {NewDKG, {send, [{multicast, {signed_leader_change, NewLeaderCap, {vss_ready_proofs, NewDKG#dkg.vss_ready_proofs}}}]}};
                        false ->
                            {NewDKG, {send, [{multicast, {signed_leader_change, NewLeaderCap, {echo_proofs, NewDKG#dkg.shares_acked, NewDKG#dkg.echo_proofs}}}]}}
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
                                                  vss_ready_proofs=NewerReadyProofs,
                                                  leader=NewLeader,
                                                  leader_cap=NewerLeaderCap,
                                                  leader_vote_counts=NewLeaderChangeMap},
                            case DKG#dkg.id == NewLeader of
                                true ->
                                    case length(NewerDKG#dkg.shares_acked) == 0 of
                                        true ->
                                            %% choose a subset of T+1 shares we actually have here
                                            SharesProposal0 = maps:to_list(NewerDKG#dkg.shares_results),
                                            {SharesProposal, _} = lists:split(T+1, SharesProposal0),
                                            ProposedReadyProofs = [ {NID, dkg_commitment:ready_proofs(C)} || {NID, {C, _}} <- SharesProposal ],
                                            {NewerDKG, {send, [{multicast, {send, {vss_ready_proofs, ProposedReadyProofs}}}]}};
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
    PublicKeySet = output_public_key_set(DKG, Shares),
    Shard = shard(DKG, Shares),
    tc_key_share:new(DKG#dkg.id - 1, PublicKeySet, Shard).

-spec output_public_key_set(#dkg{}, node_set()) -> tc_public_key_set:pk_set().
output_public_key_set(DKG=#dkg{shares_results=R0}, Shares) ->
    {[Head|Commitments], _Shares} = lists:unzip(maps:values(maps:with(Shares, R0))),
    lists:foldl(fun(Commitment, Acc) ->
                        tc_public_key_set:combine(Acc, dkg_commitment:public_key_set(dkg_commitment:deserialize(Commitment, DKG#dkg.commitment_cache_fun)))
              end, dkg_commitment:public_key_set(dkg_commitment:deserialize(Head, DKG#dkg.commitment_cache_fun)), Commitments).

-spec shard(#dkg{}, node_set()) -> tc_secret_key_share:sk_share().
shard(_DKG=#dkg{shares_results=R0}, Shares) ->
    {_Commitments, [Head|Keys]} = lists:unzip(maps:values(maps:with(Shares, R0))),
    lists:foldl(fun(Si, Acc) ->
                        tc_secret_key_share:combine(Acc, tc_secret_key_share:from_fr(tc_fr:deserialize(Si)))
              end, tc_secret_key_share:from_fr(tc_fr:deserialize(Head)), Keys).

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
store_leader_change(DKG, Sender, {signed_leader_change, ProposedLeader, _Proof}=LeaderChangeMsg) ->
    %% TODO verify the proof here
    L = maps:get(ProposedLeader, DKG#dkg.leader_vote_counts, []),
    case lists:keyfind(Sender, 1, L) of
        false ->
            NewLCM = maps:put(ProposedLeader, lists:keystore(Sender, 1, L, {Sender, LeaderChangeMsg}), DKG#dkg.leader_vote_counts),
            {true, DKG#dkg{leader_vote_counts=NewLCM}};
        _ ->
            false
    end.

verify_proofs({vss_ready_proofs, Proofs}, DKG = #dkg{n=N, t=T, f=F}) ->
    Res = lists:all(fun({VSSId, Proof}) ->
                            %% get the VSS state for this ID
                            VSS = maps:get(VSSId, DKG#dkg.shares_map),
                            %% check we have enough proofs
                            maps:size(Proof) == (N - T - F) andalso
                            %% check that every ready proof is valid for this VSS
                            lists:all(fun({Sender, ReadyProof}) ->
                                                        dkg_hybridvss:verify_proof(VSS, Sender, ReadyProof)
                                                end, maps:to_list(Proof))
                    end, Proofs),
    case Res of
        true ->
            {Shares, _} = lists:unzip(Proofs),
            {true, Shares};
        false ->
            false
    end.

-spec serialize(dkg()) -> #{atom() => map() | binary()}.
serialize(#dkg{id = Id,
               n = N,
               f = F,
               t = T,
               shares_map = SharesMap,
               shares_results = SharesResults,
               shares_acked = SharesAcked,
               shares_seen = SharesSeen,
               vss_ready_proofs = ReadyProofs,
               echo_proofs = Echo_Proofs,
               echo_counts = EchoCount,
               ready_counts = ReadyCounts,
               leader_changing = LeaderChanging,
               leader = Leader,
               leader_cap = LeaderCap,
               leader_vote_counts = LeaderVoteCounts,
               await_vss = AwaitVSS}) ->
    PreSer = #{shares_map => serialize_shares_map(SharesMap)},
    M0 = #{id => Id,
           n => N,
           f => F,
           t => T,
           shares_seen => SharesSeen,
           shares_results => SharesResults,
           shares_acked => SharesAcked,
           vss_ready_proofs => ReadyProofs,
           echo_proofs => Echo_Proofs,
           echo_counts => EchoCount,
           ready_counts => ReadyCounts,
           leader_changing => LeaderChanging,
           leader => Leader,
           leader_cap => LeaderCap,
           leader_vote_counts => LeaderVoteCounts,
           await_vss => AwaitVSS},
    M = maps:map(fun(_K, Term) -> term_to_binary(Term) end, M0),
    maps:merge(PreSer, M).

-spec deserialize(#{}, fun(), fun()) -> dkg().
deserialize(Map0, SignFun, VerifyFun) when is_map(Map0) ->
    deserialize(Map0, SignFun, VerifyFun, fun default_commitment_cache_fun/1).

-spec deserialize(#{}, fun(), fun(), fun()) -> dkg().
deserialize(Map0, SignFun, VerifyFun, CCacheFun) when is_map(Map0) ->
    Map = maps:map(fun(K, V) when K == shares_map ->
                           V;
                      (_K, B) ->
                           binary_to_term(B)
                   end, Map0),
        #{id := Id,
          n := N,
          f := F,
          t := T,
          shares_map := SerializedSharesMap,
          shares_results := SharesResults,
          shares_acked := SharesAcked,
          shares_seen := SharesSeen,
          vss_ready_proofs := ReadyProofs,
          echo_proofs := Echo_Proofs,
          echo_counts := EchoCount,
          ready_counts := ReadyCounts,
          leader_changing := LeaderChanging,
          leader := Leader,
          leader_cap := LeaderCap,
          %% XXX: Only one element is enough?
          %% presumably we need to generate U and U2 again to deserialize? Not sure...
          leader_vote_counts := LeaderVoteCounts,
          await_vss := AwaitVSS} = Map,
    #dkg{id = Id,
         n = N,
         f = F,
         t = T,
         shares_map = deserialize_shares_map(SerializedSharesMap, SignFun, VerifyFun, CCacheFun),
         shares_results = SharesResults,
         shares_acked = SharesAcked,
         shares_seen = SharesSeen,
         vss_ready_proofs = ReadyProofs,
         echo_proofs = Echo_Proofs,
         echo_counts = EchoCount,
         ready_counts = ReadyCounts,
         leader_changing = LeaderChanging,
         leader = Leader,
         leader_cap = LeaderCap,
         leader_vote_counts = LeaderVoteCounts,
         await_vss = AwaitVSS,
         commitment_cache_fun=CCacheFun}.

-spec serialize_shares_map(shares_map()) -> serialized_shares_map().
serialize_shares_map(SharesMap) ->
    maps:fold(fun(K, Shares, Acc) ->
                      Name = shares_name(K),
                      maps:put(Name, dkg_hybridvss:serialize(Shares), Acc)
              end, #{}, SharesMap).

-spec deserialize_shares_map(serialized_shares_map(), fun(), fun(), fun()) -> shares_map().
deserialize_shares_map(SerializedSharesMap, SignFun, VerifyFun, CCacheFun) ->
    maps:fold(fun(K, Shares, Acc) ->
                      Name = shares_name(K),
                      maps:put(Name, dkg_hybridvss:deserialize(Shares, SignFun, VerifyFun, CCacheFun), Acc)
              end, #{}, SerializedSharesMap).

shares_name(N) when is_integer(N) ->
    list_to_atom("share_" ++ integer_to_list(N));
shares_name(Key) when is_atom(Key) ->
    L = atom_to_list(Key),
    "share_" ++ Int = L,
    list_to_integer(Int).

is_chosen_vss(_, #dkg{elections_allowed=true}) ->
    %% never optimize these VSSes away if we do elections
    true;
is_chosen_vss(VSSID, DKG) ->
    case maps:size(DKG#dkg.echo_counts) + maps:size(DKG#dkg.ready_counts) of
        0 ->
            %% no echoes/readies yet, so assume we need it
            true;
        _ ->
            %% check if any echo or ready mentions this VSS ID
            lists:any(fun({_Leader, SelectedShares}) -> lists:member(VSSID, SelectedShares) end, maps:keys(DKG#dkg.echo_counts) ++ maps:keys(DKG#dkg.ready_counts))
    end.

-spec status(dkg()) -> map().
status(DKG) ->
    #{id => DKG#dkg.id,
      shares_map => maps:map(fun(_K, Shares) -> dkg_hybridvss:status(Shares) end, DKG#dkg.shares_map),
      missing_shares_results_from => lists:seq(1, DKG#dkg.n) -- maps:keys(DKG#dkg.shares_results),
      echoes_required => ceil(( DKG#dkg.n + DKG#dkg.t + 1 )/2),
      echoes_details => maps:map(fun({_Leader, SelectedShares}, Echoes) ->
                                          {_Sender, {signed_echo, Shares}} = hd(Echoes),
                                          M = #{ echo_counts => count_echo(DKG, Shares),
                                             shares =>
                                             lists:foldl(fun(K, Acc) ->
                                                                 maps:put(K, #{
                                                                               acked => lists:member(K, DKG#dkg.shares_acked),
                                                                               has_share => maps:is_key(K, DKG#dkg.shares_results)
                                                                              }, Acc)
                                                         end, #{}, SelectedShares)
                                           },
                                          case count_echo(DKG, Shares) >= ceil(( DKG#dkg.n + DKG#dkg.t + 1 )/2) of
                                              true ->
                                                  M;
                                              false ->
                                                  maps:put(missing_echoes, lists:seq(1, DKG#dkg.n) -- lists:sort([X || {X, _} <- get_echo(DKG, Shares)]), M)
                                          end
                                  end, DKG#dkg.echo_counts),
      readies_details => maps:map(fun({_Leader, SelectedShares}, Readies) ->
                                          {_Sender, {signed_ready, Shares}} = hd(Readies),
                                          M = #{ ready_counts => count_ready(DKG, Shares),
                                             missing_readies => lists:seq(1, DKG#dkg.n) -- lists:sort([X || {X, _} <- get_ready(DKG, Shares)]),
                                             shares =>
                                             lists:foldl(fun(K, Acc) ->
                                                                 maps:put(K, #{
                                                                               acked => lists:member(K, DKG#dkg.shares_acked),
                                                                               has_share => maps:is_key(K, DKG#dkg.shares_results)
                                                                              }, Acc)
                                                         end, #{}, SelectedShares)
                                           },
                                          case count_ready(DKG, Shares) >= (DKG#dkg.t + 1 ) of
                                              true ->
                                                  M;
                                              false ->
                                                  maps:put(missing_readies, lists:seq(1, DKG#dkg.n) -- lists:sort([X || {X, _} <- get_ready(DKG, Shares)]), M)
                                          end

                                  end, DKG#dkg.ready_counts),
      readies_required => (DKG#dkg.t + 1),
      leader_changing => DKG#dkg.leader_changing,
      leader => DKG#dkg.leader,
      leader_cap => DKG#dkg.leader_cap}.

default_commitment_cache_fun({_Ser, _DeSer}) -> ok;
default_commitment_cache_fun(Ser) -> tc_bicommitment:deserialize(Ser).
