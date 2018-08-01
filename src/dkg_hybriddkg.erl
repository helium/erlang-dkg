-module(dkg_hybriddkg).

-export([init/7, start/1]).

-export([handle_msg/3]).

-type session() :: {Leader :: pos_integer(), Round :: pos_integer()}.

-type lead_ch() :: any().
-type signed_echo() :: any().
-type signed_ready() :: any().
-type vss_ready() :: any().

-type leader_change() :: #{Leader :: pos_integer() => [{Sender :: pos_integer(), lead_ch()}]}.

-type elq() :: #{{Leader :: pos_integer(), Q :: [pos_integer()]} => [{Sender :: pos_integer(), SignedEcho :: signed_echo()}]}.
-type rlq() :: #{{Leader :: pos_integer(), Q :: [pos_integer()]} => [{Sender :: pos_integer(), SignedReady :: signed_ready()}]}.

-type rhat() :: [vss_ready()].
-type qbar() :: [pos_integer()].
-type qhat() :: [pos_integer()].
-type mbar() :: [signed_ready() | signed_echo()].

-record(dkg, {
          state = leader_unconfirmed :: leader_unconfirmed | functional | agreement_started | agreement_completed | leader_change_started | dkg_completed,
          id :: pos_integer(),
          n :: pos_integer(),
          f :: pos_integer(),
          t :: pos_integer(),
          u :: erlang_pbc:element(),
          u2 :: erlang_pbc:element(),
          vss_map :: #{pos_integer() => dkg_hybridvss:vss()},
          vss_results = #{} :: #{pos_integer() => {C :: dkg_commitment:commitment(), Si :: erlang_pbc:element()}},
          qbar = [] :: qbar(),
          qhat = [] :: qhat(),
          rhat = [] :: rhat(),
          mbar = [] :: mbar(),
          elq = #{} :: elq(),
          rlq = #{} :: rlq(),
          session :: session(),
          lc_flag = false :: boolean(),
          leader :: pos_integer(),
          l_next :: pos_integer(),
          leader_change = #{} :: leader_change()
         }).

%% upon initialization:
%%      e(L, Q) <- 0 and r(L, Q) <- 0 for every Q; Qbar <- ∅; Q <- ∅
%%      Mbar <- Rhat <- n-t-f signed lead-ch messages for leader L
%%      cnt ← 0; cntl ← 0 for all l ∈ [1, n];
%%      lcl ← 0 for each leader L; lcflag ← false
%%      Lnext ← L + n − 1
%%      for all d ∈ [1, n] do
%%          initialize extended-HybridVSS Sh protocol (Pd, τ )
init(Id, N, F, T, G1, G2, Round) ->
    erlang_pbc:element_pp_init(G1),
    erlang_pbc:element_pp_init(G2),
    Session = {1, Round},
    VSSes = lists:foldl(fun(E, Map) ->
                                VSS = dkg_hybridvss:init(Id, N, F, T, G1, G2, {E, Round}),
                                maps:put(E, VSS, Map)
                        end, #{}, dkg_util:allnodes(N)),

    #dkg{id=Id,
         n=N,
         f=F,
         t=T,
         u=G1,
         u2=G2,
         leader=1,
         l_next=l_next(1, N),
         session=Session,
         vss_map=VSSes}.

start(DKG = #dkg{id=Id, u=G1}) ->
    MyVSS = maps:get(Id, DKG#dkg.vss_map),
    Secret = erlang_pbc:element_random(erlang_pbc:element_new('Zr', G1)),
    {NewVSS, {send, ToSend}} = dkg_hybridvss:input(MyVSS, Secret),
    {DKG#dkg{vss_map=maps:put(Id, NewVSS, DKG#dkg.vss_map), state=functional}, {send, dkg_util:wrap({vss, Id, DKG#dkg.session}, ToSend)}}.


%% upon (Pd, τ, out, shared, Cd , si,d , Rd ) (first time):
%%      Qhat ← {Pd}; Rhat ← {Rd}
%%      if |Qhat| = t + 1 and Qbar = ∅ then
%%           if Pi = L then
%%               send the message (L, τ, send, Qhat, Rhat) to each Pj
%%            else
%%               delay ← delay(T); start timer(delay)
handle_msg(DKG = #dkg{session=Session={Leader, _}}, Sender, {{vss, VSSId, Session}, VssMSG}) ->
    case dkg_hybridvss:handle_msg(maps:get(VSSId, DKG#dkg.vss_map), Sender, VssMSG) of
        {NewVSS, ok} ->
            {DKG#dkg{vss_map=maps:put(VSSId, NewVSS, DKG#dkg.vss_map), state=agreement_started}, ok};
        {NewVSS, {send, ToSend}} ->
            {DKG#dkg{vss_map=maps:put(VSSId, NewVSS, DKG#dkg.vss_map), state=agreement_started}, {send, dkg_util:wrap({vss, VSSId, Session}, ToSend)}};
        {NewVSS, {result, {_Session, Commitment, Si, Rd}}} ->
            %% upon (Pd, τ, out, shared, Cd , si,d , Rd ) (first time):
            %%      Qhat ← {Pd}; Rhat ← {Rd}
            %%      if |Qhat| = t + 1 and Qbar = ∅ then
            %%           if Pi = L then
            %%               send the message (L, τ, send, Qhat, Rhat) to each Pj
            %%            else
            %%               delay ← delay(T); start timer(delay)
            %% TODO 'extended' VSS should output signed ready messages

            NewDKG = DKG#dkg{vss_map=maps:put(VSSId, NewVSS, DKG#dkg.vss_map),
                             vss_results=maps:put(VSSId, {Commitment, Si}, DKG#dkg.vss_results),
                             qhat=[VSSId | DKG#dkg.qhat],
                             rhat=[{signed_vss_ready, Rd} | DKG#dkg.rhat]
                            },

            case length(NewDKG#dkg.qhat) == NewDKG#dkg.t + 1 andalso length(NewDKG#dkg.qbar) == 0 of
                true ->
                    case NewDKG#dkg.id == Leader of
                        true ->
                            {NewDKG, {send, [{multicast, {send, Session, NewDKG#dkg.qhat, {rhat, NewDKG#dkg.rhat}}}]}};
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
handle_msg(DKG = #dkg{session=Session={_Leader, _}}, _Sender, {send, Session, Q, {rhat, _Rhat}}) ->
    %% TODO verify signatures
    case length(DKG#dkg.qbar) == 0 orelse lists:usort(DKG#dkg.qbar) == lists:usort(Q) of
        true ->
            {DKG, {send, [{multicast, {echo, Session, Q}}]}};
        false ->
            {DKG, ok}
    end;
handle_msg(DKG, _Sender, {send, _Session, _, _}) ->
    {DKG, ok};


%% upon a message (L, τ, echo, Q)sign from Pm (first time):
%%      e(L,Q) ← e(L,Q) + 1
%%      if e(L,Q) = ceil((n+t+1)/2) and r(L,Q) < t + 1 then
%%          Qbar ← Q; Mbar ← ceil((n+t+1)/2) signed echo messages for Q
%%          send the message (L, τ, ready, Q)sign to each Pj
handle_msg(DKG = #dkg{id=_Id, session=Session, n=N, t=T}, Sender, {echo, Session, Q}=EchoMsg) ->
    case update_elq(DKG, Sender, EchoMsg) of
        false ->
            {DKG, ok};
        {true, NewDKG} ->
            case count_echo(NewDKG, Q) == ceil((N+T+1)/2) andalso count_ready(NewDKG, Q) < T+1 of
                true ->
                    %% update qbar
                    NewQbar = Q,
                    %% update mbar
                    NewMbar = get_echo(NewDKG, Q),
                    %% send ready message
                    {NewDKG#dkg{qbar=NewQbar, mbar=NewMbar}, {send, [{multicast, {ready, Session, Q}}]}};
                false ->
                    {NewDKG, ok}
            end
    end;
handle_msg(DKG, _Sender, {echo, _Session, _}=_EchoMsg) ->
    {DKG, ok};

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
handle_msg(DKG = #dkg{id=_Id, n=N, t=T, f=F}, Sender, {ready, Session, Q}=ReadyMsg) ->
    case update_rlq(DKG, Sender, ReadyMsg) of
        false ->
            {DKG, ok};
        {true, NewDKG} ->
            case count_ready(NewDKG, Q) == T+1 andalso count_echo(NewDKG, Q) < ceil((N+T+1)/2) of
                true ->
                    %% update qbar
                    NewQbar = Q,
                    %% update mbar
                    NewMbar = get_ready(NewDKG, Q),
                    %% send ready msg
                    {NewDKG#dkg{qbar=NewQbar, mbar=NewMbar}, {send, [{multicast, {ready, Session, Q}}]}};
                false ->
                    case count_ready(NewDKG, Q) == N-T-F of
                        true ->
                            %% TODO stop timer
                            %% TODO presumably we need to check this when we get a VSS result as well?
                            OutputCommitment = output_commitment(NewDKG),
                            PublicKeyShares = public_key_shares(NewDKG, OutputCommitment),
                            VerificationKey = verification_key(OutputCommitment),
                            Shard = shard(NewDKG),
                            {NewDKG#dkg{state=dkg_completed}, {result, {Shard, VerificationKey, PublicKeyShares}}};
                        false ->
                            {NewDKG#dkg{state=agreement_started}, ok}
                    end
            end
    end;
handle_msg(DKG, _Sender, {ready, _Session, _VSSDone}=_ReadyMsg) ->
    %% DKG received ready message from itself, what to do?
    {DKG, ok};

%% upon timeout:
%% if lcflag = false then
%%      if Q = ∅ then
%%          lcflag ← true; send msg (τ, lead-ch, L + 1, Qhat, Rhat)sign to each Pj
%%      else
%%          lcflag ← true; send msg (τ, lead-ch, L + 1, Qbar, Mbar)sign to each Pj
handle_msg(DKG=#dkg{lc_flag=false,
                    id=_Id,
                    qbar=Qbar,
                    qhat=Qhat,
                    rhat=Rhat,
                    mbar=Mbar,
                    session={CurrentLeader, Round}}, _Sender, timeout) ->
    NewDKG = DKG#dkg{lc_flag=true},

    Msg = case length(Qbar) == 0 of
              true ->
                  {send, [{multicast, {leader_change, {CurrentLeader+1, Round}, Qhat, {rhat, Rhat}}}]};
              false ->
                  {send, [{multicast, {leader_change, {CurrentLeader+1, Round}, Qbar, {mbar, Mbar}}}]}
          end,
    {NewDKG, Msg};
handle_msg(DKG, _Sender, timeout) ->
    %% lc_flag is true
    {DKG, ok};

%% TODO
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
handle_msg(DKG=#dkg{leader=Leader, t=T, n=N, f=F, qhat=Qhat0, rhat=Rhat0, l_next=LNext},
           Sender,
           {leader_change, {Lbar, Round}, Q, RorM}=LeaderChangeMsg) when Lbar > Leader ->
    %% TODO: verify the signature(Q, R/M)
    case store_leader_change(DKG, Sender, LeaderChangeMsg) of
        {true, NewDKG0} ->
            %% new lnext
            NewLNext = min(LNext, Lbar),
            NewDKG = case RorM of
                         {rhat, Rhat} ->
                             %% FIXME: do this better
                             NewQhat = lists:usort(Qhat0 ++ Q),
                             NewRhat = lists:usort(Rhat0 ++ Rhat),
                             NewDKG0#dkg{qhat=NewQhat, rhat=NewRhat};
                         {mbar, Mbar} ->
                             NewDKG0#dkg{qbar=Q, mbar=Mbar}
                     end,
            case count_leader_change(NewDKG) == T+F+1 andalso not NewDKG#dkg.lc_flag of
                true ->
                    case length(NewDKG#dkg.qbar) == 0 of
                        true ->
                            {send, [{multicast, {leader_change, {NewLNext, Round}, NewDKG#dkg.qhat, {rhat, NewDKG#dkg.rhat}}}]};
                        false ->
                            {send, [{multicast, {leader_change, {NewLNext, Round}, NewDKG#dkg.qbar, {mbar, NewDKG#dkg.mbar}}}]}
                    end;
                false ->
                    LeaderChangeMsgs = maps:get(Lbar, NewDKG#dkg.leader_change, []),
                    case length(LeaderChangeMsgs) == N-T-F of
                        true ->
                            NewerMbar = NewerRhat = LeaderChangeMsgs,
                            NewLeader = Lbar,
                            NewerLNext = l_next(Leader, N),
                            NewLeaderChangeMap = maps:put(NewLeader, [], NewDKG#dkg.leader_change),
                            NewSession = {NewLeader, Round},
                            NewerDKG = NewDKG#dkg{lc_flag=false,
                                                  mbar=NewerMbar,
                                                  rhat=NewerRhat,
                                                  leader=NewLeader,
                                                  l_next=NewerLNext,
                                                  leader_change=NewLeaderChangeMap,
                                                  session=NewSession},
                            case DKG#dkg.id == NewLeader of
                                true ->
                                    case length(NewerDKG#dkg.qbar) == 0 of
                                        true ->
                                            {NewerDKG, {send, [{multicast, {send, NewSession, NewerDKG#dkg.qhat, {rhat, NewerDKG#dkg.rhat}}}]}};
                                        false ->
                                            {NewerDKG, {send, [{multicast, {send, NewSession, NewerDKG#dkg.qbar, {mbar, NewerDKG#dkg.mbar}}}]}}
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
handle_msg(DKG, _Sender, {leader_change, {_Lbar, _Round}, _, _}) ->
    {DKG, ok};
handle_msg(DKG, _Sender, Msg) ->
    {DKG, {unhandled_msg, Msg}}.

%% helper functions
-spec output_commitment(#dkg{}) -> dkg_commitment:commitment().
output_commitment(_DKG=#dkg{vss_results=R, u2=U2, t=T, n=N}) ->
    maps:fold(fun(_K, {Commitment, _}, Acc) ->
                      dkg_commitment:mul(Commitment, Acc)
              end, dkg_commitment:new(lists:seq(1, N), U2, T), R).

-spec public_key_shares(#dkg{}, dkg_commitment:commitment()) -> [erlang_pbc:element()].
public_key_shares(_DKG=#dkg{n=N}, OutputCommitment) ->
    [dkg_commitment:public_key_share(OutputCommitment, NodeID) || NodeID <- dkg_util:allnodes(N)].

-spec verification_key(dkg_commitment:commitment()) -> erlang_pbc:element().
verification_key(OutputCommitment) ->
    dkg_commitmentmatrix:lookup([1, 1], dkg_commitment:matrix(OutputCommitment)).

-spec shard(#dkg{}) -> erlang_pbc:element().
shard(_DKG=#dkg{vss_results=R, u=U}) ->
    Zero = erlang_pbc:element_set(erlang_pbc:element_new('Zr', U), 0),
    maps:fold(fun(_K, {_, Si}, Acc) ->
                        erlang_pbc:element_add(Acc, Si)
              end, Zero, R).

l_next(L, N) ->
    case L - 1 < 1 of
        true ->
            N;
        false ->
            L - 1
    end.

update_elq(DKG=#dkg{elq=Elq}, Sender, {echo, _Session, Q0}=EchoMsg) ->
    Q = lists:usort(Q0),
    EchoForQAndLeader = maps:get({DKG#dkg.leader, Q}, Elq, []),
    case lists:keyfind(Sender, 1, EchoForQAndLeader) of
        false ->
            NewDKG = DKG#dkg{elq=maps:put({DKG#dkg.leader, Q}, [{Sender, EchoMsg} | EchoForQAndLeader], Elq)},
            {true, NewDKG};
        _ ->
            %% already have this echo
            false
    end.

count_echo(_DKG=#dkg{elq=Elq, leader=Leader}, Q0) ->
    Q = lists:usort(Q0),
    length(maps:get({Leader, Q}, Elq, [])).
count_ready(_DKG=#dkg{rlq=Rlq, leader=Leader}, Q0) ->
    Q = lists:usort(Q0),
    length(maps:get({Leader, Q}, Rlq, [])).

get_echo(_DKG=#dkg{elq=Elq, leader=Leader}, Q) -> maps:get({Leader, Q}, Elq, []).
get_ready(_DKG=#dkg{rlq=Rlq, leader=Leader}, Q) -> maps:get({Leader, Q}, Rlq, []).

update_rlq(DKG=#dkg{rlq=Rlq}, Sender, {ready, _Session, Q0}=ReadyMsg) ->
    Q = lists:usort(Q0),
    ReadyForQAndLeader = maps:get({DKG#dkg.leader, Q}, Rlq, []),
    case lists:keyfind(Sender, 1, ReadyForQAndLeader) of
        false ->
            NewDKG = DKG#dkg{rlq=maps:put({DKG#dkg.leader, Q}, [{Sender, ReadyMsg} | ReadyForQAndLeader], Rlq)},
            {true, NewDKG};
        _ ->
            %% already have this echo
            false
    end.

store_leader_change(DKG, Sender, {leader_change, {Lbar, _Round}, _, _}=LeaderChangeMsg) ->
    L = maps:get(Lbar, DKG#dkg.leader_change, []),
    case lists:keyfind(Sender, 1, L) of
        false ->
            NewLCM = maps:put(Lbar, lists:keystore(Sender, 1, L, {Sender, LeaderChangeMsg}), DKG#dkg.leader_change),
            {true, DKG#dkg{leader_change=NewLCM}};
        _ ->
            false
    end.

count_leader_change(DKG) -> length(lists:flatten(maps:keys(DKG#dkg.leader_change))).
