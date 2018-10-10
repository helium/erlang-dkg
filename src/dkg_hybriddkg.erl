-module(dkg_hybriddkg).

-export([init/7,
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
          vss_map = #{} :: vss_map(),
          vss_results = #{} :: vss_results(),
          qbar = [] :: qset(),
          qhat = [] :: qset(),
          rhat = [] :: rhat(),
          mbar = [] :: mbar(),
          elq = #{} :: elq(),
          rlq = #{} :: rlq(),
          lc_flag = false :: boolean(),
          leader :: pos_integer(),
          l_next :: pos_integer(),
          lc_map = #{} :: lc_map()
         }).

-record(serialized_dkg, {
          id :: pos_integer(),
          n :: pos_integer(),
          f :: pos_integer(),
          t :: pos_integer(),
          u :: binary(),
          u2 :: binary(),
          vss_map :: #{pos_integer() => dkg_hybridvss:serialized_vss()},
          vss_results = #{} :: serialized_vss_results(),
          qbar = [] :: qset(),
          qhat = [] :: qset(),
          rhat = [] :: rhat(),
          mbar = [] :: mbar(),
          elq = #{} :: elq(),
          rlq = #{} :: rlq(),
          lc_flag = false :: boolean(),
          leader :: pos_integer(),
          l_next :: pos_integer(),
          lc_map = #{} :: lc_map()
         }).

-type rhat() :: [vss_ready()].
-type qset() :: [pos_integer()].
-type mbar() :: [signed_ready() | signed_echo()].
-type signed_leader_change() :: {signed_leader_change, pos_integer(), qset(), rhat() | mbar()}.
-type signed_echo() :: {signed_echo, qset()}.
-type signed_ready() :: {signed_ready, qset()}.
-type vss_ready() :: {signed_vss_ready, dkg_hybridvss:readies()}.
-type identity() :: {Leader :: pos_integer(), Q :: [pos_integer()]}.
-type echo() :: {Sender :: pos_integer(), SignedEcho :: signed_echo()}.
-type ready() :: {Sender :: pos_integer(), SignedReady :: signed_ready()}.
-type elq() :: #{identity() => [echo()]}.
-type rlq() :: #{identity() => [ready()]}.
-type lc_map() :: #{Leader :: pos_integer() => [{Sender :: pos_integer(), signed_leader_change()}]}.
-type vss_map() :: #{pos_integer() => dkg_hybridvss:vss()}.
-type serialized_vss_map() :: #{pos_integer() => dkg_hybridvss:serialized_vss()}.
-type vss_results() :: #{pos_integer() => {C :: dkg_commitment:commitment(), Si :: erlang_pbc:element()}}.
-type serialized_vss_results() :: #{pos_integer() => {C :: dkg_commitment:serialized_commitment(), Si :: binary()}}.
-type dkg() :: #dkg{}.
-type serialized_dkg() :: #serialized_dkg{}.

-export_type([dkg/0]).

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
         vss_map=VSSes}.

start(DKG = #dkg{id=Id, u=G1}) ->
    MyVSS = maps:get(Id, DKG#dkg.vss_map),
    Secret = erlang_pbc:element_random(erlang_pbc:element_new('Zr', G1)),
    {NewVSS, {send, ToSend}} = dkg_hybridvss:input(MyVSS, Secret),
    {DKG#dkg{vss_map=maps:put(Id, NewVSS, DKG#dkg.vss_map)}, {send, dkg_util:wrap({vss, Id}, ToSend)}}.

handle_msg(DKG=#dkg{leader=Leader}, Sender, {{vss, VSSId}, VssMSG}) ->
    case dkg_hybridvss:handle_msg(maps:get(VSSId, DKG#dkg.vss_map), Sender, VssMSG) of
        {NewVSS, ok} ->
            {DKG#dkg{vss_map=maps:put(VSSId, NewVSS, DKG#dkg.vss_map)}, ok};
        {NewVSS, {send, ToSend}} ->
            {DKG#dkg{vss_map=maps:put(VSSId, NewVSS, DKG#dkg.vss_map)}, {send, dkg_util:wrap({vss, VSSId}, ToSend)}};
        {NewVSS, {result, {_Session, Commitment, Si}}} ->
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
                             %% XXX these readies are currently not signed
                             rhat=[{signed_vss_ready, dkg_commitment:readies(Commitment)} | DKG#dkg.rhat]
                            },

            case length(NewDKG#dkg.qhat) == NewDKG#dkg.t + 1 andalso length(NewDKG#dkg.qbar) == 0 of
                true ->
                    case NewDKG#dkg.id == Leader of
                        true ->
                            {NewDKG, {send, [{multicast, {send, NewDKG#dkg.qhat, {rhat, NewDKG#dkg.rhat}}}]}};
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
handle_msg(DKG, _Sender, {send, Q, {rhat, _Rhat}}) ->
    %% TODO verify signatures
    case length(DKG#dkg.qbar) == 0 orelse lists:usort(DKG#dkg.qbar) == lists:usort(Q) of
        true ->
            {DKG, {send, [{multicast, {signed_echo, Q}}]}};
        false ->
            {DKG, ok}
    end;

%% upon a message (L, τ, echo, Q)sign from Pm (first time):
%%      e(L,Q) ← e(L,Q) + 1
%%      if e(L,Q) = ceil((n+t+1)/2) and r(L,Q) < t + 1 then
%%          Qbar ← Q; Mbar ← ceil((n+t+1)/2) signed echo messages for Q
%%          send the message (L, τ, ready, Q)sign to each Pj
handle_msg(DKG = #dkg{id=_Id, n=N, t=T}, Sender, {signed_echo, Q}=EchoMsg) ->
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
                    {NewDKG#dkg{qbar=NewQbar, mbar=NewMbar}, {send, [{multicast, {signed_ready, Q}}]}};
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
handle_msg(DKG = #dkg{id=_Id, n=N, t=T, f=F}, Sender, {signed_ready, Q}=ReadyMsg) ->
    case update_rlq(DKG, Sender, ReadyMsg) of
        false ->
            {DKG, ok};
        {true, NewDKG} ->
            case count_ready(NewDKG, Q) == T+1 andalso count_echo(NewDKG, Q) < ceil((N+T+1)/2) of
                true ->
                    %% NOTE: qbar and mbar are new assignments here
                    NewQbar = Q,
                    NewMbar = get_ready(NewDKG, Q),
                    {NewDKG#dkg{qbar=NewQbar, mbar=NewMbar}, {send, [{multicast, {signed_ready, Q}}]}};
                false ->
                    case count_ready(NewDKG, Q) == N-T-F of
                        true ->
                            %% TODO stop timer
                            %% TODO presumably we need to check this when we get a VSS result as well?
                            OutputCommitment = output_commitment(NewDKG),
                            PublicKeyShares = public_key_shares(NewDKG, OutputCommitment),
                            VerificationKey = verification_key(OutputCommitment),
                            Shard = shard(NewDKG),
                            {NewDKG, {result, {Shard, VerificationKey, PublicKeyShares}}};
                        false ->
                            {NewDKG, ok}
                    end
            end
    end;
handle_msg(DKG, _Sender, {signed_ready, _VSSDone}=_ReadyMsg) ->
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
                    leader=CurrentLeader}, _Sender, timeout) ->
    NewDKG = DKG#dkg{lc_flag=true},

    Msg = case length(Qbar) == 0 of
              true ->
                  {send, [{multicast, {signed_leader_change, CurrentLeader+1, Qhat, {rhat, Rhat}}}]};
              false ->
                  {send, [{multicast, {signed_leader_change, CurrentLeader+1, Qbar, {mbar, Mbar}}}]}
          end,
    {NewDKG, Msg};
handle_msg(DKG, _Sender, timeout) ->
    %% lc_flag is true
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
handle_msg(DKG=#dkg{leader=Leader, t=T, n=N, f=F, qhat=Qhat0, rhat=Rhat0, l_next=LNext},
           Sender,
           {signed_leader_change, Lbar, Q, RorM}=LeaderChangeMsg) when Lbar > Leader ->
    %% TODO: verify the signature(Q, R/M)
    case store_leader_change(DKG, Sender, LeaderChangeMsg) of
        {true, NewDKG0} ->
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
                            {send, [{multicast, {signed_leader_change, NewLNext, NewDKG#dkg.qhat, {rhat, NewDKG#dkg.rhat}}}]};
                        false ->
                            {send, [{multicast, {signed_leader_change, NewLNext, NewDKG#dkg.qbar, {mbar, NewDKG#dkg.mbar}}}]}
                    end;
                false ->
                    LeaderChangeMsgs = maps:get(Lbar, NewDKG#dkg.lc_map, []),
                    case length(LeaderChangeMsgs) == N-T-F of
                        true ->
                            NewerMbar = NewerRhat = LeaderChangeMsgs,
                            NewLeader = Lbar,
                            NewerLNext = l_next(Leader, N),
                            NewLeaderChangeMap = maps:put(NewLeader, [], NewDKG#dkg.lc_map),
                            NewerDKG = NewDKG#dkg{lc_flag=false,
                                                  mbar=NewerMbar,
                                                  rhat=NewerRhat,
                                                  leader=NewLeader,
                                                  l_next=NewerLNext,
                                                  lc_map=NewLeaderChangeMap},
                            case DKG#dkg.id == NewLeader of
                                true ->
                                    case length(NewerDKG#dkg.qbar) == 0 of
                                        true ->
                                            {NewerDKG, {send, [{multicast, {send, NewerDKG#dkg.qhat, {rhat, NewerDKG#dkg.rhat}}}]}};
                                        false ->
                                            {NewerDKG, {send, [{multicast, {send, NewerDKG#dkg.qbar, {mbar, NewerDKG#dkg.mbar}}}]}}
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
handle_msg(DKG, _Sender, {signed_leader_change, _Lbar, _, _}) ->
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

-spec l_next(pos_integer(), pos_integer()) -> pos_integer().
l_next(L, N) ->
    case L - 1 < 1 of
        true ->
            N;
        false ->
            L - 1
    end.

-spec count_echo(dkg(), qset()) -> non_neg_integer().
count_echo(_DKG=#dkg{elq=Elq, leader=Leader}, Q0) ->
    Q = lists:usort(Q0),
    length(maps:get({Leader, Q}, Elq, [])).

-spec get_echo(dkg(), qset()) -> [echo()].
get_echo(_DKG=#dkg{elq=Elq, leader=Leader}, Q0) ->
    Q = lists:usort(Q0),
    maps:get({Leader, Q}, Elq, []).

-spec update_elq(dkg(), pos_integer(), signed_echo()) -> {true, dkg()} | false.
update_elq(DKG=#dkg{elq=Elq}, Sender, {signed_echo, Q0}=EchoMsg) ->
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

-spec count_ready(dkg(), qset()) -> non_neg_integer().
count_ready(_DKG=#dkg{rlq=Rlq, leader=Leader}, Q0) ->
    Q = lists:usort(Q0),
    length(maps:get({Leader, Q}, Rlq, [])).

-spec get_ready(dkg(), qset()) -> [ready()].
get_ready(_DKG=#dkg{rlq=Rlq, leader=Leader}, Q0) ->
    Q = lists:usort(Q0),
    maps:get({Leader, Q}, Rlq, []).

-spec update_rlq(dkg(), pos_integer(), signed_ready()) -> {true, dkg()} | false.
update_rlq(DKG=#dkg{rlq=Rlq}, Sender, {signed_ready, Q0}=ReadyMsg) ->
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

-spec count_leader_change(dkg()) -> non_neg_integer().
count_leader_change(DKG) -> length(lists:flatten(maps:keys(DKG#dkg.lc_map))).

-spec store_leader_change(dkg(), pos_integer(), signed_leader_change()) -> {true, dkg()} | false.
store_leader_change(DKG, Sender, {signed_leader_change, Lbar, _, _}=LeaderChangeMsg) ->
    L = maps:get(Lbar, DKG#dkg.lc_map, []),
    case lists:keyfind(Sender, 1, L) of
        false ->
            NewLCM = maps:put(Lbar, lists:keystore(Sender, 1, L, {Sender, LeaderChangeMsg}), DKG#dkg.lc_map),
            {true, DKG#dkg{lc_map=NewLCM}};
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
               vss_map=VSSMap,
               vss_results=VSSResults,
               qbar=Qbar,
               qhat=Qhat,
               rhat=Rhat,
               mbar=Mbar,
               elq=Elq,
               rlq=Rlq,
               lc_flag=LCFlag,
               leader=Leader,
               l_next=LNext,
               lc_map=LCMap}) ->
    #serialized_dkg{id=Id,
                    n=N,
                    f=F,
                    t=T,
                    u=erlang_pbc:element_to_binary(U),
                    u2=erlang_pbc:element_to_binary(U2),
                    vss_map=serialize_vss_map(VSSMap),
                    vss_results=serialize_vss_results(VSSResults),
                    qbar=Qbar,
                    qhat=Qhat,
                    rhat=Rhat,
                    mbar=Mbar,
                    elq=Elq,
                    rlq=Rlq,
                    lc_flag=LCFlag,
                    leader=Leader,
                    l_next=LNext,
                    lc_map=LCMap}.

-spec deserialize(serialized_dkg(), erlang_pbc:element()) -> dkg().
deserialize(#serialized_dkg{id=Id,
                            n=N,
                            f=F,
                            t=T,
                            u=SerializedU,
                            u2=SerializedU2,
                            vss_map=SerializedVSSMap,
                            vss_results=SerializedVSSResults,
                            qbar=Qbar,
                            qhat=Qhat,
                            rhat=Rhat,
                            mbar=Mbar,
                            elq=Elq,
                            rlq=Rlq,
                            lc_flag=LCFlag,
                            leader=Leader,
                            l_next=LNext,
                            %% XXX: Only one element is enough?
                            %% presumably we need to generate U and U2 again to deserialize? Not sure...
                            lc_map=LCMap}, Element) ->
    #dkg{id=Id,
         n=N,
         f=F,
         t=T,
         u=erlang_pbc:binary_to_element(Element, SerializedU),
         u2=erlang_pbc:binary_to_element(Element, SerializedU2),
         vss_map=deserialize_vss_map(SerializedVSSMap, Element),
         vss_results=deserialize_vss_results(SerializedVSSResults, Element),
         qbar=Qbar,
         qhat=Qhat,
         rhat=Rhat,
         mbar=Mbar,
         elq=Elq,
         rlq=Rlq,
         lc_flag=LCFlag,
         leader=Leader,
         l_next=LNext,
         lc_map=LCMap}.

-spec serialize_vss_map(vss_map()) -> serialized_vss_map().
serialize_vss_map(VSSMap) ->
    maps:fold(fun(K, VSS, Acc) ->
                      maps:put(K, dkg_hybridvss:serialize(VSS), Acc)
              end, #{}, VSSMap).

-spec deserialize_vss_map(serialized_vss_map(), erlang_pbc:element()) -> vss_map().
deserialize_vss_map(SerializedVSSMap, Element) ->
    maps:fold(fun(K, VSS, Acc) ->
                      maps:put(K, dkg_hybridvss:deserialize(VSS, Element), Acc)
              end, #{}, SerializedVSSMap).

-spec serialize_vss_results(vss_results()) -> serialized_vss_results().
serialize_vss_results(VSSResults) ->
    maps:fold(fun(K, {C, Si}, Acc) ->
                      maps:put(K, {dkg_commitment:serialize(C), erlang_pbc:element_to_binary(Si)}, Acc)
              end, #{}, VSSResults).

-spec deserialize_vss_results(serialized_vss_results(), erlang_pbc:element()) -> vss_results().
deserialize_vss_results(SerializedVSSResults, U) ->
    maps:fold(fun(K, {C, Si}, Acc) ->
                      maps:put(K, {dkg_commitment:deserialize(C, U), erlang_pbc:binary_to_element(U, Si)}, Acc)
              end, #{}, SerializedVSSResults).

-spec status(dkg()) -> map().
status(DKG) ->
    #{id => DKG#dkg.id,
      vss_map => maps:map(fun(_K, VSS) -> dkg_hybridvss:status(VSS) end, DKG#dkg.vss_map),
      vss_results_from => maps:keys(DKG#dkg.vss_results),
      echoes_required => ceil(( DKG#dkg.n + DKG#dkg.t + 1 )/2),
      echoes_detail => maps:map(fun(_K, Echoes) ->
                                        lists:foldl(fun({Sender, {signed_echo, Q}}, Acc) ->
                                                            [#{sender => Sender,
                                                               echo_count => count_echo(DKG, Q),
                                                               echo => lists:sort([X || {X, _} <- get_echo(DKG, Q)])} | Acc]
                                                    end, [], Echoes)
                                end, DKG#dkg.elq),
      readies_details => maps:map(fun(_K, Readies) ->
                                          lists:foldl(fun({Sender, {signed_ready, Q}}, Acc) ->
                                                              [#{sender => Sender,
                                                                 ready_count => count_ready(DKG, Q),
                                                                 ready => lists:sort([X || {X, _} <- get_ready(DKG, Q)])} | Acc]
                                                      end, [], Readies)
                                  end, DKG#dkg.rlq),
      readies_required => (DKG#dkg.t + 1),
      lc_flag => DKG#dkg.lc_flag,
      leader => DKG#dkg.leader,
      l_next => DKG#dkg.l_next}.
