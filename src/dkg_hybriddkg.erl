-module(dkg_hybriddkg).

-export([init/7, start/1]).

-export([handle_msg/3]).

-type session() :: {Leader :: pos_integer(), Round :: pos_integer()}.
-type qhat() :: #{{D :: pos_integer(), Session :: session()} => {C :: dkg_commitment:commitment(), Si :: erlang_pbc:element(), SignedReadyMsgs :: any()}}.
-type qbar() :: #{{D :: pos_integer(), Session :: session()} => #{signed_ready => any(), signed_echo => any(), signed_leader_change => any()}}.

-record(dkg, {
          state = leader_unconfirmed :: leader_unconfirmed | functional | agreement_started | agreement_completed | leader_change_started | dkg_completed,
          id :: pos_integer(),
          n :: pos_integer(),
          f :: pos_integer(),
          t :: pos_integer(),
          u :: erlang_pbc:element(),
          u2 :: erlang_pbc:element(),
          vss_map :: #{pos_integer() => dkg_hybridvss:vss()},
          qhat = #{} :: qhat(),
          qbar = #{} :: qbar(),
          session :: session(),
          lc_flag = false :: boolean(),
          leader :: pos_integer(),
          l_next :: pos_integer(),
          leader_change = #{} :: map()
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
         l_next=N,
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
handle_msg(DKG = #dkg{id=Id, session=Session={Leader, _}}, Sender, {{vss, VSSId, Session}, VssMSG}) ->
    case dkg_hybridvss:handle_msg(maps:get(VSSId, DKG#dkg.vss_map), Sender, VssMSG) of
        {NewVSS, ok} ->
            {DKG#dkg{vss_map=maps:put(VSSId, NewVSS, DKG#dkg.vss_map), state=agreement_started}, ok};
        {NewVSS, {send, ToSend}} ->
            {DKG#dkg{vss_map=maps:put(VSSId, NewVSS, DKG#dkg.vss_map), state=agreement_started}, {send, dkg_util:wrap({vss, VSSId, Session}, ToSend)}};
        {NewVSS, {result, {_Session, _Commitment, _Si, _Rd}}=Res} ->
            %% upon (Pd, τ, out, shared, Cd , si,d , Rd ) (first time):
            %%      Qhat ← {Pd}; Rhat ← {Rd}
            %%      if |Qhat| = t + 1 and Qbar = ∅ then
            %%           if Pi = L then
            %%               send the message (L, τ, send, Qhat, Rhat) to each Pj
            %%            else
            %%               delay ← delay(T); start timer(delay)
            %% TODO 'extended' VSS should output signed ready messages
            case update_qhat(DKG, VSSId, Res) of
                false ->
                    {DKG, ok};
                {true, NewDKG} ->
                    %% ct:pal("DKG: ~p, VSS: ~p, update_qhat", [Id, VSSId]),
                    ct:pal("DKG: ~p, VSS: ~p, count_ready: ~p", [Id, VSSId, count_ready(NewDKG)]),
                    case count_vss_ready(NewDKG) == NewDKG#dkg.t + 1 andalso maps:size(NewDKG#dkg.qbar) == 0 of
                        true ->
                            ct:pal("DKG: ~p, VSS: ~p, count_ready: ~p", [Id, VSSId, count_ready(NewDKG)]),
                            case NewDKG#dkg.id == Leader of
                                true ->
                                    %% send, send message
                                    {NewDKG#dkg{vss_map=maps:put(VSSId, NewVSS, DKG#dkg.vss_map)}, {send, [{multicast, {send, Session, maps:keys(NewDKG#dkg.qhat)}}]}};
                                false ->
                                    %% start timer
                                    {NewDKG#dkg{vss_map=maps:put(VSSId, NewVSS, DKG#dkg.vss_map)}, start_timer}
                            end;
                        false ->
                            {NewDKG, ok}
                    end
            end
    end;

%% upon a message (L, τ, send, Q, R/M) from L (first time):
%%      if verify-signature(Q, R/M) and (Qbar = ∅  or Qbar = Q) then
%%          send the message (L, τ, echo, Q)sign to each Pj
handle_msg(DKG = #dkg{session=Session={Leader, _}}, Sender, {send, Session, VSSDone}) when Sender == Leader ->
    %% TODO verify signatures
    case maps:size(DKG#dkg.qbar) == 0 orelse maps:size(DKG#dkg.qbar) == length(VSSDone) of
        true ->
            {DKG, {send, [{multicast, {echo, Session, VSSDone}}]}};
        false ->
            {DKG, ok}
    end;
handle_msg(DKG, Sender, {send, _Session, _VSSDone}) ->
    ct:pal("~p got message for different leader ~p", [DKG#dkg.id, Sender]),
    {DKG, ok};


%% upon a message (L, τ, echo, Q)sign from Pm (first time):
%%      e(L,Q) ← e(L,Q) + 1
%%      if e(L,Q) = ceil((n+t+1)/2) and r(L,Q) < t + 1 then
%%          Qbar ← Q; Mbar ← ceil((n+t+1)/2) signed echo messages for Q
%%          send the message (L, τ, ready, Q)sign to each Pj
handle_msg(DKG = #dkg{session=Session, n=N, t=T}, Sender, {echo, Session, VSSDone}=EchoMsg) ->
    case store_echo(DKG, Sender, EchoMsg) of
        false ->
            {DKG, ok};
        {true, NewDKG} ->
            case count_echo(NewDKG) == ceil((N+T+1)/2) andalso count_ready(NewDKG) < T+1 of
                true ->
                    %% send ready message
                    {NewDKG, {send, [{multicast, {ready, Session, VSSDone}}]}};
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
handle_msg(DKG = #dkg{n=N, t=T, f=F}, Sender, {ready, Session, VSSDone}=ReadyMsg) ->
    case store_ready(DKG, Sender, ReadyMsg) of
        false ->
            {DKG, ok};
        {true, NewDKG} ->
            case count_ready(NewDKG) == T+1 andalso count_echo(NewDKG) < ceil((N+T+1)/2) of
                true ->
                    {NewDKG, {ready, Session, VSSDone}};
                false ->
                    case count_ready(NewDKG) == N-T-F of
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

%% upon timeout:
%% if lcflag = false then
%%      if Q = ∅ then
%%          lcflag ← true; send msg (τ, lead-ch, L + 1, Qhat, Rhat)sign to each Pj
%%      else
%%          lcflag ← true; send msg (τ, lead-ch, L + 1, Qbar, Mbar)sign to each Pj
handle_msg(DKG=#dkg{lc_flag=false,
                    t=T,
                    n=N,
                    qbar=Qbar,
                    qhat=Qhat,
                    session={CurrentLeader, Round}}, _Sender, timeout) ->
    NewDKG = DKG#dkg{lc_flag=true},
    Msg = case maps:size(Qbar) == 0 of
              true ->
                  {send, [{multicast, {leader_change, {CurrentLeader+1, Round}, {signed_vss_ready, Qhat}}}]};
              false ->
                  MBar = case count_ready(NewDKG) == T + 1 of
                             true ->
                                 %% Mbar is t+1 signed ready messages
                                 {signed_ready, get_msg(NewDKG, signed_ready)};
                             false ->
                                 case count_echo(NewDKG) == ceil((N+T+1)/2) of
                                     true ->
                                         {signed_echo, get_msg(NewDKG, signed_echo)};
                                     false ->
                                         erlang:error(wtf)
                                 end
                         end,
                  {send, [{multicast, {leader_change, {CurrentLeader+1, Round}, MBar}}]}
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

handle_msg(DKG=#dkg{leader=Leader, n=N, t=T, f=F, qbar=Qbar, l_next=LNext}, Sender, {leader_change, {Lbar, Round}, Checkpoint}=LeaderChangeMsg) when Lbar > Leader ->
    NewLNext = min(LNext, Lbar),
    %% XXX is the leader change message unique by sender or by the Q,R/M attachment, or both?
    %% TODO we need to verify these messages are signed
    case store_leader_change(DKG, Sender, LeaderChangeMsg) of
       false ->
            %% already received leader-change
            {DKG, ok};
        {true, NewDKG0} ->
            %% we need to *update* QHat or QBar here
            NewDKG = case Checkpoint of
                         {signed_vss_ready, SignedVSSReady} ->
                             %% I *think* we merge the sets here?
                             NewDKG0#dkg{qhat=maps:merge(NewDKG0#dkg.qhat, SignedVSSReady)};
                         {signed_echo, SignedEchoes} ->
                             lists:foldl(fun({S, Msg}, Acc) ->
                                                 store_echo(Acc, S, Msg)
                                         end, NewDKG0, maps:to_list(SignedEchoes));
                         {signed_ready, SignedReadies} ->
                             lists:foldl(fun({S, Msg}, Acc) ->
                                                 store_ready(Acc, S, Msg)
                                         end, NewDKG0, maps:to_list(SignedReadies))
                             %% TODO can these be leader change messages too?
                     end,
            case count_leader_change(NewDKG) == T+F+1 andalso not DKG#dkg.lc_flag of
                true ->
                    Msg = case maps:size(NewDKG#dkg.qbar) == 0 of
                              true ->
                                  {send, [{multicast, {leader_change, {NewLNext, Round}, {signed_vss_ready, NewDKG#dkg.qhat}}}]};
                              false ->
                                  {send, [{multicast, {leader_change, {NewLNext, Round}, {qbar, Qbar}}}]}
                          end,
                    {NewDKG, Msg};
                false ->
                    case leader_change_count(NewDKG, Lbar) == N - T - F of
                        true ->
                            MBar = maps:get(Lbar, DKG#dkg.leader_change),
                            case DKG#dkg.id == Lbar of
                                true ->
                                    Msg = case maps:size(NewDKG#dkg.qbar) == 0 of
                                              true ->
                                                  {send, [{multicast, {send, {NewLNext, Round}, NewDKG#dkg.qhat}}]};
                                              false ->
                                                  {send, [{multicast, {send, {NewLNext, Round}, MBar}}]}
                                          end,
                                    {NewDKG#dkg{leader=Lbar, lc_flag=false}, Msg};
                                false ->
                                    {NewDKG#dkg{leader=Lbar, lc_flag=false}, start_timer}
                            end;
                        false ->
                            {NewDKG, ok}
                    end
            end
    end;
handle_msg(DKG, _Sender, {leader_change, {_Lbar, _Round}, _}) ->
    {DKG, ok};


handle_msg(DKG, _Sender, Msg) ->
    {DKG, {unhandled_msg, Msg}}.

%% helper functions
-spec output_commitment(#dkg{}) -> dkg_commitment:commitment().
output_commitment(_DKG=#dkg{qhat=Qhat, u2=U2, t=T, n=N}) ->
    maps:fold(fun(_K, {Commitment, _, _}, Acc) ->
                      dkg_commitment:mul(Commitment, Acc)
              end, dkg_commitment:new(lists:seq(1, N), U2, T), Qhat).

-spec public_key_shares(#dkg{}, dkg_commitment:commitment()) -> [erlang_pbc:element()].
public_key_shares(_DKG=#dkg{n=N}, OutputCommitment) ->
    [dkg_commitment:public_key_share(OutputCommitment, NodeID) || NodeID <- dkg_util:allnodes(N)].

-spec verification_key(dkg_commitment:commitment()) -> erlang_pbc:element().
verification_key(OutputCommitment) ->
    dkg_commitmentmatrix:lookup([1, 1], dkg_commitment:matrix(OutputCommitment)).

-spec shard(#dkg{}) -> erlang_pbc:element().
shard(_DKG=#dkg{qhat=Qhat, u=U}) ->
    Zero = erlang_pbc:element_set(erlang_pbc:element_new('Zr', U), 0),
    maps:fold(fun(_K, {_, Si, _}, Acc) ->
                        erlang_pbc:element_add(Acc, Si)
              end, Zero, Qhat).

store_echo(DKG, Sender, EchoMsg) ->
    store_msg(DKG, Sender, {signed_echo, EchoMsg}).

store_ready(DKG, Sender, ReadyMsg) ->
    store_msg(DKG, Sender, {signed_ready, ReadyMsg}).

store_leader_change(DKG, Sender, {leader_change, {Lbar, _Round}, _}=LeaderChangeMsg) ->
    L = maps:get(Lbar, DKG#dkg.leader_change, []),
    case lists:keyfind(Sender, 1, L) of
        false ->
            NewLCM = maps:put(Lbar, lists:keystore(Sender, 1, L, {Sender, LeaderChangeMsg}), DKG#dkg.leader_change),
            {true, DKG#dkg{leader_change=NewLCM}};
        true ->
            false
    end.
    %store_msg(DKG, Sender, {signed_leader_change, LeaderChangeMsg}).

store_msg(DKG, Sender, {MsgType, Msg}) ->
    QbarSender = maps:get({Sender, DKG#dkg.session}, DKG#dkg.qbar, #{}),
    case maps:is_key(MsgType, QbarSender) of
        true ->
            false;
        false ->
            NewQbar = maps:put({Sender, DKG#dkg.session}, maps:put(MsgType, Msg, QbarSender), DKG#dkg.qbar),
            {true, DKG#dkg{qbar=NewQbar}}
    end.

count_echo(DKG) -> maps:size(get_msg(DKG, signed_echo)).
count_ready(DKG) -> maps:size(get_msg(DKG, signed_ready)).
count_leader_change(DKG) -> length(lists:flatten(maps:keys(DKG#dkg.leader_change))).

get_msg(DKG=#dkg{session=Session0}, MsgType) ->
    maps:fold(fun({K, Session}, V, Acc) when Session == Session0 ->
                      case maps:is_key(MsgType, V) of
                          true ->
                              maps:put(K, maps:get(MsgType, V), Acc);
                          false ->
                              Acc
                      end;
                 (_, _, Acc) ->
                      Acc
              end, #{}, DKG#dkg.qbar).

leader_change_count(DKG, L) ->
    length(maps:get(L, DKG#dkg.leader_change, [])).

update_qhat(DKG=#dkg{id=_Id, qhat=Qhat}, VSSId, {result, {Session, Commitment, Si, Rd}}) ->
    %% ct:pal("DKG: ~p, VSS: ~p, update_qhat", [Id, VSSId]),
    case maps:is_key({VSSId, Session}, Qhat) of
        true ->
            false;
        false ->
            NewQhat = maps:put({VSSId, Session}, {Commitment, Si, Rd}, Qhat),
            {true, DKG#dkg{qhat=NewQhat}}
    end.

count_vss_ready(DKG) ->
    maps:size(DKG#dkg.qhat).

