-module(dkg_hybriddkg).

-export([init/6, init/7]).

-export([handle_msg/3]).

-type session() :: {Leader :: pos_integer(), Round :: pos_integer()}.

 %% Q hat in the protocol
-type qhat() :: #{pos_integer() => {dkg_commitment:commitment(), erlang_pbc:element()}}.
-type qbar() :: #{pos_integer() => erlang_pbc:element()}.

-record(state, {
          id :: pos_integer(),
          n :: pos_integer(),
          f :: pos_integer(),
          t :: pos_integer(),
          u :: erlang_pbc:element(),
          u2 :: erlang_pbc:element(),
          vss_map :: #{pos_integer() => dkg_hybridvss:vss()},
          echoes_this_round = [] :: [non_neg_integer()], %% eL,Q in the protocol, echoes seen this round
          readies_this_round = [] :: [non_neg_integer()], %% rL,Q in the protocol, readies seen this round
          vss_done_this_round = #{} :: qhat(),
          vss_done_last_round = #{} :: qbar(),
          leader = 1 :: pos_integer(),
          session :: session()
         }).

%% upon initialization:
%%      e(L, Q) <- 0 and r(L, Q) <- 0 for every Q; Qbar <- ∅; Q <- ∅
%%      Mbar <- Rhat <- n-t-f signed lead-ch messages for leader L
%%      cnt ← 0; cntl ← 0 for all l ∈ [1, n];
%%      lcl ← 0 for each leader L; lcflag ← false
%%      Lnext ← L + n − 1
%%      for all d ∈ [1, n] do
%%          initialize extended-HybridVSS Sh protocol (Pd, τ )
init(Id, N, F, T, Generator, Round) ->
    case erlang_pbc:pairing_is_symmetric(Generator) of
        true ->
            init(Id, N, F, T, Generator, Generator, Round);
        false ->
            erlang:error(pairing_not_symmetric)
    end.

init(Id, N, F, T, Generator, G2, Round) ->
    Session = {1, Round},
    {VSSes, Msgs} = lists:foldl(fun(E, {Map, ToSendAcc}) ->
                                        VSS = dkg_hybridvss:init(Id, N, F, T, Generator, G2, {E, Round}),
                                        case E == Id of
                                            true ->
                                                Secret = erlang_pbc:element_random(erlang_pbc:element_new('Zr', Generator)),
                                                case dkg_hybridvss:input(VSS, Secret) of
                                                    {NewVSS, {send, ToSend}} ->
                                                        {maps:put(E, NewVSS, Map), dkg_util:wrap({vss, E, Session}, ToSend)};
                                                    {error, Reason} ->
                                                        {error, Reason}
                                                end;
                                            false ->
                                                {maps:put(E, VSS, Map), ToSendAcc}
                                        end
                                end, {#{}, []}, dkg_util:allnodes(N)),
    {#state{id=Id, n=N, f=F, t=T, u=Generator, u2=G2, session=Session, vss_map=VSSes}, {send, Msgs}}.

%% upon (Pd, τ, out, shared, Cd , si,d , Rd ) (first time):
%%      Qhat ← {Pd}; Rhat ← {Rd}
%%      if |Qhat| = t + 1 and Qbar = ∅ then
%%           if Pi = L then
%%               send the message (L, τ, send, Qhat, Rhat) to each Pj
%%            else
%%               delay ← delay(T); start timer(delay)
handle_msg(State = #state{session=Session={Leader, _}}, Sender, {{vss, VssID, Session}, VssMSG}) ->
    case dkg_hybridvss:handle_msg(maps:get(VssID, State#state.vss_map), Sender, VssMSG) of
        {NewVSS, ok} ->
            {State#state{vss_map=maps:put(VssID, NewVSS, State#state.vss_map)}, ok};
        {NewVSS, {send, ToSend}} ->
            {State#state{vss_map=maps:put(VssID, NewVSS, State#state.vss_map)}, {send, dkg_util:wrap({vss, VssID, Session}, ToSend)}};
        {NewVSS, {result, {_Session, Commitment, Si}}} ->
            %% TODO 'extended' VSS should output signed ready messages
            VSSDoneThisRound = maps:put(VssID, {Commitment, Si}, State#state.vss_done_this_round),
            case maps:size(VSSDoneThisRound) == State#state.t + 1 andalso maps:size(State#state.vss_done_last_round) == 0 of
                true ->
                    case State#state.id == Leader of
                        true ->
                            %% this is not multicast in the protocol, but we have multicast support, sooooooo....
                            {State#state{vss_map=maps:put(VssID, NewVSS, State#state.vss_map),
                                         vss_done_this_round=VSSDoneThisRound},
                             {send, [{multicast, {send, Session, maps:keys(VSSDoneThisRound)}}]}};
                        false ->
                            {State#state{vss_map=maps:put(VssID, NewVSS, State#state.vss_map), vss_done_this_round=VSSDoneThisRound}, ok}
                    end;
                false ->
                    {State#state{vss_map=maps:put(VssID, NewVSS, State#state.vss_map), vss_done_this_round=VSSDoneThisRound}, ok}
            end;
        {error, Reason} ->
            {error, Reason}
    end;

%% upon a message (L, τ, send, Q, R/M) from L (first time):
%%      if verify-signature(Q, R/M) and (Qbar = ∅  or Qbar = Q) then
%%          send the message (L, τ, echo, Q)sign to each Pj
handle_msg(State = #state{session=Session={Leader, _}}, Sender, {send, Session, VSSDone}) when Sender == Leader ->
    %% TODO verify signatures
    case maps:size(State#state.vss_done_last_round) == 0 orelse lists:sort(maps:keys(State#state.vss_done_this_round)) == lists:sort(VSSDone) of
        true ->
            {State, {send, [{multicast, {echo, Session, VSSDone}}]}};
        false ->
            {State, ok}
    end;

%% upon a message (L, τ, echo, Q)sign from Pm (first time):
%%      e(L,Q) ← e(L,Q) + 1
%%      if e(L,Q) = ceil((n+t+1)/2) and r(L,Q) < t + 1 then
%%          Qbar ← Q; Mbar ← ceil((n+t+1)/2) signed echo messages for Q
%%          send the message (L, τ, ready, Q)sign to each Pj
handle_msg(State = #state{session=Session, n=N, t=T}, Sender, {echo, Session, VSSDone}) ->
    case lists:member(Sender, State#state.echoes_this_round) of
        false ->
            EchoesThisRound = [Sender | State#state.echoes_this_round],
            case length(EchoesThisRound) == ceil((N + T + 1) / 2) andalso length(State#state.readies_this_round) < T + 1 of
                true ->
                    %% TODO QBar <- Q, MBar <- ...
                    {State#state{echoes_this_round=EchoesThisRound}, {send, [{multicast, {ready, Session, VSSDone}}]}};
                false ->
                    {State#state{echoes_this_round=EchoesThisRound}, ok}
            end;
        true ->
            {State, ok}
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
handle_msg(State = #state{n=N, t=T, f=F}, Sender, {ready, Session, VSSDone}) ->
    case lists:member(Sender, State#state.readies_this_round) of
        false ->
            ReadiesThisRound = [Sender | State#state.readies_this_round],
            case length(ReadiesThisRound) == T + 1 andalso length(State#state.echoes_this_round) < ceil((N + T + 1) /2) of
                true ->
                    %% TODO QBar <- Q, MBar <- ...
                    {State#state{readies_this_round=ReadiesThisRound}, {send, [{multicast, {ready, Session, VSSDone}}]}};
                false ->
                    case length(ReadiesThisRound) == N - T - F of
                        true ->
                            case lists:all(fun(E) -> maps:is_key(E, State#state.vss_done_this_round) end, ReadiesThisRound) of
                                true ->
                                    %% TODO presumably we need to check this when we get a VSS result as well?
                                    OutputCommitment = output_commitment(State, VSSDone),
                                    PublicKeyShares = public_key_shares(State, OutputCommitment),
                                    VerificationKey = verification_key(OutputCommitment),
                                    Shard = shard(State, VSSDone),
                                    {State#state{readies_this_round=ReadiesThisRound}, {result, {Shard, VerificationKey, PublicKeyShares}}};
                                false ->
                                    {State#state{readies_this_round=ReadiesThisRound}, ok}
                            end;
                        false ->
                            {State#state{readies_this_round=ReadiesThisRound}, ok}
                    end
            end;
        true ->
            {State, ok}
    end;
handle_msg(State, _Sender, Msg) ->
    {State, {unhandled_msg, Msg}}.

%% helper functions
-spec output_commitment(#state{}, [non_neg_integer()]) -> dkg_commitment:commitment().
output_commitment(_State=#state{vss_done_this_round=VSSDoneThisRound}, VSSDone) ->
    [FirstCommitment | RemainingCommitments] = lists:foldl(fun(VSSId, Acc) ->
                                                                   {Commitment, _} = maps:get(VSSId, VSSDoneThisRound),
                                                                   [Commitment | Acc]
                                                           end, [], VSSDone),
    lists:foldl(fun(Commitment, Acc) ->
                        dkg_commitment:mul(Acc, Commitment)
                end, FirstCommitment, RemainingCommitments).

-spec public_key_shares(#state{}, dkg_commitment:commitment()) -> [erlang_pbc:element()].
public_key_shares(_State=#state{n=N}, OutputCommitment) ->
    [dkg_commitment:public_key_share(OutputCommitment, NodeID) || NodeID <- dkg_util:allnodes(N)].

-spec verification_key(dkg_commitment:commitment()) -> erlang_pbc:element().
verification_key(OutputCommitment) ->
    dkg_commitmentmatrix:lookup([1, 1], dkg_commitment:matrix(OutputCommitment)).

-spec shard(#state{}, [non_neg_integer()]) -> erlang_pbc:element().
shard(_State=#state{vss_done_this_round=VSSDoneThisRound}, VSSDone) ->
    [FirstShare | RemainingShares] = lists:foldl(fun(VSSId, Acc) ->
                                                         {_, Share} = maps:get(VSSId, VSSDoneThisRound),
                                                         [Share | Acc]
                                                 end, [], VSSDone),
    lists:foldl(fun(Share, Acc) ->
                        erlang_pbc:element_add(Acc, Share)
                end, FirstShare, RemainingShares).
