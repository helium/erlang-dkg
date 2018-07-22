-module(dkg_hybriddkg).

-export([init/6]).

-export([handle_msg/3]).

-type session() :: {Leader :: pos_integer(), Round :: pos_integer()}.

-record(state, {
          id :: pos_integer(),
          n :: pos_integer(),
          f :: pos_integer(),
          t :: pos_integer(),
          u :: erlang_pbc:element(),
          vss_map :: #{pos_integer() => dkg_hybridvss:vss()},
          echoes_this_round = [] :: [non_neg_integer()], %% eL,Q in the protocol, echoes seen this round
          readies_this_round = [] :: [non_neg_integer()], %% rL,Q in the protocol, readies seen this round
          vss_done_this_round = #{} :: #{pos_integer() => erlang_pbc:element()}, %% Q hat in the protocol
          vss_done_last_round = #{} :: #{pos_integer() => erlang_pbc:element()}, %% Q bar in the protocol
          leader = 1 :: pos_integer(),
          session :: session()
         }).

init(Id, N, T, F, Generator, Round) ->
    Session = {1, Round},
    {VSSes, Msgs} = lists:foldl(fun(E, {Map, ToSendAcc}) ->
                              VSS = dkg_hybridvss:init(Id, N, T, F, Generator, {E, Round}),
                              case E == Id of
                                  true ->
                                      Secret = erlang_pbc:element_random(erlang_pbc:element_new('Zr', Generator)),
                                      {NewVSS, {send, ToSend}} = dkg_hybridvss:input(VSS, Secret),
                                      {maps:put(E, NewVSS, Map), wrap({vss, E, Session}, ToSend)};
                                  false ->
                                      {maps:put(E, VSS, Map), ToSendAcc}
                              end
                      end, {#{}, []}, lists:seq(1, N)),
    {#state{id=Id, n=N, f=F, t=T, u=Generator, session=Session, vss_map=VSSes}, {send, Msgs}}.

handle_msg(State = #state{session=Session={Leader, _}}, Sender, {{vss, VssID, Session}, VssMSG}) ->
    case dkg_hybridvss:handle_msg(maps:get(VssID, State#state.vss_map), Sender, VssMSG) of
        {NewVSS, ok} ->
            {State#state{vss_map=maps:put(VssID, NewVSS, State#state.vss_map)}, ok};
        {NewVSS, {send, ToSend}} ->
            {State#state{vss_map=maps:put(VssID, NewVSS, State#state.vss_map)}, {send, wrap({vss, VssID, Session}, ToSend)}};
        {NewVSS, {result, {_Session, _Commitment, Si}}} ->
            %% TODO 'extended' VSS should output signed ready messages
            VSSDoneThisRound = maps:put(VssID, Si, State#state.vss_done_this_round),
            case maps:size(VSSDoneThisRound) == State#state.t + 1 andalso maps:size(State#state.vss_done_last_round) == 0 of
                true ->
                    case State#state.id == Leader of
                        true ->
                            %% this is not multicast in the protocol, but we have multicast support, sooooooo....
                            {State#state{vss_map=maps:put(VssID, NewVSS, State#state.vss_map), vss_done_this_round=VSSDoneThisRound}, {send, [{multicast, {send, Session, maps:keys(VSSDoneThisRound)}}]}};
                        false ->
                            {State#state{vss_map=maps:put(VssID, NewVSS, State#state.vss_map), vss_done_this_round=VSSDoneThisRound}, ok}
                    end;
                false ->
                    {State#state{vss_map=maps:put(VssID, NewVSS, State#state.vss_map), vss_done_this_round=VSSDoneThisRound}, ok}
            end
    end;
handle_msg(State = #state{session=Session={Leader, _}}, Sender, {send, Session, VSSDone}) when Sender == Leader ->
    %% TODO verify signatures
    case maps:size(State#state.vss_done_last_round) == 0 orelse lists:sort(maps:keys(State#state.vss_done_this_round)) == lists:sort(VSSDone) of
        true ->
            {State, {send, [{multicast, {echo, Session, VSSDone}}]}};
        false ->
            {State, ok}
    end;
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
                                    %% do some magic shit to create the key shard

                                    %% XXX: multiply the commitment matrices for each commitment
                                    %% and ignore the readies and echoes inside the commitment?
                                    %% is that what we're supposed to do?
                                    %% we also have dkg_commitment:mul, but it does the same thing as this
                                    %% don't know what happens to the readies and echos, perhaps those
                                    %% don't matter once this point is reached
                                    Matrices = [ dkg_commitment:matrix(dkg_hybridvss:commitment(VSS)) || {_VSSId, VSS} <- maps:to_list(State#state.vss_map)],
                                    OutputMatrix = lists:foldl(fun(Matrix, Acc) ->
                                                                       dkg_commitmentmatrix:mul(Matrix, Acc)
                                                                       %% this matrix multiplication is commutative?
                                                               end, hd(Matrices), Matrices), %% this is probably C in the output message

                                    %% XXX: add the shares up and output that
                                    Shares = find_shares(State#state.vss_done_this_round),
                                    Zero = erlang_pbc:element_set(erlang_pbc:element_new('Zr', State#state.u), 0),
                                    Shard = lists:foldl(fun(Share, Acc) ->
                                                                erlang_pbc:element_add(Share, Acc)
                                                        end, Zero, Shares),

                                    ct:pal("Id: ~p, Shard: ~p, OutputMatrix: ~p", [State#state.id, Shard, OutputMatrix]),

                                    {State#state{readies_this_round=ReadiesThisRound}, {result, {OutputMatrix, Shard}}};
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

%% wrap a subprotocol's outbound messages with a protocol identifier
-spec wrap(Tag :: atom() | {atom(), non_neg_integer()}, [{multicast, Msg :: any()} | {unicast, non_neg_integer(),  Msg :: any()}]) -> [{multicast, {Tag, Msg}} | {unicast, non_neg_integer(), {Tag, Msg}}].
wrap(_, []) ->
    [];
wrap(Id, [{multicast, Msg}|T]) ->
    [{multicast, {Id, Msg}}|wrap(Id, T)];
wrap(Id, [{unicast, Dest, Msg}|T]) ->
    [{unicast, Dest, {Id, Msg}}|wrap(Id, T)].

find_shares(NodesAndShares) ->
    {Indices0, Elements} = lists:unzip(maps:to_list(NodesAndShares)),
    Indices = [ erlang_pbc:element_set(erlang_pbc:element_new('Zr', hd(Elements)), I) || I <- Indices0 ],
    Shares = lists:foldl(fun(Index, Acc) ->
                                 case maps:is_key(Index, NodesAndShares) of
                                     false ->
                                         %% Node ${Index} has not sent us a share, interpolate it
                                         Alpha = erlang_pbc:element_set(erlang_pbc:element_new('Zr', hd(Elements)), Index),
                                         LagrangePoly = dkg_lagrange:coefficients(Indices, Alpha),
                                         InterpolatedShare = dkg_lagrange:apply_zr(LagrangePoly, Elements),
                                         [ InterpolatedShare | Acc];
                                     true ->
                                         %% Node ${Index} has sent us a share
                                         [ maps:get(Index, NodesAndShares) | Acc]
                                 end
                         end, [], [0 | lists:seq(1, maps:size(NodesAndShares))]), %% note that we also evaluate at 0
    lists:reverse(Shares).
