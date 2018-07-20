-module(dkg_hybridvss).

-export([init/5,
         init/6]).

-export([handle_msg/3]).

-record(state, {
            id :: non_neg_integer(),
            n :: non_neg_integer(),
            f :: non_neg_integer(),
            t :: non_neg_integer(),
            u :: erlang_pbc:element(),
            e :: erlang_pbc:element(),
            ph :: non_neg_integer(),
            is_dealer = false :: boolean(),
            recv_send = false :: boolean(),
            echoes = #{} :: map(),
            readies = #{} :: map(),
            commitments = #{} :: #{non_neg_integer() => dkg_commitment:commitment()}
         }).

init(Id, N, F, T, Ph) ->
    init(Id, N, F, T, Ph, 'SS512').

init(Id, N, F, T, Ph, Curve) ->
    Group = erlang_pbc:group_new(Curve),
    Generator = erlang_pbc:element_from_hash(erlang_pbc:element_new('G1', Group), <<"honeybadger">>),
    #state{id=Id,
           n=N,
           f=F,
           t=T,
           ph=Ph,
           u=Generator,
           e=Group}.

handle_msg(State=#state{e=E, t=T, n=N, ph=Ph, is_dealer=false}, _Sender, share) ->
    Secret = erlang_pbc:element_random(erlang_pbc:element_new('Zr', E)),
    BiPoly = dkg_bipolynomial:generate(E, T, Secret),
    Commitment = dkg_commitment:new(allnodes(N), E, BiPoly),

    Msgs = lists:map(fun(Node) ->
                             NodeZr = erlang_pbc:element_set(erlang_pbc:element_new('Zr', E), Node),
                             Aj = dkg_bipolynomial:apply(BiPoly, NodeZr),
                             {unicast, Node, {send, {Ph, Commitment, Aj}}}
                     end, allnodes(N)),
    NewState = State#state{is_dealer=true},
    {NewState, {send, Msgs}};
handle_msg(State, _Sender, share) ->
    {State, {error, already_dealer}};
handle_msg(State=#state{n=N, recv_send=false, commitments=Commitments}, Sender, {send, {Ph, Commitment, A}}) ->
    case dkg_commitment:verify_poly(Commitment, State#state.id, A) of
        true ->
            Msgs = lists:map(fun(Node) ->
                                     {unicast, Node, {echo, {Sender, Ph, Commitment, dkg_polynomial:apply(A, Node)}}}
                             end, allnodes(N)),
            NewState = State#state{recv_send=true, commitments=maps:put(Sender, Commitment, Commitments)},
            {NewState, {send, Msgs}};
        false ->
            {State, {error, failed_dkg_bipolynomial_verify_poly}}
    end;
handle_msg(State, _Sender, {send, {_Ph, _Commitment, _A}}) ->
    {State, {error, alread_received_send}};
handle_msg(State=#state{echoes=Echoes, id=Id, n=N, t=T, commitments=Commitments}, Sender, {echo, {Dealer, Ph, Commitment0, A}}) ->

    Commitment = maps:get(Dealer, Commitments, Commitment0),

    case dkg_commitment:verify_point(Commitment, Id, Sender, A) of
        true ->
            case dkg_commitment:add_echo(Commitment, Sender, A) of
                {true, NewCommitment} ->
                    case dkg_commitment:num_echoes(NewCommitment) == ceil((N+T+1)/2) andalso
                         dkg_commitment:num_readies(NewCommitment) < (T+1) of
                        true ->
                            Abar = dkg_commitment:interpolate(NewCommitment, echo, allnodes(N)),
                            Msgs = lists:map(fun(Node) ->
                                                     {unicast, Node, {ready, {Dealer, Ph, NewCommitment, dkg_polynomial:apply(Abar, Node)}}}
                                             end, allnodes(N)),
                            NewState = State#state{echoes=maps:put(Sender, true, Echoes), commitments=maps:put(Dealer, NewCommitment, Commitments)},
                            {NewState, {send, Msgs}};
                        false ->
                            NewState = State#state{echoes=maps:put(Sender, true, Echoes), commitments=maps:put(Dealer, NewCommitment, Commitments)},
                            {NewState, ok}
                    end;
                {false, _OldCommitment} ->
                    {State, ok}
            end;
        false ->
            {State, ok}
    end;
handle_msg(State=#state{readies=Readies, e=E, n=N, t=T, f=F, id=Id, commitments=Commitments}, Sender, {ready, {Dealer, Ph, Commitment0, A}}) ->
    ct:pal("Got ready, Sender: ~p, Id: ~p", [Sender, Id]),

    Commitment = maps:get(Dealer, Commitments, Commitment0),

    case dkg_commitment:verify_point(Commitment, Id, Sender, A) of
        true ->
            ct:pal("verify_point success. Sender: ~p, Id: ~p", [Sender, Id]),
            case dkg_commitment:add_ready(Commitment, Sender, A) of
                {true, NewCommitment} ->
                    case dkg_commitment:num_readies(NewCommitment) == (T+1) andalso
                         dkg_commitment:num_echoes(NewCommitment) < ceil((N+T+1)/2) of
                        true ->
                            Abar = dkg_commitment:interpolate(NewCommitment, ready, allnodes(N)),
                            Msgs = lists:map(fun(Node) ->
                                                     {unicast, Node, {ready, {Dealer, Ph, NewCommitment, dkg_polynomial:apply(Abar, Node)}}}
                                             end, allnodes(N)),
                            NewState = State#state{readies=maps:put(Sender, true, Readies), commitments=maps:put(Dealer, NewCommitment, Commitments)},
                            {NewState, {send, Msgs}};
                        false ->
                            case dkg_commitment:num_readies(NewCommitment) == (N-T-F) of
                                true->
                                    Abar = dkg_commitment:interpolate(NewCommitment, ready, allnodes(N)),
                                    Zero = erlang_pbc:element_set(erlang_pbc:element_new('Zr', E), 0),
                                    Share = dkg_polynomial:apply(Abar, Zero),
                                    NewState = State#state{readies=maps:put(Sender, true, Readies), commitments=maps:put(Dealer, NewCommitment, Commitments)},
                                    {NewState, {result, {Dealer, Ph, NewCommitment, Share}}};
                                false ->
                                    NewState = State#state{readies=maps:put(Sender, true, Readies), commitments=maps:put(Dealer, NewCommitment, Commitments)},
                                    {NewState, ok}
                            end
                    end;
                {false, _OldCommitment} ->
                    {State, ok}
            end;
        false ->
            ct:pal("verify_point failed. Sender: ~p, Id: ~p", [Sender, Id]),
            {State, ok}
    end;
handle_msg(State, _Sender, Msg) ->
    {State, {unhandled_msg, Msg}}.

allnodes(N) ->
    lists:seq(1, N).
