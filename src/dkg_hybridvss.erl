-module(dkg_hybridvss).

-export([init/5,
         init/6,
         secret/1
        ]).

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
            commitments = #{} :: #{non_neg_integer() => dkg_commitment:commitment()},
            secret :: erlang_pbc:element()
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


%% upon a message (Pd, τ, in, share, s): /* only Pd */
%% choose a symmetric bivariate polynomial φ(x, y) = Pt
%%  j,`=0 φj` xj y` ∈R ZpC ← {Cj` }tj,`=0 where Cj` = gφj` for j, ` ∈ [0, t]
%% for all j ∈ [1, n] do
%% aj (y) ← φ(j, y); send the message (Pd , τ, send, C, aj ) to Pj
%% [x, y] and φ00 = s
handle_msg(State=#state{e=E, t=T, n=N, ph=Ph, is_dealer=false}, _Sender, share) ->
    Secret = erlang_pbc:element_random(erlang_pbc:element_new('Zr', E)),
    ct:pal("Secret: ~p", [erlang_pbc:element_to_string(Secret)]),
    BiPoly = dkg_bipolynomial:generate(E, T, Secret),
    Commitment = dkg_commitment:new(allnodes(N), E, BiPoly),

    Msgs = lists:map(fun(Node) ->
                             NodeZr = erlang_pbc:element_set(erlang_pbc:element_new('Zr', E), Node),
                             Aj = dkg_bipolynomial:apply(BiPoly, NodeZr),
                             {unicast, Node, {send, {Ph, Commitment, Aj}}}
                     end, allnodes(N)),
    NewState = State#state{is_dealer=true, secret=Secret},
    {NewState, {send, Msgs}};
handle_msg(State, _Sender, share) ->
    {State, {error, already_dealer}};

%% upon a message (Pd, τ, send, C, a) from Pd (first time):
%% if verify-poly(C, i, a) then
%% for all j ∈ [1, n] do
%% send the message (Pd , τ, echo, C, a(j)) to Pj
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

%% upon a message (Pd, τ, echo, C, α) from Pm (first time):
%% if verify-point(C, i, m, α) then
%% AC ← AC ∪ {(m, α)}; eC ← eC + 1
%% if eC = d n+t+1/2 and rC < t + 1 then
%% Lagrange-interpolate a from AC
%% for all j ∈ [1, n] do
%% send the message (Pd, τ, ready, C, a(j)) to Pj
handle_msg(State=#state{echoes=Echoes, id=Id, n=N, t=T, commitments=Commitments}, Sender, {echo, {Dealer, Ph, Commitment0, A}}) ->
    case dkg_commitment:verify_point(Commitment0, Sender, Id, A) of
        true ->
            Commitment = maps:get(Dealer, Commitments, Commitment0),
            case dkg_commitment:add_echo(Commitment, Sender, A) of
                {true, NewCommitment} ->
                    case dkg_commitment:num_echoes(NewCommitment) == ceil((N+T+1)/2) andalso
                         dkg_commitment:num_readies(NewCommitment) < (T+1) of
                        true ->
                            Subshares = dkg_commitment:interpolate(NewCommitment, echo, allnodes(N)),
                            Msgs = lists:map(fun(Node) ->
                                                     {unicast, Node, {ready, {Dealer, Ph, NewCommitment, lists:nth(Node+1, Subshares)}}}
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
            %% ct:pal("verify_point failed. Sender: ~p, Id: ~p", [Sender, Id]),
            {State, ok}
    end;


%% upon a message (Pd, τ, ready, C, α) from Pm (first time):
%% if verify-point(C, i, m, α) then
%% AC ← AC ∪ {(m, α)}; rC ← rC + 1
%% if rC = t + 1 and eC < n+t+1/2
%% Lagrange-interpolate a from AC
%% for all j ∈ [1, n] do
%% send the message (Pd, τ, ready, C, a(j)) to Pj
%% else if rC = n − t − f then
%% si ← a(0); output (Pd , τ, out, shared, C, si )
handle_msg(State=#state{readies=Readies, n=N, t=T, f=F, id=Id, commitments=Commitments}, Sender, {ready, {Dealer, Ph, Commitment0, A}}) ->
    %% ct:pal("Got ready, Sender: ~p, Id: ~p", [Sender, Id]),

    case dkg_commitment:verify_point(Commitment0, Sender, Id, A) of
        true ->
            Commitment = maps:get(Dealer, Commitments, Commitment0),
            %% ct:pal("verify_point success. Sender: ~p, Id: ~p", [Sender, Id]),
            case dkg_commitment:add_ready(Commitment, Sender, A) of
                {true, NewCommitment} ->
                    case dkg_commitment:num_readies(NewCommitment) == (T+1) andalso
                         dkg_commitment:num_echoes(NewCommitment) < ceil((N+T+1)/2) of
                        true ->
                            SubShares = dkg_commitment:interpolate(NewCommitment, ready, allnodes(N)),
                            Msgs = lists:map(fun(Node) ->
                                                     {unicast, Node, {ready, {Dealer, Ph, NewCommitment, lists:nth(Node+1, SubShares)}}}
                                             end, allnodes(N)),
                            NewState = State#state{readies=maps:put(Sender, true, Readies), commitments=maps:put(Dealer, NewCommitment, Commitments)},
                            {NewState, {send, Msgs}};
                        false ->
                            case dkg_commitment:num_readies(NewCommitment) == (N-T-F) of
                                true->
                                    [SubShare] = dkg_commitment:interpolate(NewCommitment, ready, []),
                                    NewState = State#state{readies=maps:put(Sender, true, Readies), commitments=maps:put(Dealer, NewCommitment, Commitments)},
                                    {NewState, {result, {Dealer, Ph, NewCommitment, SubShare}}};
                                false ->
                                    NewState = State#state{readies=maps:put(Sender, true, Readies), commitments=maps:put(Dealer, NewCommitment, Commitments)},
                                    {NewState, ok}
                            end
                    end;
                {false, _OldCommitment} ->
                    {State, ok}
            end;
        false ->
            %% ct:pal("verify_point failed. Sender: ~p, Id: ~p", [Sender, Id]),
            {State, ok}
    end;
handle_msg(State, _Sender, Msg) ->
    {State, {unhandled_msg, Msg}}.

allnodes(N) ->
    lists:seq(1, N).

%% XXX: THIS IS JUST FOR SANITY CHECK, REMOVE THIS
secret(State) ->
    State#state.secret.
