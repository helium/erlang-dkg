-module(dkg_hybridvss).

-export([init/6, init/7]).

-export([input/2, commitment/1]).

-export([handle_msg/3]).

-type session() :: {Dealer :: pos_integer(), Round :: pos_integer()}.

-record(state, {
            id :: pos_integer(),
            n :: pos_integer(),
            f :: pos_integer(),
            t :: pos_integer(),
            u :: erlang_pbc:element(),
            u2 :: erlang_pbc:element(),
            session :: session(),
            sent_echo = false :: boolean(),
            received_commitment = false :: boolean(),
            echoes = #{} :: map(),
            readies = #{} :: map(),
            commitment :: dkg_commitment:commitment()
         }).

-type vss() :: #state{}.
-type send_msg() :: {unicast, pos_integer(), {send, {session(), dkg_commitmentmatrix:serialized_matrix(), dkg_polynomial:polynomial()}}}.
-type echo_msg() :: {unicast, pos_integer(), {echo, {session(), dkg_commitmentmatrix:serialized_matrix(), binary()}}}.
-type ready_msg() :: {unicast, pos_integer(), {ready, {session(), dkg_commitmentmatrix:serialized_matrix(), binary()}}}.
-type result() :: {result, {session(), dkg_commitment:commitment(), [erlang_pbc:element()]}}.

-export_type([vss/0, session/0]).

-spec init(Id :: pos_integer(), N :: pos_integer(), F :: pos_integer(), T :: pos_integer(), erlang_pbc:element(), session()) -> vss().
init(Id, N, F, T, Generator, Session) ->
    case erlang_pbc:pairing_is_symmetric(Generator) of
        true ->
            init(Id, N, F, T, Generator, Generator, Session);
        false ->
            erlang:error(pairing_not_symmetric)
    end.

-spec init(Id :: pos_integer(), N :: pos_integer(), F :: pos_integer(), T :: pos_integer(), erlang_pbc:element(), erlang_pbc:element(), session()) -> vss().
init(Id, N, F, T, Generator, G2, Session) ->
    #state{id=Id,
           n=N,
           f=F,
           t=T,
           session=Session,
           commitment=dkg_commitment:new(dkg_util:allnodes(N), G2, T),
           u=Generator,
           u2=G2}.

%% upon a message (Pd, τ, in, share, s): /* only Pd */
%%     choose a symmetric bivariate polynomial φ(x,y) = ∑tj,l=0 φjl x^j y^l ∈R Zp[x,y] and φ00 = s
%%     C ←{Cjl } t j,l=0 where Cjl = gφ^jl for j,l ∈[0,t]
%%     for all j ∈ [1,n] do
%%         aj(y) ← φ(j,y); send the message (Pd, τ, send, C, aj) to Pj
-spec input(State :: vss(), Secret :: erlang_pbc:element()) -> {vss(), {send, [send_msg()]} | ok}.
input(State = #state{session=Session={Dealer,_}, id=Id, u=U, u2=U2, t=T, n=N}, Secret) when Dealer == Id ->
    BiPoly = dkg_bipolynomial:generate(U2, T, Secret),
    Commitment = dkg_commitment:new(dkg_util:allnodes(N), U2, BiPoly),

    Msgs = lists:map(fun(Node) ->
                             NodeZr = erlang_pbc:element_set(erlang_pbc:element_new('Zr', U), Node),
                             Aj = dkg_bipolynomial:evaluate(BiPoly, NodeZr),
                             {unicast, Node, {send, {Session, dkg_commitmentmatrix:serialize(dkg_commitment:matrix(Commitment)), Aj}}}
                     end, dkg_util:allnodes(N)),
    NewState = State#state{commitment=Commitment, received_commitment=true},
    {NewState, {send, Msgs}};
input(State, _Secret) ->
    {State, ok}.

%% upon a message (Pd, τ, send, C, a) from Pd (first time):
%%     if verify-poly(C, i, a) then
%%         for all j ∈ [1, n] do
%%             send the message (Pd , τ, echo, C, a(j)) to Pj
-spec handle_msg(vss(), pos_integer(), send_msg() | echo_msg() | ready_msg()) -> {vss(), {send, [echo_msg() | ready_msg()]} | ok | result()}.
handle_msg(State=#state{n=N, u=U, id=Id, session=Session, sent_echo=false}, Sender, {send, {Session = {Sender, _}, SerializedCommitmentMatrix0, A}}) ->
    CommitmentMatrix0 = dkg_commitmentmatrix:deserialize(SerializedCommitmentMatrix0, U),
    case dkg_commitmentmatrix:verify_poly(State#state.u2, CommitmentMatrix0, Id, A) of
        true ->
            %% always update the commitment matrix if we get it straight from the dealer
            Commitment = dkg_commitment:set_matrix(State#state.commitment, CommitmentMatrix0),
            Msgs = lists:map(fun(Node) ->
                                     {unicast, Node, {echo, {Session, SerializedCommitmentMatrix0, erlang_pbc:element_to_binary(dkg_polynomial:evaluate(A, Node))}}}
                             end, dkg_util:allnodes(N)),
            {State#state{sent_echo=true, commitment=Commitment, received_commitment=true}, {send, Msgs}};
        false ->
            {State, ok}
    end;
handle_msg(State, _Sender, {send, {_Session, _Commitment, _A}}) ->
    %% already received a commitment, or it's not from the dealer; ignore this one
    {State, ok};

%% upon a message (Pd, τ, echo, C, α) from Pm (first time):
%%     if verify-point(C, i, m, α) then
%%         AC ← AC ∪ {(m, α)}; eC ← eC + 1
%%         if eC = d n+t+1/2 and rC < t + 1 then
%%             Lagrange-interpolate a from AC
%%             for all j ∈ [1, n] do
%%                  send the message (Pd, τ, ready, C, a(j)) to Pj
handle_msg(State=#state{echoes=Echoes, id=Id, n=N, t=T, session=Session}, Sender, {echo, {Session, SerializedCommitmentMatrix0, SA}}) ->
    CommitmentMatrix0 = dkg_commitmentmatrix:deserialize(SerializedCommitmentMatrix0, State#state.u),
    A = erlang_pbc:binary_to_element(State#state.u, SA),
    case dkg_commitmentmatrix:verify_point(State#state.u2, CommitmentMatrix0, Sender, Id, A) of
        true ->
            Commitment = case State#state.received_commitment of
                             true -> State#state.commitment;
                             false -> dkg_commitment:set_matrix(State#state.commitment, CommitmentMatrix0)
                         end,
            case dkg_commitment:add_echo(Commitment, Sender, A) of
                {true, NewCommitment} ->
                    case dkg_commitment:num_echoes(NewCommitment) == ceil((N+T+1)/2) andalso
                         dkg_commitment:num_readies(NewCommitment) < (T+1) of
                        true ->
                            Subshares = dkg_commitment:interpolate(NewCommitment, echo, dkg_util:allnodes(N)),
                            Msgs = lists:map(fun(Node) ->
                                                     {unicast, Node, {ready, {Session, SerializedCommitmentMatrix0, erlang_pbc:element_to_binary(lists:nth(Node+1, Subshares))}}}
                                             end, dkg_util:allnodes(N)),
                            NewState = State#state{echoes=maps:put(Sender, true, Echoes), commitment=NewCommitment, received_commitment=true},
                            {NewState, {send, Msgs}};
                        false ->
                            NewState = State#state{echoes=maps:put(Sender, true, Echoes), commitment=NewCommitment, received_commitment=true},
                            {NewState, ok}
                    end;
                {false, OldCommitment} ->
                    {State#state{commitment=OldCommitment, received_commitment=true}, ok}
            end;
        false ->
            {State, ok}
    end;

%% upon a message (Pd, τ, ready, C, α) from Pm (first time):
%%     if verify-point(C, i, m, α) then
%%         AC ← AC ∪ {(m, α)}; rC ← rC + 1
%%         if rC = t + 1 and eC < n+t+1/2
%%             Lagrange-interpolate a from AC
%%             for all j ∈ [1, n] do
%%                 send the message (Pd, τ, ready, C, a(j)) to Pj
%%     else if rC = n − t − f then
%%         si ← a(0); output (Pd , τ, out, shared, C, si )
handle_msg(State=#state{readies=Readies, n=N, t=T, f=F, id=Id, commitment=Commitment}, Sender, {ready, {Session, SerializedCommitmentMatrix0, SA}}) ->
    CommitmentMatrix0 = dkg_commitmentmatrix:deserialize(SerializedCommitmentMatrix0, State#state.u),
    A = erlang_pbc:binary_to_element(State#state.u, SA),
    case dkg_commitmentmatrix:verify_point(State#state.u2, CommitmentMatrix0, Sender, Id, A) of
        true ->
            Commitment = case State#state.received_commitment of
                             true -> State#state.commitment;
                             false -> dkg_commitment:set_matrix(State#state.commitment, CommitmentMatrix0)
                         end,
            case dkg_commitment:add_ready(Commitment, Sender, A) of
                {true, NewCommitment} ->
                    case dkg_commitment:num_readies(NewCommitment) == (T+1) andalso
                         dkg_commitment:num_echoes(NewCommitment) < ceil((N+T+1)/2) of
                        true ->
                            SubShares = dkg_commitment:interpolate(NewCommitment, ready, dkg_util:allnodes(N)),
                            Msgs = lists:map(fun(Node) ->
                                                     {unicast, Node, {ready, {Session, SerializedCommitmentMatrix0, erlang_pbc:element_to_binary(lists:nth(Node+1, SubShares))}}}
                                             end, dkg_util:allnodes(N)),
                            NewState = State#state{readies=maps:put(Sender, true, Readies), commitment=NewCommitment, received_commitment=true},
                            {NewState, {send, Msgs}};
                        false ->
                            case dkg_commitment:num_readies(NewCommitment) == (N-T-F) of
                                true->
                                    [SubShare] = dkg_commitment:interpolate(NewCommitment, ready, []),
                                    NewState = State#state{readies=maps:put(Sender, true, Readies), commitment=NewCommitment, received_commitment=true},
                                    {NewState, {result, {Session, Commitment, SubShare}}};
                                false ->
                                    NewState = State#state{readies=maps:put(Sender, true, Readies), commitment=NewCommitment, received_commitment=true},
                                    {NewState, ok}
                            end
                    end;
                {false, OldCommitment} ->
                    {State#state{commitment=OldCommitment, received_commitment=true}, ok}
            end;
        false ->
            {State, ok}
    end;
handle_msg(State, _Sender, Msg) ->
    {State, {unhandled_msg, Msg}}.

commitment(State) ->
    State#state.commitment.
