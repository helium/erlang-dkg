-module(dkg_hybridvss).

-export([init/6]).

-export([input/2]).

-export([handle_msg/3]).


-type session() :: {Dealer :: pos_integer(), Round :: pos_integer()}.

-record(state, {
            id :: pos_integer(),
            n :: pos_integer(),
            f :: pos_integer(),
            t :: pos_integer(),
            u :: erlang_pbc:element(),
            session :: session(),
            sent_echo = false :: boolean(),
            echoes = #{} :: map(),
            readies = #{} :: map(),
            commitment :: undefined | dkg_commitment:commitment()
         }).

-spec init(Id :: pos_integer(), N :: pos_integer(), F :: pos_integer(), T :: pos_integer(), erlang_pbc:element(), session()) -> #state{}.
init(Id, N, F, T, Generator, Session) ->
    #state{id=Id,
           n=N,
           f=F,
           t=T,
           commitment = dkg_commitment:new(allnodes(N), Generator, T),
           session=Session,
           u=Generator}.

%% upon a message (Pd, τ, in, share, s): /* only Pd */
%%     choose a symmetric bivariate polynomial φ(x,y) = ∑tj,l=0 φjl x^j y^l ∈R Zp[x,y] and φ00 = s
%%     C ←{Cjl } t j,l=0 where Cjl = gφ^jl for j,l ∈[0,t]
%%     for all j ∈ [1,n] do
%%         aj(y) ← φ(j,y); send the message (Pd, τ, send, C, aj) to Pj
input(State = #state{session=Session={Dealer,_}, id=Id, u=U, t=T, n=N}, Secret) when Dealer == Id ->
    BiPoly = dkg_bipolynomial:generate(U, T, Secret),
    Commitment = dkg_commitment:new(allnodes(N), U, BiPoly),

    Msgs = lists:map(fun(Node) ->
                             NodeZr = erlang_pbc:element_set(erlang_pbc:element_new('Zr', U), Node),
                             Aj = dkg_bipolynomial:apply(BiPoly, NodeZr),
                             {unicast, Node, {send, {Session, Commitment, Aj}}}
                     end, allnodes(N)),
    NewState = State#state{commitment=Commitment},
    {NewState, {send, Msgs}};
input(_State, _Secret) ->
    {error, not_dealer}.

%% upon a message (Pd, τ, send, C, a) from Pd (first time):
%%     if verify-poly(C, i, a) then
%%         for all j ∈ [1, n] do
%%             send the message (Pd , τ, echo, C, a(j)) to Pj
handle_msg(State=#state{n=N, session=Session, sent_echo=false}, Sender, {send, {Session = {Sender, _}, Commitment0, A}}) ->
    case dkg_commitment:verify_poly(Commitment0, State#state.id, A) of
        true ->
            Msgs = lists:map(fun(Node) ->
                                     {unicast, Node, {echo, {Session, Commitment0, dkg_polynomial:apply(A, Node)}}}
                             end, allnodes(N)),
            {State#state{sent_echo=true}, {send, Msgs}};
        false ->
            {error, bad_commitment}
    end;
handle_msg(State, _Sender, {send, {_Session, _Commitment, _A}}) ->
    %% already received a commitment, or it's not from the dealer; ignore this one
    {State, {error, already_received_commitment}};

%% upon a message (Pd, τ, echo, C, α) from Pm (first time):
%%     if verify-point(C, i, m, α) then
%%         AC ← AC ∪ {(m, α)}; eC ← eC + 1
%%         if eC = d n+t+1/2 and rC < t + 1 then
%%             Lagrange-interpolate a from AC
%%             for all j ∈ [1, n] do
%%                  send the message (Pd, τ, ready, C, a(j)) to Pj
handle_msg(State=#state{echoes=Echoes, id=Id, n=N, t=T, commitment=Commitment, session=Session}, Sender, {echo, {Session, Commitment0, A}}) ->
    case dkg_commitment:verify_point(Commitment0, Sender, Id, A) of
        true ->
            case dkg_commitment:add_echo(Commitment, Sender, A) of
                {true, NewCommitment} ->
                    case dkg_commitment:num_echoes(NewCommitment) == ceil((N+T+1)/2) andalso
                         dkg_commitment:num_readies(NewCommitment) < (T+1) of
                        true ->
                            Subshares = dkg_commitment:interpolate(NewCommitment, echo, allnodes(N)),
                            Msgs = lists:map(fun(Node) ->
                                                     {unicast, Node, {ready, {Session, Commitment0, lists:nth(Node+1, Subshares)}}}
                                             end, allnodes(N)),
                            NewState = State#state{echoes=maps:put(Sender, true, Echoes), commitment=NewCommitment},
                            {NewState, {send, Msgs}};
                        false ->
                            NewState = State#state{echoes=maps:put(Sender, true, Echoes), commitment=NewCommitment},
                            {NewState, ok}
                    end;
                {false, _OldCommitment} ->
                    {State, ok}
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
handle_msg(State=#state{readies=Readies, n=N, t=T, f=F, id=Id, commitment=Commitment}, Sender, {ready, {Session, Commitment0, A}}) ->
    case dkg_commitment:verify_point(Commitment0, Sender, Id, A) of
        true ->
            case dkg_commitment:add_ready(Commitment, Sender, A) of
                {true, NewCommitment} ->
                    case dkg_commitment:num_readies(NewCommitment) == (T+1) andalso
                         dkg_commitment:num_echoes(NewCommitment) < ceil((N+T+1)/2) of
                        true ->
                            SubShares = dkg_commitment:interpolate(NewCommitment, ready, allnodes(N)),
                            Msgs = lists:map(fun(Node) ->
                                                     {unicast, Node, {ready, {Session, Commitment0, lists:nth(Node+1, SubShares)}}}
                                             end, allnodes(N)),
                            NewState = State#state{readies=maps:put(Sender, true, Readies), commitment=NewCommitment},
                            {NewState, {send, Msgs}};
                        false ->
                            case dkg_commitment:num_readies(NewCommitment) == (N-T-F) of
                                true->
                                    [SubShare] = dkg_commitment:interpolate(NewCommitment, ready, []),
                                    NewState = State#state{readies=maps:put(Sender, true, Readies), commitment=NewCommitment},
                                    {NewState, {result, {Session, Commitment0, SubShare}}};
                                false ->
                                    NewState = State#state{readies=maps:put(Sender, true, Readies), commitment=NewCommitment},
                                    {NewState, ok}
                            end
                    end;
                {false, _OldCommitment} ->
                    {State, ok}
            end;
        false ->
            {State, ok}
    end;
handle_msg(State, _Sender, Msg) ->
    {State, {unhandled_msg, Msg}}.

allnodes(N) ->
    lists:seq(1, N).
