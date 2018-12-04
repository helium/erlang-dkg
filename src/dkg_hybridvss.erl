-module(dkg_hybridvss).

-include_lib("kernel/include/logger.hrl").

-export([init/8]).

-export([input/2,
         serialize/1,
         deserialize/2,
         status/1
        ]).

-export([handle_msg/3]).

-record(vss, {
          done = false :: boolean(),
          id :: pos_integer(),
          n :: pos_integer(),
          f :: pos_integer(),
          t :: pos_integer(),
          u :: erlang_pbc:element(),
          u2 :: erlang_pbc:element(),
          session :: session(),
          received_commitment = false :: boolean(),
          commitments = #{} :: #{binary() => dkg_commitment:commitment()},
          callback = false :: boolean()
         }).

-record(serialized_vss, {
          done = false :: boolean(),
          id :: pos_integer(),
          n :: pos_integer(),
          f :: pos_integer(),
          t :: pos_integer(),
          u :: binary(),
          u2 :: binary(),
          session :: session(),
          received_commitment = false :: boolean(),
          commitments = #{} :: #{binary() => dkg_commitment:serialized_commitment()},
          callback :: boolean()
         }).

-type session() :: {Dealer :: pos_integer(), Round :: pos_integer()}.
-type send_msg() :: {unicast, pos_integer(), {send, {session(), dkg_commitmentmatrix:serialized_matrix(), dkg_polynomial:polynomial()}}}.
-type echo_msg() :: {unicast, pos_integer(), {echo, {session(), dkg_commitmentmatrix:serialized_matrix(), binary()}}}.
-type ready_msg() :: {unicast, pos_integer(), {ready, {session(), dkg_commitmentmatrix:serialized_matrix(), binary()}}}.
-type result() :: {result, {session(), dkg_commitment:commitment(), [erlang_pbc:element()]}}.
-type vss() :: #vss{}.
-type serialized_vss() :: #serialized_vss{}.

-export_type([vss/0, session/0, serialized_vss/0]).

-spec init(Id :: pos_integer(), N :: pos_integer(), F :: pos_integer(), T :: pos_integer(), erlang_pbc:element(), erlang_pbc:element(), session(), boolean())-> vss().
init(Id, N, F, T, G1, G2, Session, Callback) ->
    true = N >= (3*T + 2*F + 1),
    #vss{id=Id,
         n=N,
         f=F,
         t=T,
         session=Session,
         u=G1,
         u2=G2,
         callback=Callback}.

%% upon a message (Pd, τ, in, share, s): /* only Pd */
%%     choose a symmetric bivariate polynomial φ(x,y) = ∑tj,l=0 φjl x^j y^l ∈R Zp[x,y] and φ00 = s
%%     C ←{Cjl } t j,l=0 where Cjl = gφ^jl for j,l ∈[0,t]
%%     for all j ∈ [1,n] do
%%         aj(y) ← φ(j,y); send the message (Pd, τ, send, C, aj) to Pj
-spec input(VSS :: vss(), Secret :: erlang_pbc:element()) -> {vss(), {send, [send_msg()]} | ok}.
input(VSS = #vss{session=Session={Dealer,_}, id=Id, u=U, u2=U2, t=T, n=N, callback=true}, Secret) when Dealer == Id ->
    BiPoly = dkg_bipolynomial:generate(U2, T, Secret),
    Commitment = dkg_commitment:new(dkg_util:allnodes(N), U2, BiPoly),
    %% only serialize this once, not in the loop below
    SerializedCommitmentMatrix = dkg_commitmentmatrix:serialize(dkg_commitment:matrix(Commitment)),

    Msgs = lists:map(fun(Node) ->
                             NodeZr = erlang_pbc:element_set(erlang_pbc:element_new('Zr', U), Node),
                             dkg_polynomial:serialize(dkg_bipolynomial:evaluate(BiPoly, NodeZr))
                     end, dkg_util:allnodes(N)),
    {store_commitment(Commitment, VSS), {send, [{callback, {send, {Session, SerializedCommitmentMatrix, Msgs}}}]}};
input(VSS = #vss{session=Session={Dealer,_}, id=Id, u=U, u2=U2, t=T, n=N}, Secret) when Dealer == Id ->
    BiPoly = dkg_bipolynomial:generate(U2, T, Secret),
    Commitment = dkg_commitment:new(dkg_util:allnodes(N), U2, BiPoly),
    %% only serialize this once, not in the loop below
    SerializedCommitmentMatrix = dkg_commitmentmatrix:serialize(dkg_commitment:matrix(Commitment)),

    Msgs = lists:map(fun(Node) ->
                             NodeZr = erlang_pbc:element_set(erlang_pbc:element_new('Zr', U), Node),
                             Aj = dkg_polynomial:serialize(dkg_bipolynomial:evaluate(BiPoly, NodeZr)),
                             {unicast, Node, {send, {Session, SerializedCommitmentMatrix, Aj}}}
                     end, dkg_util:allnodes(N)),
    {store_commitment(Commitment, VSS), {send, Msgs}};
input(VSS, _Secret) ->
    {VSS, ok}.

%% upon a message (Pd, τ, send, C, a) from Pd (first time):
%%     if verify-poly(C, i, a) then
%%         for all j ∈ [1, n] do
%%             send the message (Pd , τ, echo, C, a(j)) to Pj
-spec handle_msg(vss(), pos_integer(), send_msg() | echo_msg() | ready_msg()) -> {vss(), {send, [echo_msg() | ready_msg()]} | ok | ignore | result()}.
handle_msg(VSS=#vss{n=N, id=Id, session=Session, received_commitment=false, callback=true}, Sender, {send, {Session = {Sender, _}, SerializedCommitmentMatrix0, SA}}) ->
    Commitment = get_commitment(SerializedCommitmentMatrix0, VSS),
    A = dkg_polynomial:deserialize(SA, VSS#vss.u),
    case dkg_commitment:verify_poly(Commitment, Id, A) of
        true ->
            Msgs = lists:map(fun(Node) ->
                                     erlang_pbc:element_to_binary(dkg_polynomial:evaluate(A, Node))
                             end, dkg_util:allnodes(N)),
            {store_commitment(Commitment, VSS#vss{received_commitment=true}), {send, [{callback, {echo, {Session, SerializedCommitmentMatrix0, Msgs}}}]}};
        false ->
            {VSS, ok}
    end;
handle_msg(VSS=#vss{n=N, id=Id, session=Session, received_commitment=false}, Sender, {send, {Session = {Sender, _}, SerializedCommitmentMatrix0, SA}}) ->
    Commitment = get_commitment(SerializedCommitmentMatrix0, VSS),
    A = dkg_polynomial:deserialize(SA, VSS#vss.u),
    case dkg_commitment:verify_poly(Commitment, Id, A) of
        true ->
            Msgs = lists:map(fun(Node) ->
                                     {unicast, Node, {echo, {Session, SerializedCommitmentMatrix0, erlang_pbc:element_to_binary(dkg_polynomial:evaluate(A, Node))}}}
                             end, dkg_util:allnodes(N)),
            {store_commitment(Commitment, VSS#vss{received_commitment=true}), {send, Msgs}};
        false ->
            {VSS, ok}
    end;
handle_msg(VSS, _Sender, {send, {_Session, _Commitment, _A}}) ->
    %% already received a commitment, or it's not from the dealer; ignore this one
    {VSS, ignore};

%% upon a message (Pd, τ, echo, C, α) from Pm (first time):
%%     if verify-point(C, i, m, α) then
%%         AC ← AC ∪ {(m, α)}; eC ← eC + 1
%%         if eC = d n+t+1/2 and rC < t + 1 then
%%             Lagrange-interpolate a from AC
%%             for all j ∈ [1, n] do
%%                  send the message (Pd, τ, ready, C, a(j)) to Pj
handle_msg(VSS=#vss{id=Id, n=N, t=T, session=Session, done=false, callback=true}, Sender, {echo, {Session, SerializedCommitmentMatrix0, SA}}) ->
    Commitment = get_commitment(SerializedCommitmentMatrix0, VSS),
    A = erlang_pbc:binary_to_element(VSS#vss.u, SA),
    case dkg_commitment:verify_point(Commitment, Sender, Id, A) of
        true ->
            case dkg_commitment:add_echo(Commitment, Sender, A) of
                {true, NewCommitment} ->
                    case dkg_commitment:num_echoes(NewCommitment) == ceil((N+T+1)/2) andalso
                         dkg_commitment:num_readies(NewCommitment) < (T+1) of
                        true ->
                            Subshares = dkg_commitment:interpolate(NewCommitment, echo, dkg_util:allnodes(N)),
                            Msgs = lists:map(fun(Node) ->
                                                     erlang_pbc:element_to_binary(lists:nth(Node+1, Subshares))
                                             end, dkg_util:allnodes(N)),
                            {store_commitment(NewCommitment, VSS), {send, [{callback, {ready, {Session, SerializedCommitmentMatrix0, Msgs}}}]}};
                        false ->
                            {store_commitment(NewCommitment, VSS), ok}
                    end;
                {false, OldCommitment} ->
                    {store_commitment(OldCommitment, VSS), ok}
            end;
        false ->
            {VSS, ok}
    end;
handle_msg(VSS=#vss{id=Id, n=N, t=T, session=Session, done=false}, Sender, {echo, {Session, SerializedCommitmentMatrix0, SA}}) ->
    Commitment = get_commitment(SerializedCommitmentMatrix0, VSS),
    A = erlang_pbc:binary_to_element(VSS#vss.u, SA),
    case dkg_commitment:verify_point(Commitment, Sender, Id, A) of
        true ->
            case dkg_commitment:add_echo(Commitment, Sender, A) of
                {true, NewCommitment} ->
                    case Id of
                        1 ->
                            ?LOG_INFO
("sender ~p echoes ~p readies ~p",
                                   [Sender,
                                    dkg_commitment:num_echoes(NewCommitment),
                                    dkg_commitment:num_readies(NewCommitment)]);
                        _ -> meh
                    end,
                    case dkg_commitment:num_echoes(NewCommitment) == ceil((N+T+1)/2) andalso
                         dkg_commitment:num_readies(NewCommitment) < (T+1) of
                        true ->
                            Subshares = dkg_commitment:interpolate(NewCommitment, echo, dkg_util:allnodes(N)),
                            Msgs = lists:map(fun(Node) ->
                                                     {unicast, Node, {ready, {Session, SerializedCommitmentMatrix0, erlang_pbc:element_to_binary(lists:nth(Node+1, Subshares))}}}
                                             end, dkg_util:allnodes(N)),
                            {store_commitment(NewCommitment, VSS), {send, Msgs}};
                        false ->
                            {store_commitment(NewCommitment, VSS), ok}
                    end;
                {false, OldCommitment} ->
                    {store_commitment(OldCommitment, VSS), ok}
            end;
        false ->
            {VSS, ok}
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
handle_msg(VSS=#vss{n=N, t=T, f=F, id=Id, done=false, callback=true}, Sender, {ready, {Session, SerializedCommitmentMatrix0, SA}}=_ReadyMsg) ->
    Commitment = get_commitment(SerializedCommitmentMatrix0, VSS),
    A = erlang_pbc:binary_to_element(VSS#vss.u, SA),
    case dkg_commitment:verify_point(Commitment, Sender, Id, A) of
        true ->
            %% TODO the ready should be signed, so we should store the signature in the commitment as well
            case dkg_commitment:add_ready(Commitment, Sender, A) of
                {true, NewCommitment} ->
                    case dkg_commitment:num_readies(NewCommitment) == (T+1) andalso
                         dkg_commitment:num_echoes(NewCommitment) < ceil((N+T+1)/2) of
                        true ->
                            SubShares = dkg_commitment:interpolate(NewCommitment, ready, dkg_util:allnodes(N)),
                            Msgs = lists:map(fun(Node) ->
                                                     erlang_pbc:element_to_binary(lists:nth(Node+1, SubShares))
                                             end, dkg_util:allnodes(N)),
                            NewVSS = store_commitment(NewCommitment, VSS),
                            {NewVSS, {send, [{callback, {ready, {Session, SerializedCommitmentMatrix0, Msgs}}}]}};
                        false ->
                            case dkg_commitment:num_readies(NewCommitment) == (N-T-F) of
                                true->
                                    [SubShare] = dkg_commitment:interpolate(NewCommitment, ready, []),
                                    %% clear the commitments out of our state and return the winning one
                                    {VSS#vss{done=true, commitments=#{}}, {result, {Session, NewCommitment, SubShare}}};
                                false ->
                                    NewVSS = store_commitment(NewCommitment, VSS),
                                    {NewVSS, ok}
                            end
                    end;
                {false, OldCommitment} ->
                    {store_commitment(OldCommitment, VSS), ok}
            end;
        false ->
            {VSS, ok}
    end;
handle_msg(VSS=#vss{n=N, t=T, f=F, id=Id, done=false}, Sender, {ready, {Session, SerializedCommitmentMatrix0, SA}}=_ReadyMsg) ->
    Commitment = get_commitment(SerializedCommitmentMatrix0, VSS),
    A = erlang_pbc:binary_to_element(VSS#vss.u, SA),
    case dkg_commitment:verify_point(Commitment, Sender, Id, A) of
        true ->
            %% TODO the ready should be signed, so we should store the signature in the commitment as well
            case dkg_commitment:add_ready(Commitment, Sender, A) of
                {true, NewCommitment} ->
                    case Id of
                        1 ->
                            ?LOG_INFO("GOT READY ~p ~p sender ~p echoes ~p readies ~p ?= ~p",
                                   [(T+1), ceil((N+T+1)/2), Sender,
                                    dkg_commitment:num_echoes(NewCommitment),
                                    dkg_commitment:num_readies(NewCommitment), (N-T-F)]);
                        _ -> meh
                    end,
                    case dkg_commitment:num_readies(NewCommitment) == (T+1) andalso
                         dkg_commitment:num_echoes(NewCommitment) < ceil((N+T+1)/2) of
                        true ->
                            SubShares = dkg_commitment:interpolate(NewCommitment, ready, dkg_util:allnodes(N)),
                            Msgs = lists:map(fun(Node) ->
                                                     {unicast, Node, {ready, {Session, SerializedCommitmentMatrix0, erlang_pbc:element_to_binary(lists:nth(Node+1, SubShares))}}}
                                             end, dkg_util:allnodes(N)),
                            NewVSS = store_commitment(NewCommitment, VSS),
                            {NewVSS, {send, Msgs}};
                        false ->
                            case dkg_commitment:num_readies(NewCommitment) == (N-T-F) of
                                true->
                                    [SubShare] = dkg_commitment:interpolate(NewCommitment, ready, []),
                                    %% clear the commitments out of our state and return the winning one
                                    {VSS#vss{done=true, commitments=#{}}, {result, {Session, NewCommitment, SubShare}}};
                                false ->
                                    NewVSS = store_commitment(NewCommitment, VSS),
                                    {NewVSS, ok}
                            end
                    end;
                {false, OldCommitment} ->
                    {store_commitment(OldCommitment, VSS), ok}
            end;
        false ->
            {VSS, ok}
    end;
handle_msg(VSS, _Sender, _Msg) ->
    %% we're likely done here, so there's no point in processing more messages
    {VSS, ignore}.

-spec serialize(vss()) -> serialized_vss().
serialize(#vss{id=Id,
               n=N,
               f=F,
               t=T,
               u=U,
               u2=U2,
               done=Done,
               session=Session,
               received_commitment=ReceivedCommitment,
               commitments=Commitments,
               callback=Callback}) ->
    #serialized_vss{id=Id,
                    n=N,
                    f=F,
                    t=T,
                    u=erlang_pbc:element_to_binary(U),
                    u2=erlang_pbc:element_to_binary(U2),
                    done=Done,
                    session=Session,
                    received_commitment=ReceivedCommitment,
                    commitments=maps:map(fun(_, V) -> dkg_commitment:serialize(V) end, Commitments),
                    callback=Callback}.

-spec deserialize(serialized_vss(), erlang_pbc:element()) -> vss().
deserialize(#serialized_vss{id=Id,
                            n=N,
                            f=F,
                            t=T,
                            u=SerializedU,
                            u2=SerializedU2,
                            done=Done,
                            session=Session,
                            received_commitment=ReceivedCommitment,
                            commitments=SerializedCommitments,
                            callback=Callback}, Element) ->
    #vss{id=Id,
         n=N,
         f=F,
         t=T,
         u=erlang_pbc:binary_to_element(Element, SerializedU),
         u2=erlang_pbc:binary_to_element(Element, SerializedU2),
         done=Done,
         session=Session,
         received_commitment=ReceivedCommitment,
         commitments=maps:map(fun(_, V) -> dkg_commitment:deserialize(V, Element) end, SerializedCommitments),
         callback=Callback}.

-spec status(vss()) -> map().
status(VSS) ->
    #{id => VSS#vss.id,
      session => VSS#vss.session,
      received_commitment => VSS#vss.received_commitment,
      commitments => maps:map(fun(_, C) ->
                                      #{
                                       num_echoes => dkg_commitment:num_echoes(C),
                                       num_readies => dkg_commitment:num_readies(C),
                                       echoes => maps:keys(dkg_commitment:echoes(C)),
                                       readies => maps:keys(dkg_commitment:readies(C))
                                      }
                              end, VSS#vss.commitments),
      done => VSS#vss.done
     }.

get_commitment(SerializedMatrix, VSS = #vss{n=N, t=T, u2=G2}) ->
    Key = erlang:phash2(SerializedMatrix),
    case maps:find(Key, VSS#vss.commitments) of
        {ok, Commitment} ->
            Commitment;
        error ->
            Commitment = dkg_commitment:new(dkg_util:allnodes(N), G2, T),
            dkg_commitment:set_matrix(Commitment, dkg_commitmentmatrix:deserialize(SerializedMatrix, G2))
    end.

store_commitment(Commitment, VSS) ->
    Key = erlang:phash2(dkg_commitment:binary_matrix(Commitment)),
    VSS#vss{commitments=maps:put(Key, Commitment, VSS#vss.commitments)}.
