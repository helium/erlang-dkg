-module(dkg_hybridvss).

-export([init/10]).

-export([input/2,
         serialize/1,
         deserialize/4,
         status/1,
         verify_proof/3
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
          callback = false :: boolean(),
          sent_echoes = [] :: [integer()],
          sent_readies = [] :: [integer()],
          signfun :: signfun(),
          verifyfun :: verifyfun()
         }).

%% Note that the 'Round' here is assumed to be some unique combination of members and some strictly increasing counter(s) or nonce.
%% For example, something like the SHA of the public keys of all the members and some global DKG counter.
%% The counter/nonce should NOT repeat under any circumstances or ready messages may be reused to forge subsequent round results.
-type session() :: {Dealer :: pos_integer(), Round :: binary()}.
-type send_msg() :: {unicast, pos_integer(), {send, {session(), dkg_commitmentmatrix:serialized_matrix(), dkg_polynomial:polynomial()}}}.
-type echo_msg() :: {unicast, pos_integer(), {echo, {session(), dkg_commitmentmatrix:serialized_matrix(), binary()}}}.
-type ready_msg() :: {unicast, pos_integer(), {ready, {session(), dkg_commitmentmatrix:serialized_matrix(), binary()}}}.
-type result() :: {result, {session(), dkg_commitment:commitment(), [erlang_pbc:element()]}}.
-type vss() :: #vss{}.
-type signfun() :: fun((Msg :: binary()) -> Signature :: binary()).
-type verifyfun() :: fun((Sender :: pos_integer(), Msg :: binary(), Signature :: binary()) -> boolean()).

-export_type([vss/0, session/0]).

-spec init(Id :: pos_integer(), N :: pos_integer(), F :: pos_integer(), T :: pos_integer(),
           G1 :: erlang_pbc:element(), G2 :: erlang_pbc:element(),
           Session :: session(), Callback :: boolean(),
           SignFun :: signfun(), VerifyFun :: verifyfun())-> vss().
init(Id, N, F, T, G1, G2, Session, Callback, SignFun, VerifyFun) ->
    true = N >= (3*T + 2*F + 1),
    #vss{id=Id,
         n=N,
         f=F,
         t=T,
         session=Session,
         u=G1,
         u2=G2,
         callback=Callback,
         signfun = SignFun,
         verifyfun = VerifyFun}.

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
            {store_commitment(Commitment, echo, VSS#vss{received_commitment=true}), {send, [{callback, {echo, {Session, SerializedCommitmentMatrix0, Msgs}}}]}};
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
            {store_commitment(Commitment, echo, VSS#vss{received_commitment=true}), {send, Msgs}};
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
handle_msg(VSS=#vss{id=Id, n=N, t=T, session=Session, done=false, callback=CB}, Sender, {echo, {Session, SerializedCommitmentMatrix0, SA}}) ->
    Commitment = get_commitment(SerializedCommitmentMatrix0, VSS),
    A = erlang_pbc:binary_to_element(VSS#vss.u, SA),
    case dkg_commitment:verify_point(Commitment, Sender, Id, A) of
        true ->
            case dkg_commitment:add_echo(Commitment, Sender, A) of
                {true, NewCommitment} ->
                    case dkg_commitment:num_echoes(NewCommitment) == ceil((N+T+1)/2) andalso
                         dkg_commitment:num_readies(NewCommitment) < (T+1) of
                        true when CB == true ->
                            Subshares = dkg_commitment:interpolate(NewCommitment, echo, dkg_util:allnodes(N)),
                            ReadyProof = construct_proof(VSS),
                            Msgs = lists:map(fun(Node) ->
                                                     erlang_pbc:element_to_binary(lists:nth(Node+1, Subshares))
                                             end, dkg_util:allnodes(N)),
                            {store_commitment(NewCommitment, ready, VSS), {send, [{callback, {ready, {Session, SerializedCommitmentMatrix0, Msgs, ReadyProof}}}]}};
                        true ->
                            %% not in callback mode
                            Subshares = dkg_commitment:interpolate(NewCommitment, echo, dkg_util:allnodes(N)),
                            ReadyProof = construct_proof(VSS),
                            Msgs = lists:map(fun(Node) ->
                                                     {unicast, Node, {ready, {Session, SerializedCommitmentMatrix0, erlang_pbc:element_to_binary(lists:nth(Node+1, Subshares)), ReadyProof}}}
                                             end, dkg_util:allnodes(N)),
                            {store_commitment(NewCommitment, ready, VSS), {send, Msgs}};
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
handle_msg(VSS=#vss{n=N, t=T, f=F, id=Id, done=false, callback=CB}, Sender, {ready, {Session, SerializedCommitmentMatrix0, SA, ReadyProof}}=_ReadyMsg) ->
    Commitment = get_commitment(SerializedCommitmentMatrix0, VSS),
    A = erlang_pbc:binary_to_element(VSS#vss.u, SA),
    case verify_proof(VSS, Sender, ReadyProof) andalso
         dkg_commitment:verify_point(Commitment, Sender, Id, A) of
        true ->
            %% The point is valid and we have a ready proof
            case dkg_commitment:add_ready(Commitment, Sender, A) of
                {true, NewCommitment0} ->
                    {true, NewCommitment} = dkg_commitment:add_ready_proof(NewCommitment0, Sender, ReadyProof),
                    case dkg_commitment:num_readies(NewCommitment) == (T+1) andalso
                         dkg_commitment:num_echoes(NewCommitment) < ceil((N+T+1)/2) of
                        true when CB == true ->
                            SubShares = dkg_commitment:interpolate(NewCommitment, ready, dkg_util:allnodes(N)),
                            MyReadyProof = construct_proof(VSS),
                            Msgs = lists:map(fun(Node) ->
                                                     erlang_pbc:element_to_binary(lists:nth(Node+1, SubShares))
                                             end, dkg_util:allnodes(N)),
                            NewVSS = store_commitment(NewCommitment, ready, VSS),
                            {NewVSS, {send, [{callback, {ready, {Session, SerializedCommitmentMatrix0, Msgs, MyReadyProof}}}]}};
                        true ->
                            %% not in callback mode
                            SubShares = dkg_commitment:interpolate(NewCommitment, ready, dkg_util:allnodes(N)),
                            MyReadyProof = construct_proof(VSS),
                            Msgs = lists:map(fun(Node) ->
                                                     {unicast, Node, {ready, {Session, SerializedCommitmentMatrix0, erlang_pbc:element_to_binary(lists:nth(Node+1, SubShares)), MyReadyProof}}}
                                             end, dkg_util:allnodes(N)),
                            NewVSS = store_commitment(NewCommitment, ready, VSS),
                            {NewVSS, {send, Msgs}};
                        false ->
                            case dkg_commitment:num_readies(NewCommitment) == (N-T-F) andalso
                                 maps:size(dkg_commitment:ready_proofs(NewCommitment)) == (N-T-F) of
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

-spec serialize(vss()) -> #{atom() => binary() | map()}.
serialize(#vss{id = Id,
               n = N,
               f = F,
               t = T,
               u = U,
               u2 = U2,
               done = Done,
               session = Session,
               received_commitment = ReceivedCommitment,
               commitments = Commitments,
               callback = Callback}) ->
    PreSer = #{u => erlang_pbc:element_to_binary(U),
               u2 => erlang_pbc:element_to_binary(U2)},
    M0 = #{id => Id,
           commitments => maps:map(fun(_, V) -> dkg_commitment:serialize(V) end,
                                   Commitments),
           n => N,
           f => F,
           t => T,
           done => Done,
           session => Session,
           received_commitment => ReceivedCommitment,
           callback => Callback},
    M = maps:map(fun(_K, Term) -> term_to_binary(Term) end, M0),
    maps:merge(PreSer, M).

-spec deserialize(#{atom() => binary() | map()}, erlang_pbc:element(), fun(), fun()) -> vss().
deserialize(Map0, Element, SignFun, VerifyFun) ->
    Map = maps:map(fun(K, V) when K == u; K == u2;
                                  K == shares_results ->
                           V;
                      (_K, B) ->
                           binary_to_term(B)
                   end, Map0),
    #{id := Id,
      n := N,
      f := F,
      t := T,
      u := SerializedU,
      u2 := SerializedU2,
      done := Done,
      session := Session,
      received_commitment := ReceivedCommitment,
      commitments := SerializedCommitments,
      callback := Callback} = Map,
    #vss{id = Id,
         n = N,
         f = F,
         t = T,
         u = erlang_pbc:binary_to_element(Element, SerializedU),
         u2 = erlang_pbc:binary_to_element(Element, SerializedU2),
         done = Done,
         session = Session,
         received_commitment = ReceivedCommitment,
         commitments = maps:map(fun(_, V) -> dkg_commitment:deserialize(V, Element) end, SerializedCommitments),
         signfun = SignFun,
         verifyfun = VerifyFun,
         callback = Callback}.

-spec status(vss()) -> map().
status(VSS) ->
    #{id => VSS#vss.id,
      session => VSS#vss.session,
      received_commitment => VSS#vss.received_commitment,
      commitments => maps:map(fun(Id, C) ->
                                      #{
                                       num_echoes => dkg_commitment:num_echoes(C),
                                       num_readies => dkg_commitment:num_readies(C),
                                       missing_echoes => lists:seq(1, VSS#vss.n) -- maps:keys(dkg_commitment:echoes(C)),
                                       missing_readies => lists:seq(1, VSS#vss.n) -- maps:keys(dkg_commitment:ready_proofs(C)),
                                       sent_echo => lists:member(Id, VSS#vss.sent_echoes),
                                       sent_ready => lists:member(Id, VSS#vss.sent_readies)
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

store_commitment(Commitment, ready, VSS) ->
    Key = erlang:phash2(dkg_commitment:binary_matrix(Commitment)),
    VSS#vss{commitments=maps:put(Key, Commitment, VSS#vss.commitments), sent_readies=[Key|VSS#vss.sent_readies]};
store_commitment(Commitment, echo, VSS) ->
    Key = erlang:phash2(dkg_commitment:binary_matrix(Commitment)),
    VSS#vss{commitments=maps:put(Key, Commitment, VSS#vss.commitments), sent_echoes=[Key|VSS#vss.sent_echoes]}.


construct_proof(#vss{id=Id, session={Dealer, Round}, signfun=SignFun}) ->
    %% Construct and sign a proof
    %% XXX See the notes on the construction of 'Round' above
    %% This proof is the evidence used to attest that we had enough shares to interpolate the secret
    SignFun(<<Id:32/integer-unsigned-little,
              Dealer:32/integer-unsigned-little, Round/binary>>).


verify_proof(#vss{session={Dealer, Round}, verifyfun=VerifyFun}, Sender, Proof) ->
    Msg = <<Sender:32/integer-unsigned-little,
              Dealer:32/integer-unsigned-little, Round/binary>>,
    VerifyFun(Sender, Msg, Proof).
