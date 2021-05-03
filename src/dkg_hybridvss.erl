-module(dkg_hybridvss).

-export([init/8, init/9]).

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
          session :: session(),
          received_commitment = false :: boolean(),
          commitments = #{} :: #{binary() => dkg_commitment:commitment()},
          callback = false :: boolean(),
          sent_echoes = [] :: [integer()],
          sent_readies = [] :: [integer()],
          signfun :: signfun(),
          verifyfun :: verifyfun(),
          commitments_sent = #{} :: #{integer() => [pos_integer()]},
          commitments_received = #{} :: #{integer() => [pos_integer()]},
          commitment_cache_fun :: fun()
         }).

%% Note that the 'Round' here is assumed to be some unique combination of members and some strictly increasing counter(s) or nonce.
%% For example, something like the SHA of the public keys of all the members and some global DKG counter.
%% The counter/nonce should NOT repeat under any circumstances or ready messages may be reused to forge subsequent round results.
-type session() :: {Dealer :: pos_integer(), Round :: binary()}.
-type send_msg() :: {unicast, pos_integer(), {send, {session(), dkg_commitment:serialized_commitment(), binary()}}}.
-type echo_msg() :: {unicast, pos_integer(), {echo, {session(), dkg_commitment:serialized_commitment(), binary()}}}.
-type ready_msg() :: {unicast, pos_integer(), {ready, {session(), dkg_commitment:serialized_commitment(), binary()}}}.
-type result() :: {result, {session(), dkg_commitment:commitment(), tc_fr:fr()}}.
-type vss() :: #vss{}.
-type signfun() :: fun((Msg :: binary()) -> Signature :: binary()).
-type verifyfun() :: fun((Sender :: pos_integer(), Msg :: binary(), Signature :: binary()) -> boolean()).

-export_type([vss/0, session/0]).


init(Id, N, F, T, Session, Callback, SignFun, VerifyFun) ->
    init(Id, N, F, T, Session, Callback, SignFun, VerifyFun, fun dkg_hybriddkg:default_commitment_cache_fun/1).

-spec init(Id :: pos_integer(), N :: pos_integer(), F :: pos_integer(), T :: pos_integer(),
           Session :: session(), Callback :: boolean(),
           SignFun :: signfun(), VerifyFun :: verifyfun(), CCacheFun :: fun())-> vss().
init(Id, N, F, T, Session, Callback, SignFun, VerifyFun, CCacheFun) ->
    true = N >= (3*T + 2*F + 1),
    #vss{id=Id,
         n=N,
         f=F,
         t=T,
         session=Session,
         callback=Callback,
         signfun = SignFun,
         verifyfun = VerifyFun,
         commitment_cache_fun=CCacheFun}.

%% upon a message (Pd, τ, in, share, s): /* only Pd */
%%     choose a symmetric bivariate polynomial φ(x,y) = ∑tj,l=0 φjl x^j y^l ∈R Zp[x,y] and φ00 = s
%%     C ←{Cjl } t j,l=0 where Cjl = gφ^jl for j,l ∈[0,t]
%%     for all j ∈ [1,n] do
%%         aj(y) ← φ(j,y); send the message (Pd, τ, send, C, aj) to Pj
-spec input(VSS :: vss(), Secret :: integer()) -> {vss(), {send, [send_msg()]} | ok}.
input(VSS = #vss{session=Session={Dealer,_}, id=Id, t=T, n=N, callback=CB}, Secret) when Dealer == Id ->
    BiPoly = tc_bipoly:with_secret(Secret, T),
    Commitment = dkg_commitment:new(dkg_util:allnodes(N), tc_bipoly:commitment(BiPoly), VSS#vss.commitment_cache_fun),
    %% only serialize this once, not in the loop below
    SerializedCommitmentMatrix = dkg_commitment:matrix(Commitment),

    case CB of
        true ->
            Msgs = lists:map(fun(Node) ->
                                     tc_poly:serialize(tc_bipoly:row(BiPoly, Node))
                             end, dkg_util:allnodes(N)),
            {commitment_sent(SerializedCommitmentMatrix, dkg_util:allnodes(N), store_commitment(Commitment, Id, VSS)), {send, [{callback, {send, {Session, SerializedCommitmentMatrix, Msgs}}}]}};
        false ->
            Msgs = lists:map(fun(Node) ->
                                     Aj = tc_poly:serialize(tc_bipoly:row(BiPoly, Node)),
                                     {unicast, Node, {send, {Session, SerializedCommitmentMatrix, Aj}}}
                             end, dkg_util:allnodes(N)),
            {commitment_sent(SerializedCommitmentMatrix, dkg_util:allnodes(N), store_commitment(Commitment, Id, VSS)), {send, Msgs}}
    end;
input(VSS, _Secret) ->
    {VSS, ok}.

%% upon a message (Pd, τ, send, C, a) from Pd (first time):
%%     if verify-poly(C, i, a) then
%%         for all j ∈ [1, n] do
%%             send the message (Pd , τ, echo, C, a(j)) to Pj
-spec handle_msg(vss(), pos_integer(), send_msg() | echo_msg() | ready_msg()) -> {vss(), {send, [echo_msg() | ready_msg()]} | ok | ignore | result()}.
handle_msg(VSS=#vss{n=N, id=Id, session=Session, received_commitment=false, callback=CB}, Sender, {send, {Session = {Sender, _}, SerializedCommitmentMatrix0, SA}}) ->
    case get_commitment(SerializedCommitmentMatrix0, VSS) of
        {ok, Commitment} ->
            A = tc_poly:deserialize(SA),
            case dkg_commitment:verify_poly(Commitment, Id, A) of
                true when CB == true ->
                    Msgs = lists:map(fun(Node) ->
                                             tc_fr:serialize(tc_poly:eval(A, Node))
                                     end, dkg_util:allnodes(N)),
                    {commitment_sent(SerializedCommitmentMatrix0, dkg_util:allnodes(N), store_commitment(Commitment, Sender, echo, VSS#vss{received_commitment=true})), {send, [{callback, {echo, {Session, SerializedCommitmentMatrix0, Msgs}}}]}};
                true ->
                    Msgs = lists:map(fun(Node) ->
                                             {unicast, Node, {echo, {Session, maybe_send_commitment(SerializedCommitmentMatrix0, N, VSS), tc_fr:serialize(tc_poly:eval(A, Node))}}}
                                     end, dkg_util:allnodes(N)),
                    {commitment_sent(SerializedCommitmentMatrix0, dkg_util:allnodes(N), store_commitment(Commitment, Sender, echo, VSS#vss{received_commitment=true})), {send, Msgs}};
                false ->
                    {VSS, ok}
            end;
        {error, _Reason} ->
            {VSS, ignore}
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
    case get_commitment(SerializedCommitmentMatrix0, VSS) of
        {ok, Commitment} ->
            case dkg_commitment:num_echoes(Commitment) < ceil((N+T+1)/2) andalso
                 dkg_commitment:verify_point(Commitment, Sender, Id, SA) of
                true ->
                    case dkg_commitment:add_echo(Commitment, Sender, SA) of
                        {true, NewCommitment} ->
                            case dkg_commitment:num_echoes(NewCommitment) == ceil((N+T+1)/2) andalso
                                 dkg_commitment:num_readies(NewCommitment) < (T+1) of
                                true when CB == true ->
                                    SubShares = dkg_commitment:interpolate(NewCommitment, T, echo),
                                    ReadyProof = construct_proof(VSS),
                                    Msgs = lists:map(fun(Node) ->
                                                             tc_fr:serialize(tc_poly:eval(SubShares, Node))
                                                     end, dkg_util:allnodes(N)),
                                    {store_commitment(NewCommitment, Sender, ready, VSS), {send, [{callback, {ready, {Session, SerializedCommitmentMatrix0, Msgs, ReadyProof}}}]}};
                                true ->
                                    %% not in callback mode
                                    SubShares = dkg_commitment:interpolate(NewCommitment, T, echo),
                                    ReadyProof = construct_proof(VSS),
                                    Msgs = lists:map(fun(Node) ->
                                                             {unicast, Node, {ready, {Session, maybe_send_commitment(SerializedCommitmentMatrix0, Node, VSS), tc_fr:serialize(tc_poly:eval(SubShares, Node)), ReadyProof}}}
                                                     end, dkg_util:allnodes(N)),
                                    {store_commitment(NewCommitment, Sender, ready, VSS), {send, Msgs}};
                                false ->
                                    {store_commitment(NewCommitment, Sender, VSS), ok}
                            end;
                        {false, OldCommitment} ->
                            {store_commitment(OldCommitment, Sender, VSS), ok}
                    end;
                false ->
                    {VSS, ok}
            end;
        {error, _Error} ->
            {VSS, ignore}
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
    case get_commitment(SerializedCommitmentMatrix0, VSS) of
        {ok, Commitment} ->
            case verify_proof(VSS, Sender, ReadyProof) andalso
                 dkg_commitment:verify_point(Commitment, Sender, Id, SA) of
                true ->
                    %% The point is valid and we have a ready proof
                    case dkg_commitment:add_ready(Commitment, Sender, SA) of
                        {true, NewCommitment0} ->
                            {true, NewCommitment} = dkg_commitment:add_ready_proof(NewCommitment0, Sender, ReadyProof),
                            case dkg_commitment:num_readies(NewCommitment) == (T+1) andalso
                                 dkg_commitment:num_echoes(NewCommitment) < ceil((N+T+1)/2) of
                                true when CB == true ->
                                    SubShares = dkg_commitment:interpolate(NewCommitment, T, ready),
                                    MyReadyProof = construct_proof(VSS),
                                    Msgs = lists:map(fun(Node) ->
                                                             tc_fr:serialize(tc_poly:eval(SubShares, Node))
                                                     end, dkg_util:allnodes(N)),
                                    NewVSS = store_commitment(NewCommitment, ready, VSS),
                                    {NewVSS, {send, [{callback, {ready, {Session, SerializedCommitmentMatrix0, Msgs, MyReadyProof}}}]}};
                                true ->
                                    %% not in callback mode
                                    SubShares = dkg_commitment:interpolate(NewCommitment, T, ready),
                                    MyReadyProof = construct_proof(VSS),
                                    Msgs = lists:map(fun(Node) ->
                                                             {unicast, Node, {ready, {Session, maybe_send_commitment(SerializedCommitmentMatrix0, Node, VSS), tc_fr:serialize(tc_poly:eval(SubShares, Node)), MyReadyProof}}}
                                                     end, dkg_util:allnodes(N)),
                                    NewVSS = store_commitment(NewCommitment, ready, VSS),
                                    {NewVSS, {send, Msgs}};
                                false ->
                                    case dkg_commitment:num_readies(NewCommitment) == (N-T-F) andalso
                                         maps:size(dkg_commitment:ready_proofs(NewCommitment)) == (N-T-F) of
                                        true->
                                            SubShares = dkg_commitment:interpolate(NewCommitment, T, ready),
                                            SubShare = tc_poly:eval(SubShares, 0),
                                            %% clear the commitments out of our state and return the winning one
                                            {VSS#vss{done=true, commitments=#{}}, {result, {Session, NewCommitment, SubShare}}};
                                        false ->
                                            NewVSS = store_commitment(NewCommitment, Sender, VSS),
                                            {NewVSS, ok}
                                    end
                            end;
                        {false, OldCommitment} ->
                            {store_commitment(OldCommitment, Sender, VSS), ok}
                    end;
                false ->
                    {VSS, ok}
            end;
        {error, _Error} ->
            {VSS, ignore}
    end;
handle_msg(VSS, _Sender, _Msg) ->
    %% we're likely done here, so there's no point in processing more messages
    {VSS, ignore}.

-spec serialize(vss()) -> #{atom() => binary() | map()}.
serialize(#vss{id = Id,
               n = N,
               f = F,
               t = T,
               done = Done,
               session = Session,
               received_commitment = ReceivedCommitment,
               commitments = Commitments,
               commitments_sent = CS,
               commitments_received = CR,
               callback = Callback}) ->
    M0 = #{id => Id,
           commitments => maps:map(fun(_, V) -> dkg_commitment:serialize(V) end,
                                   Commitments),
           n => N,
           f => F,
           t => T,
           done => Done,
           session => Session,
           received_commitment => ReceivedCommitment,
           commitments_sent => CS,
           commitments_received => CR,
           callback => Callback},
    maps:map(fun(_K, Term) -> term_to_binary(Term) end, M0).

-spec deserialize(#{atom() => binary() | map()}, fun(), fun(), fun()) -> vss().
deserialize(Map0, SignFun, VerifyFun, CCacheFun) ->
    Map = maps:map(fun(_K, B) ->
                           binary_to_term(B)
                   end, Map0),
    #{id := Id,
      n := N,
      f := F,
      t := T,
      done := Done,
      session := Session,
      received_commitment := ReceivedCommitment,
      commitments := SerializedCommitments,
      commitments_sent := CS,
      commitments_received := CR,
      callback := Callback} = Map,
    #vss{id = Id,
         n = N,
         f = F,
         t = T,
         done = Done,
         session = Session,
         received_commitment = ReceivedCommitment,
         commitments = maps:map(fun(_, V) -> dkg_commitment:deserialize(V, CCacheFun) end, SerializedCommitments),
         commitments_sent = CS,
         commitments_received = CR,
         signfun = SignFun,
         verifyfun = VerifyFun,
         callback = Callback,
         commitment_cache_fun=CCacheFun}.

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

get_commitment(Hash, VSS) when is_integer(Hash) ->
    case maps:find(Hash, VSS#vss.commitments) of
        {ok, Commitment} ->
            {ok, Commitment};
        _ ->
            {error, unknown_commitment_hash}
    end;
get_commitment(SerializedMatrix, VSS = #vss{n=N, t=T, commitment_cache_fun=Fun}) ->
    Key = erlang:phash2(SerializedMatrix),
    case maps:find(Key, VSS#vss.commitments) of
        {ok, Commitment} ->
            {ok, Commitment};
        error ->
            try tc_bicommitment:deserialize(SerializedMatrix) of
                BiCommitment ->
                    case tc_bicommitment:degree(BiCommitment) == T of
                        true ->
                            {ok, dkg_commitment:new(dkg_util:allnodes(N), BiCommitment, SerializedMatrix, Fun)};
                        false ->
                            {error, degree_mismatch}
                    end
            catch _:_ ->
                      {error, commitment_deserialization_failed}
            end
    end.

store_commitment(Commitment, Sender, VSS) ->
    Key = dkg_commitment:hash(Commitment),
    commitment_received(dkg_commitment:matrix(Commitment), Sender, VSS#vss{commitments=maps:put(Key, Commitment, VSS#vss.commitments)}).

store_commitment(Commitment, Sender, ready, VSS) ->
    Key = dkg_commitment:hash(Commitment),
    commitment_received(dkg_commitment:matrix(Commitment), Sender, VSS#vss{commitments=maps:put(Key, Commitment, VSS#vss.commitments), sent_readies=[Key|VSS#vss.sent_readies]});
store_commitment(Commitment, Sender, echo, VSS) ->
    Key = dkg_commitment:hash(Commitment),
    commitment_received(dkg_commitment:matrix(Commitment), Sender, VSS#vss{commitments=maps:put(Key, Commitment, VSS#vss.commitments), sent_echoes=[Key|VSS#vss.sent_echoes]}).


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

commitment_sent(SerializedCommitment, Nodes, VSS=#vss{commitments_sent=CS}) ->
    VSS#vss{commitments_sent=maps:update_with(erlang:phash2(SerializedCommitment), fun(V) -> lists:usort(Nodes ++ V) end, Nodes, CS)}.

commitment_received(SerializedCommitment, Node, VSS=#vss{commitments_received=CR}) ->
    VSS#vss{commitments_received=maps:update_with(erlang:phash2(SerializedCommitment), fun(V) -> lists:usort([Node|V]) end, [Node], CR)}.

maybe_send_commitment(Commitment, Node, VSS) ->
    Hash = erlang:phash2(Commitment),
    case lists:member(Node, maps:get(Hash, VSS#vss.commitments_sent, [])) orelse
         lists:member(Node, maps:get(Hash, VSS#vss.commitments_sent, [])) of
        true ->
            Hash;
        false ->
            Commitment
    end.

