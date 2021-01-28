-module(dkg_hybridvss2).

-export([init/8]).

-export([
    input/2
    %% status/1,
    %% verify_proof/3
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
    commitments = #{} :: #{binary() => bicommitment:bicommitment()},
    callback = false :: boolean(),
    signfun :: signfun(),
    verifyfun :: verifyfun()
}).

%% Note that the 'Round' here is assumed to be some unique combination of members and some strictly increasing counter(s) or nonce.
%% For example, something like the SHA of the public keys of all the members and some global DKG counter.
%% The counter/nonce should NOT repeat under any circumstances or ready messages may be reused to forge subsequent round results.
-type session() :: {Dealer :: pos_integer(), Round :: binary()}.
-type send_msg() ::
    {unicast, pos_integer(),
        {send, {Session :: session(), BiCommitment :: binary(), Poly :: binary()}}}.

-type echo_msg() ::
    {unicast, pos_integer(),
        {echo, {Session :: session(), BiCommitment :: binary(), Alpha :: binary()}}}.

-type ready_msg() ::
    {unicast, pos_integer(),
        {ready, {Session :: session(), BiCommitment :: binary(), Alpha :: binary()}}}.

-type result() ::
    {result, {Session :: session(), Commitment :: commitment:commitment(), [fr:fr()]}}.

-type vss() :: #vss{}.
-type signfun() :: fun((Msg :: binary()) -> Signature :: binary()).
-type verifyfun() :: fun(
    (Sender :: pos_integer(), Msg :: binary(), Signature :: binary()) -> boolean()
).

-export_type([vss/0, session/0]).

-spec init(
    Id :: pos_integer(),
    N :: pos_integer(),
    F :: pos_integer(),
    T :: pos_integer(),
    Session :: session(),
    Callback :: boolean(),
    SignFun :: signfun(),
    VerifyFun :: verifyfun()
) -> vss().
init(Id, N, F, T, Session, Callback, SignFun, VerifyFun) ->
    true = N >= (3 * T + 2 * F + 1),
    #vss{
        id = Id,
        n = N,
        f = F,
        t = T,
        session = Session,
        callback = Callback,
        signfun = SignFun,
        verifyfun = VerifyFun
    }.

%% upon a message (Pd, τ, in, share, s): /* only Pd */
%%     choose a symmetric bivariate polynomial φ(x,y) = ∑tj,l=0 φjl x^j y^l ∈R Zp[x,y] and φ00 = s
%%     C ←{Cjl } t j,l=0 where Cjl = gφ^jl for j,l ∈[0,t]
%%     for all j ∈ [1,n] do
%%         aj(y) ← φ(j,y); send the message (Pd, τ, send, C, aj) to Pj
-spec input(VSS :: vss(), Secret :: non_neg_integer()) -> {vss(), {send, [send_msg()]} | ok}.
input(
    VSS = #vss{session = Session = {Dealer, _}, id = Id, t = T, n = N, callback = true},
    Secret
) when Dealer == Id ->
    BiPoly = bipoly:with_secret(Secret, T),
    BiCommitment = bipoly:commitment(BiPoly),
    SerializedBiCommitment = bicommitment:serialize(BiCommitment),

    Msgs = lists:map(
        fun(Node) ->
            Poly = bipoly:row(BiPoly, Node),
            poly:serialize(Poly)
        end,
        dkg_util:allnodes(N)
    ),
    {store_bicommitment(BiCommitment, SerializedBiCommitment, VSS),
        {send, [{callback, {send, {Session, SerializedBiCommitment, Msgs}}}]}};
input(VSS = #vss{session = Session = {Dealer, _}, id = Id, t = T, n = N}, Secret) when
    Dealer == Id
->
    BiPoly = bipoly:with_secret(Secret, T),
    BiCommitment = bipoly:commitment(BiPoly),
    SerializedBiCommitment = bicommitment:serialize(BiCommitment),

    Msgs = lists:map(
        fun(Node) ->
            Aj = bipoly:row(BiPoly, Node),
            SerializedAj = poly:serialize(Aj),
            {unicast, Node, {send, {Session, SerializedBiCommitment, SerializedAj}}}
        end,
        dkg_util:allnodes(N)
    ),
    {store_bicommitment(BiCommitment, SerializedBiCommitment, VSS), {send, Msgs}};
input(VSS, _Secret) ->
    {VSS, ok}.

%% upon a message (Pd, τ, send, C, a) from Pd (first time):
%%     if verify-poly(C, i, a) then
%%         for all j ∈ [1, n] do
%%             send the message (Pd , τ, echo, C, a(j)) to Pj
-spec handle_msg(vss(), pos_integer(), send_msg() | echo_msg() | ready_msg()) ->
    {vss(), {send, [echo_msg() | ready_msg()]} | ok | ignore | result()}.
handle_msg(
    VSS = #vss{n = N, id = Id, session = Session, received_commitment = false, callback = true},
    Sender,
    {send, {Session = {Sender, _}, SerializedBiCommitment, SerializedAj}}
) ->
    BiCommitment = get_bicommitment(SerializedBiCommitment, VSS),
    Aj = poly:deserialize(SerializedAj),
    case bicommitment:verify_poly(BiCommitment, Id, Aj) of
        true ->
            Msgs = lists:map(
                fun(Node) ->
                    Eval = poly:eval(Aj, Node),
                    fr:serialize(Eval)
                end,
                dkg_util:allnodes(N)
            ),
            {store_bicommitment(BiCommitment, SerializedBiCommitment, VSS#vss{
                    received_commitment = true
                }), {send, [{callback, {echo, {Session, SerializedBiCommitment, Msgs}}}]}};
        false ->
            {VSS, ok}
    end;
handle_msg(
    VSS = #vss{n = N, id = Id, session = Session, received_commitment = false},
    Sender,
    {send, {Session = {Sender, _}, SerializedBiCommitment, SerializedAj}}
) ->
    BiCommitment = get_bicommitment(SerializedBiCommitment, VSS),
    Aj = poly:deserialize(SerializedAj),
    case bicommitment:verify_poly(BiCommitment, Id, Aj) of
        true ->
            Msgs = lists:map(
                fun(Node) ->
                    {unicast, Node,
                        {echo,
                            {Session, SerializedBiCommitment, fr:serialize(poly:eval(Aj, Node))}}}
                end,
                dkg_util:allnodes(N)
            ),
            {store_bicommitment(BiCommitment, SerializedBiCommitment, VSS#vss{
                    received_commitment = true
                }), {send, Msgs}};
        false ->
            {VSS, ok}
    end;
handle_msg(VSS, _Sender, {send, {_Session, _BiCommitment, _Aj}}) ->
    %% already received a commitment, or it's not from the dealer; ignore this one
    {VSS, ignore}.
%% %% upon a message (Pd, τ, echo, C, α) from Pm (first time):
%% %%     if verify-point(C, i, m, α) then
%% %%         AC ← AC ∪ {(m, α)}; eC ← eC + 1
%% %%         if eC = d n+t+1/2 and rC < t + 1 then
%% %%             Lagrange-interpolate a from AC
%% %%             for all j ∈ [1, n] do
%% %%                  send the message (Pd, τ, ready, C, a(j)) to Pj
%% handle_msg(
%%     VSS = #vss{id = Id, n = N, t = T, session = Session, done = false, callback = CB},
%%     Sender,
%%     {echo, {Session, SerializedBiCommitment, SAlpha}}
%% ) ->
%%     BiCommitment = get_bicommitment(SerializedBiCommitment, VSS),
%%     Alpha = fr:deserialize(SAlpha),
%%     case bicommitment:verify_point(Commitment, Sender, Id, A) of
%%         true ->
%%             case dkg_commitment:add_echo(Commitment, Sender, A) of
%%                 {true, NewCommitment} ->
%%                     case
%%                         dkg_commitment:num_echoes(NewCommitment) == ceil((N + T + 1) / 2) andalso
%%                             dkg_commitment:num_readies(NewCommitment) < (T + 1)
%%                     of
%%                         true when CB == true ->
%%                             Subshares = dkg_commitment:interpolate(
%%                                 NewCommitment,
%%                                 echo,
%%                                 dkg_util:allnodes(N)
%%                             ),
%%                             ReadyProof = construct_proof(VSS),
%%                             Msgs = lists:map(
%%                                 fun(Node) ->
%%                                     erlang_pbc:element_to_binary(lists:nth(Node + 1, Subshares))
%%                                 end,
%%                                 dkg_util:allnodes(N)
%%                             ),
%%                             {store_commitment(NewCommitment, VSS),
%%                                 {send, [
%%                                     {callback,
%%                                         {ready,
%%                                             {Session, SerializedCommitmentMatrix0, Msgs,
%%                                                 ReadyProof}}}
%%                                 ]}};
%%                         true ->
%%                             %% not in callback mode
%%                             Subshares = dkg_commitment:interpolate(
%%                                 NewCommitment,
%%                                 echo,
%%                                 dkg_util:allnodes(N)
%%                             ),
%%                             ReadyProof = construct_proof(VSS),
%%                             Msgs = lists:map(
%%                                 fun(Node) ->
%%                                     {unicast, Node,
%%                                         {ready,
%%                                             {Session, SerializedCommitmentMatrix0,
%%                                                 erlang_pbc:element_to_binary(
%%                                                     lists:nth(Node + 1, Subshares)
%%                                                 ),
%%                                                 ReadyProof}}}
%%                                 end,
%%                                 dkg_util:allnodes(N)
%%                             ),
%%                             {store_commitment(NewCommitment, VSS), {send, Msgs}};
%%                         false ->
%%                             {store_commitment(NewCommitment, VSS), ok}
%%                     end;
%%                 {false, OldCommitment} ->
%%                     {store_commitment(OldCommitment, VSS), ok}
%%             end;
%%         false ->
%%             {VSS, ok}
%%     end;
%% %% upon a message (Pd, τ, ready, C, α) from Pm (first time):
%% %%     if verify-point(C, i, m, α) then
%% %%         AC ← AC ∪ {(m, α)}; rC ← rC + 1
%% %%         if rC = t + 1 and eC < n+t+1/2
%% %%             Lagrange-interpolate a from AC
%% %%             for all j ∈ [1, n] do
%% %%                 send the message (Pd, τ, ready, C, a(j)) to Pj
%% %%     else if rC = n − t − f then
%% %%         si ← a(0); output (Pd , τ, out, shared, C, si )
%% handle_msg(
%%     VSS = #vss{n = N, t = T, f = F, id = Id, done = false, callback = CB},
%%     Sender,
%%     {ready, {Session, SerializedCommitmentMatrix0, SA, ReadyProof}} = _ReadyMsg
%% ) ->
%%     Commitment = get_commitment(SerializedCommitmentMatrix0, VSS),
%%     A = erlang_pbc:binary_to_element(VSS#vss.u, SA),
%%     case
%%         verify_proof(VSS, Sender, ReadyProof) andalso
%%             dkg_commitment:verify_point(Commitment, Sender, Id, A)
%%     of
%%         true ->
%%             %% The point is valid and we have a ready proof
%%             case dkg_commitment:add_ready(Commitment, Sender, A) of
%%                 {true, NewCommitment0} ->
%%                     {true, NewCommitment} = dkg_commitment:add_ready_proof(
%%                         NewCommitment0,
%%                         Sender,
%%                         ReadyProof
%%                     ),
%%                     case
%%                         dkg_commitment:num_readies(NewCommitment) == (T + 1) andalso
%%                             dkg_commitment:num_echoes(NewCommitment) < ceil((N + T + 1) / 2)
%%                     of
%%                         true when CB == true ->
%%                             SubShares = dkg_commitment:interpolate(
%%                                 NewCommitment,
%%                                 ready,
%%                                 dkg_util:allnodes(N)
%%                             ),
%%                             MyReadyProof = construct_proof(VSS),
%%                             Msgs = lists:map(
%%                                 fun(Node) ->
%%                                     erlang_pbc:element_to_binary(lists:nth(Node + 1, SubShares))
%%                                 end,
%%                                 dkg_util:allnodes(N)
%%                             ),
%%                             NewVSS = store_commitment(NewCommitment, VSS),
%%                             {NewVSS,
%%                                 {send, [
%%                                     {callback,
%%                                         {ready,
%%                                             {Session, SerializedCommitmentMatrix0, Msgs,
%%                                                 MyReadyProof}}}
%%                                 ]}};
%%                         true ->
%%                             %% not in callback mode
%%                             SubShares = dkg_commitment:interpolate(
%%                                 NewCommitment,
%%                                 ready,
%%                                 dkg_util:allnodes(N)
%%                             ),
%%                             MyReadyProof = construct_proof(VSS),
%%                             Msgs = lists:map(
%%                                 fun(Node) ->
%%                                     {unicast, Node,
%%                                         {ready,
%%                                             {Session, SerializedCommitmentMatrix0,
%%                                                 erlang_pbc:element_to_binary(
%%                                                     lists:nth(Node + 1, SubShares)
%%                                                 ),
%%                                                 MyReadyProof}}}
%%                                 end,
%%                                 dkg_util:allnodes(N)
%%                             ),
%%                             NewVSS = store_commitment(NewCommitment, VSS),
%%                             {NewVSS, {send, Msgs}};
%%                         false ->
%%                             case
%%                                 dkg_commitment:num_readies(NewCommitment) == (N - T - F) andalso
%%                                     maps:size(dkg_commitment:ready_proofs(NewCommitment)) ==
%%                                         (N - T - F)
%%                             of
%%                                 true ->
%%                                     [SubShare] = dkg_commitment:interpolate(
%%                                         NewCommitment,
%%                                         ready,
%%                                         []
%%                                     ),
%%                                     %% clear the commitments out of our state and return the winning one
%%                                     {VSS#vss{done = true, commitments = #{}},
%%                                         {result, {Session, NewCommitment, SubShare}}};
%%                                 false ->
%%                                     NewVSS = store_commitment(NewCommitment, VSS),
%%                                     {NewVSS, ok}
%%                             end
%%                     end;
%%                 {false, OldCommitment} ->
%%                     {store_commitment(OldCommitment, VSS), ok}
%%             end;
%%         false ->
%%             {VSS, ok}
%%     end;
%% handle_msg(VSS, _Sender, _Msg) ->
%%     %% we're likely done here, so there's no point in processing more messages
%%     {VSS, ignore}.
%%
%% %%
%% %%-spec status(vss()) -> map().
%% %%status(VSS) ->
%% %%    #{id => VSS#vss.id,
%% %%      session => VSS#vss.session,
%% %%      received_commitment => VSS#vss.received_commitment,
%% %%      commitments => maps:map(fun(_, C) ->
%% %%                                      #{
%% %%                                       num_echoes => dkg_commitment:num_echoes(C),
%% %%                                       num_readies => dkg_commitment:num_readies(C),
%% %%                                       echoes => maps:keys(dkg_commitment:echoes(C)),
%% %%                                       readies => maps:keys(dkg_commitment:ready_proofs(C))
%% %%                                      }
%% %%                              end, VSS#vss.commitments),
%% %%      done => VSS#vss.done
%% %%     }.
%% %%
get_bicommitment(SerializedBiCommitment, VSS = #vss{t = T}) ->
    Key = erlang:phash2(SerializedBiCommitment),
    case maps:find(Key, VSS#vss.commitments) of
        {ok, BiCommitment} ->
            BiCommitment;
        error ->
            bipoly:commitment(bipoly:random(T))
    end.

store_bicommitment(BiCommitment, SerializedBiCommitment, VSS) ->
    Key = erlang:phash2(SerializedBiCommitment),
    VSS#vss{commitments = maps:put(Key, BiCommitment, VSS#vss.commitments)}.

%%
%%construct_proof(#vss{id=Id, session={Dealer, Round}, signfun=SignFun}) ->
%%    %% Construct and sign a proof
%%    %% XXX See the notes on the construction of 'Round' above
%%    %% This proof is the evidence used to attest that we had enough shares to interpolate the secret
%%    SignFun(<<Id:32/integer-unsigned-little,
%%              Dealer:32/integer-unsigned-little, Round/binary>>).
%%
%%
%%verify_proof(#vss{session={Dealer, Round}, verifyfun=VerifyFun}, Sender, Proof) ->
%%    Msg = <<Sender:32/integer-unsigned-little,
%%              Dealer:32/integer-unsigned-little, Round/binary>>,
%%    VerifyFun(Sender, Msg, Proof).
