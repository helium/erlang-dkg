-module(dkg_hybridvss_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-export([all/0, init_per_testcase/2, end_per_testcase/2]).
-export([
         symmetric_test/1,
         asymmetric_test/1
        ]).

all() ->
    [
     symmetric_test,
     asymmetric_test
    ].

init_per_testcase(_, Config) ->
    N = list_to_integer(os:getenv("N", "10")),
    F = (N - 1) div 3,
    T = F,
    Ph = 0,
    Module = dkg_hybridvss,
    [{n, N}, {f, F}, {module, Module}, {t, T}, {ph, Ph} | Config].

end_per_testcase(_, _Config) ->
    ok.

symmetric_test(Config) ->
    N = proplists:get_value(n, Config),
    F = proplists:get_value(f, Config),
    T = proplists:get_value(t, Config),
    Module = proplists:get_value(module, Config),
    Curve = 'SS512',
    {G1, G2} = dkg_test_utils:generate(Curve),
    run(Module, N, F, T, Curve, G1, G2).

asymmetric_test(Config) ->
    N = proplists:get_value(n, Config),
    F = proplists:get_value(f, Config),
    T = proplists:get_value(t, Config),
    Module = proplists:get_value(module, Config),
    Curve = 'MNT224',
    {G1, G2} = dkg_test_utils:generate(Curve),
    run(Module, N, F, T, Curve, G1, G2).

run(Module, N, F, T, Curve, G1, G2) ->

    [Dealer | Rest] = [ Module:init(Id, N, F, T, G1, G2, {1, 0}, false) || Id <- lists:seq(1, N) ],

    Secret = erlang_pbc:element_random(erlang_pbc:element_new('Zr', G1)),

    {NewDealerState, {send, MsgsToSend}} = Module:input(Dealer, Secret),

    States = [NewDealerState | Rest],
    StatesWithId = lists:zip(lists:seq(1, length(States)), States),
    {_FinalStates, ConvergedResults} = dkg_test_utils:do_send_outer(Module, [{1, {send, MsgsToSend}}], StatesWithId, sets:new()),

    %% check that the shares from nodes can be interpolated to calculate the original secret back
    NodesAndShares = lists:foldl(fun({result, {Node, {_Session, _Commitment, Share}}}, Acc) ->
                                        maps:put(Node, Share, Acc)
                                end, #{}, sets:to_list(ConvergedResults)),

    AllCommitments = [Commitment || {result, {_Node, {_Session, Commitment, _Share}}} <- sets:to_list(ConvergedResults)],
    OutputCommitment = hd(AllCommitments),

    %[VerificationKey | PublicKeyShares] = dkg_commitment:interpolate(OutputCommitment, ready, lists:seq(1, N)),
    VerificationKey = dkg_commitmentmatrix:lookup([1, 1], dkg_commitment:matrix(OutputCommitment)),
    true = erlang_pbc:element_cmp(VerificationKey, dkg_commitment:public_key_share(OutputCommitment, 0)),
    PublicKeyShares = [dkg_commitment:public_key_share(OutputCommitment, NodeID) || NodeID <- lists:seq(1, N)] ,

    PublicKey = tpke_pubkey:init(N, F, G1, G2, VerificationKey, PublicKeyShares, Curve),
    PrivateKeys = [ tpke_privkey:init(PublicKey, Share, NodeID-1) || {NodeID, Share} <- maps:to_list(NodesAndShares)],
    Msg = crypto:hash(sha256, crypto:strong_rand_bytes(12)),
    MessageToSign = tpke_pubkey:hash_message(PublicKey, Msg),
    SignatureShares = [tpke_privkey:sign(PrivateKey, MessageToSign) || PrivateKey <- PrivateKeys],
    ?assert(lists:all(fun(E) -> E end, [tpke_pubkey:verify_signature_share(PublicKey, SignatureShare, MessageToSign) || SignatureShare <- SignatureShares])),

    case erlang_pbc:element_cmp(G1, G2) of
        true ->
            Message = crypto:hash(sha256, <<"my hovercraft is full of eels">>),
            CipherText = tpke_pubkey:encrypt(PublicKey, Message),
            ?assert(tpke_pubkey:verify_ciphertext(PublicKey, CipherText)),
            DecShares = [tpke_privkey:decrypt_share(PrivateKey, CipherText) || PrivateKey <- PrivateKeys],
            ?assert(lists:all(fun(E) -> E end, [tpke_pubkey:verify_share(PublicKey, DecShare, CipherText) || DecShare <- DecShares])),
            ?assertEqual(Message, tpke_pubkey:combine_shares(PublicKey, CipherText, dealer:random_n(T+1, DecShares)));
        false ->
            ok
    end,

    true = lists:all(fun(C) -> dkg_commitment:cmp(hd(AllCommitments), C) end, tl(AllCommitments)),

    {Indices0, Elements} = lists:unzip(maps:to_list(NodesAndShares)),
    Indices = [ erlang_pbc:element_set(erlang_pbc:element_new('Zr', hd(Elements)), I) || I <- Indices0 ],
    Shares = lists:foldl(fun(Index, Acc) ->
                                 case maps:is_key(Index, NodesAndShares) of
                                     false ->
                                         %% Node ${Index} has not sent us a share, interpolate it
                                         Alpha = erlang_pbc:element_set(erlang_pbc:element_new('Zr', hd(Elements)), Index),
                                         LagrangePoly = dkg_lagrange:coefficients(Indices, Alpha),
                                         InterpolatedShare = dkg_lagrange:evaluate_zr(LagrangePoly, Elements),
                                         [ InterpolatedShare | Acc];
                                     true ->
                                         %% Node ${Index} has sent us a share
                                         [ maps:get(Index, NodesAndShares) | Acc]
                                 end
                         end, [], [0 | lists:seq(1,N)]), %% note that we also evaluate at 0

    CalculatedSecret = hd(lists:reverse(Shares)),
    ?assert(erlang_pbc:element_cmp(CalculatedSecret, Secret)),
    ok.
