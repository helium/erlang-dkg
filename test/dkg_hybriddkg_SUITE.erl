-module(dkg_hybriddkg_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-export([all/0, init_per_testcase/2, end_per_testcase/2]).
-export([
         init_test/1
        ]).

all() ->
    [
     init_test
    ].

init_per_testcase(_, Config) ->
    N = list_to_integer(os:getenv("N", "10")),
    F = (N - 1) div 3,
    T = F + 1,
    Ph = 0,
    Module = dkg_hybriddkg,
    [{n, N}, {f, F}, {module, Module}, {t, T}, {ph, Ph} | Config].

end_per_testcase(_, _Config) ->
    ok.

init_test(Config) ->
    N = proplists:get_value(n, Config),
    F = proplists:get_value(f, Config),
    T = proplists:get_value(t, Config),
    Module = proplists:get_value(module, Config),
    Group = erlang_pbc:group_new('SS512'),
    Generator = erlang_pbc:element_from_hash(erlang_pbc:element_new('G1', Group), <<"honeybadger">>),

    {StatesWithId, Replies} = lists:unzip(lists:map(fun(E) ->
                                                   {State, {send, Replies}} = Module:init(E, N, F, T, Generator, {1, 0}),
                                                   {{E, State}, {E, {send, Replies}}}
                                           end, lists:seq(1, N))),

    {_FinalStates, ConvergedResults} = dkg_test_utils:do_send_outer(Module, Replies, StatesWithId, sets:new()),
    ct:pal("Results ~p", [sets:to_list(ConvergedResults)]),

    %% XXX: this is the same as the pubkeyshare test, I'm sure there is more to it
    SecretKeyShares = lists:keysort(1, [ {Node, SecretKey} || {result, {Node, {SecretKey, _VerificationKey, _VerificationKeys}}} <- sets:to_list(ConvergedResults)]),
    VerificationKeys = lists:keysort(1, [ {Node, VerificationKey} || {result, {Node, {_SecretKey, VerificationKey, _VerificationKeys}}} <- sets:to_list(ConvergedResults)]),
    VerificationKeyss = lists:keysort(1, [ {Node, VerificationKeyz} || {result, {Node, {_SecretKey, _VerificationKey, VerificationKeyz}}} <- sets:to_list(ConvergedResults)]),
    ct:pal("Secret key shares ~p", [[ erlang_pbc:element_to_string(S) || {_, S} <- SecretKeyShares]]),
    ct:pal("Public key shares ~p", [[ erlang_pbc:element_to_string(S) || {_, S} <- VerificationKeys]]),
    ct:pal("Public key shares ~p", [[ lists:map(fun erlang_pbc:element_to_string/1, S) || {_, S} <- VerificationKeyss]]),
    PublicKeySharePoly = [Share || Share <- element(2, hd(VerificationKeyss))],
    KnownSecret = dkg_polynomial:apply(PublicKeySharePoly, 0),
    Indices = [ erlang_pbc:element_set(erlang_pbc:element_new('Zr', Generator), I) || I <- lists:seq(1, N) ],
    Alpha = erlang_pbc:element_set(erlang_pbc:element_new('Zr', Generator), 0),
    CalculatedSecret = dkg_lagrange:interpolate(PublicKeySharePoly, Indices, Alpha),
    ?assert(erlang_pbc:element_cmp(KnownSecret, CalculatedSecret)),

    %% attempt to construct some TPKE keys...

    PrivateKeys = lists:map(fun({result, {Node, {SK, VK, VKs}}}) ->
                                    PK = tpke_pubkey:init(N, F, Generator, Generator, VK, VKs, 'SS512'),
                                    tpke_privkey:init(PK, SK, Node)
                            end, sets:to_list(ConvergedResults)),
    PubKey = tpke_privkey:public_key(hd(PrivateKeys)),
    Msg = crypto:hash(sha256, crypto:strong_rand_bytes(12)),
    MessageToSign = tpke_pubkey:hash_message(PubKey, Msg),
    Signatures = [ tpke_privkey:sign(PrivKey, MessageToSign) || PrivKey <- PrivateKeys],
    ?assert(lists:all(fun(X) -> X end, [tpke_pubkey:verify_signature_share(PubKey, Share, MessageToSign) || Share <- Signatures])),
    {ok, Sig} = tpke_pubkey:combine_signature_shares(PubKey, dealer:random_n(T, Signatures), MessageToSign),
    ?assert(tpke_pubkey:verify_signature(PubKey, Sig, MessageToSign)),

    Message = crypto:hash(sha256, <<"my hovercraft is full of eels">>),
    CipherText = tpke_pubkey:encrypt(PubKey, Message),
    ?assert(tpke_pubkey:verify_ciphertext(PubKey, CipherText)),
    Shares = [ tpke_privkey:decrypt_share(SK, CipherText) || SK <- PrivateKeys ],
    ct:pal("Decrypted shares ~p", [Shares]),
    ?assert(lists:all(fun(X) -> X end, [tpke_pubkey:verify_share(PubKey, Share, CipherText) || Share <- Shares])),
    ?assertEqual(Message, tpke_pubkey:combine_shares(PubKey, CipherText, dealer:random_n(T, Shares))),

    ?assertEqual(N, length(sets:to_list(ConvergedResults))),
    ok.
