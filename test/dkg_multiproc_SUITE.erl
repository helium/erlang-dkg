-module(dkg_multiproc_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-export([all/0, init_per_testcase/2, end_per_testcase/2]).
-export([
         init_test/1,
         mnt224_test/1
        ]).

all() ->
    [
     init_test,
     mnt224_test
    ].

init_per_testcase(_, Config) ->
    N = list_to_integer(os:getenv("N", "10")),
    F = (N - 1) div 3,
    T = F,
    Module = dkg_hybriddkg,
    [{n, N}, {f, F}, {t, T}, {module, Module} | Config].

end_per_testcase(_, _Config) ->
    ok.

init_test(Config) ->
    N = proplists:get_value(n, Config),
    F = proplists:get_value(f, Config),
    T = proplists:get_value(t, Config),
    Group = erlang_pbc:group_new('SS512'),
    G1 = erlang_pbc:element_from_hash(erlang_pbc:element_new('G1', Group), <<"honeybadger">>),
    G2 = G1,

    Workers = [ element(2, dkg_worker:start_link(Id, N, F, T, 'SS512', G1, G2, 0)) || Id <- lists:seq(1, N) ],

    [ dkg_worker:start_round(Worker) || Worker <- Workers ],
    %% wait for DKG to complete
    ok = dkg_ct_utils:wait_until(fun() ->
                                         lists:all(fun(W) -> dkg_worker:is_done(W) end, Workers)
                                 end, 60*2, 500),
    %% check everyone agreed on the same public key
    Keys = [ dkg_worker:get_pubkey(W) || W <- Workers],
    true = lists:all(fun(X) -> tpke_pubkey:serialize(X) == tpke_pubkey:serialize(hd(Keys)) end, Keys),
    PubKey = hd(Keys),

    %% check threshold decryption works
    Message = crypto:hash(sha256, <<"my hovercraft is full of eels">>),
    CipherText = tpke_pubkey:encrypt(PubKey, Message),
    ?assert(tpke_pubkey:verify_ciphertext(PubKey, CipherText)),
    DecShares = [ dkg_worker:dec_share(W, CipherText) || W <- Workers ],
    ?assert(lists:all(fun(X) -> X end, [tpke_pubkey:verify_share(PubKey, Share, CipherText) || Share <- DecShares])),
    ?assertEqual(Message, tpke_pubkey:combine_shares(PubKey, CipherText, dealer:random_n(T+1, DecShares))),

    %% check threshold signatures work
    Msg = crypto:hash(sha256, crypto:strong_rand_bytes(12)),
    MessageToSign = tpke_pubkey:hash_message(PubKey, Msg),
    Signatures = [ dkg_worker:sign_share(W, MessageToSign) || W <- Workers],
    ?assert(lists:all(fun(X) -> X end, [tpke_pubkey:verify_signature_share(PubKey, Share, MessageToSign) || Share <- Signatures])),
    {ok, Sig} = tpke_pubkey:combine_signature_shares(PubKey, dealer:random_n(T+1, Signatures), MessageToSign),
    ?assert(tpke_pubkey:verify_signature(PubKey, Sig, MessageToSign)),

    ok.

mnt224_test(Config) ->
    N = proplists:get_value(n, Config),
    F = proplists:get_value(f, Config),
    T = proplists:get_value(t, Config),
    Group = erlang_pbc:group_new('MNT224'),
    G1 = erlang_pbc:element_from_hash(erlang_pbc:element_new('G1', Group), <<"honeybadger">>),
    G2 = erlang_pbc:element_from_hash(erlang_pbc:element_new('G2', Group), <<"rocks">>),

    Workers = [ element(2, dkg_worker:start_link(Id, N, F, T, 'MNT224', G1, G2, 0)) || Id <- lists:seq(1, N) ],

    [ dkg_worker:start_round(Worker) || Worker <- Workers ],
    %% wait for DKG to complete
    ok = dkg_ct_utils:wait_until(fun() ->
                                         lists:all(fun(W) -> dkg_worker:is_done(W) end, Workers)
                                 end, 60*2, 500),
    %% check everyone agreed on the same public key
    Keys = [ dkg_worker:get_pubkey(W) || W <- Workers],
    true = lists:all(fun(X) -> tpke_pubkey:serialize(X) == tpke_pubkey:serialize(hd(Keys)) end, Keys),
    PubKey = hd(Keys),

    %% check threshold signatures work
    Msg = crypto:hash(sha256, crypto:strong_rand_bytes(12)),
    MessageToSign = tpke_pubkey:hash_message(PubKey, Msg),
    Signatures = [ dkg_worker:sign_share(W, MessageToSign) || W <- Workers],
    ?assert(lists:all(fun(X) -> X end, [tpke_pubkey:verify_signature_share(PubKey, Share, MessageToSign) || Share <- Signatures])),
    {ok, Sig} = tpke_pubkey:combine_signature_shares(PubKey, dealer:random_n(T+1, Signatures), MessageToSign),
    ?assert(tpke_pubkey:verify_signature(PubKey, Sig, MessageToSign)),
    ok.
