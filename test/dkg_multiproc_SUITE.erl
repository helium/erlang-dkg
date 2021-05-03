-module(dkg_multiproc_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-export([all/0, init_per_testcase/2, end_per_testcase/2]).
-export([
         symmetric_test/1
        ]).

all() ->
    [
     symmetric_test
    ].

init_per_testcase(_, Config) ->
    N = list_to_integer(os:getenv("N", "10")),
    T = ((N - 1) div 3) - 1,
    F = 1,
    Module = dkg_hybriddkg,
    [{n, N}, {f, F}, {t, T}, {module, Module} | Config].

end_per_testcase(_, _Config) ->
    ok.

symmetric_test(Config) ->
    N = proplists:get_value(n, Config),
    F = proplists:get_value(f, Config),
    T = proplists:get_value(t, Config),
    run(N, F, T).

run(N, F, T) ->

    %fprof:trace(start),
    Workers = [ element(2, dkg_worker:start_link(Id, N, F, T, <<0>>)) || Id <- lists:seq(1, N) ],

    [ dkg_worker:start_round(Worker) || Worker <- Workers ],
    %% wait for DKG to complete
    ok = dkg_ct_utils:wait_until(fun() ->
                                         lists:all(fun(W) -> dkg_worker:is_done(W) end, Workers)
                                 end, 60*2, 500),

    %fprof:trace(stop),
    %fprof:profile(),
    %fprof:analyse([totals, {dest, "/tmp/dkg.analysis"}]),
    %fprof:stop(),

    %% check everyone agreed on the same public key
    Keys = [ dkg_worker:get_pubkey(W) || W <- Workers],
    true = lists:all(fun(X) -> tc_pubkey:cmp(X, hd(Keys)) end, Keys),
    PubKey = hd(Keys),

    %% check threshold signatures work
    MessageToSign= crypto:hash(sha256, crypto:strong_rand_bytes(12)),
    Signatures = [ dkg_worker:sign_share(W, MessageToSign) || W <- Workers],
    ?assert(lists:all(fun(X) -> X end, [dkg_worker:verify_signature_share(hd(Workers), Share, MessageToSign) || Share <- Signatures])),
    {ok, CombinedSignature} = dkg_worker:combine_signature_shares(hd(Workers), dkg_ct_utils:random_n(T+1, Signatures)),
    ?assert(tc_pubkey:verify(PubKey, CombinedSignature, MessageToSign)),

    %% check threshold decryption works
    Message = crypto:hash(sha256, <<"my hovercraft is full of eels">>),
    CipherText = tc_pubkey:encrypt(PubKey, Message),
    true = tc_ciphertext:verify(CipherText),
    DecShares = [ dkg_worker:dec_share(W, CipherText) || W <- Workers],
    ?assert(lists:all(fun(E) -> E end, [dkg_worker:verify_decryption_share(hd(Workers), DecShare, CipherText) || DecShare <- DecShares])),
    ?assertEqual({ok, Message}, dkg_worker:combine_decryption_shares(hd(Workers), dkg_ct_utils:random_n(T+1, DecShares), CipherText)),
    ok.
