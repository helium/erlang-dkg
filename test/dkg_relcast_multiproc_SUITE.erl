-module(dkg_relcast_multiproc_SUITE).

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

init_per_testcase(TestCase, Config) ->
    N = list_to_integer(os:getenv("N", "10")),
    T = ((N - 1) div 3) - 1,
    F = 1,
    Module = dkg_hybriddkg,
    Round = <<0>>,
    DataDir = atom_to_list(?MODULE) ++ atom_to_list(TestCase) ++ "data",
    [{n, N}, {f, F}, {t, T}, {round, Round}, {module, Module}, {data_dir, DataDir} | Config].

end_per_testcase(_, _Config) ->
    ok.

symmetric_test(Config) ->
    N = proplists:get_value(n, Config),
    F = proplists:get_value(f, Config),
    T = proplists:get_value(t, Config),
    R = proplists:get_value(round, Config),
    DataDir = proplists:get_value(data_dir, Config),
    run(N, F, T, R, DataDir).

run(N, F, T, R, DataDir) ->

    %fprof:trace([start, {procs, all}]),
    fprof:trace(start),
    Workers = [ element(2, dkg_relcast_worker:start_link([{id, Id},
                                                          {n, N},
                                                          {f, F},
                                                          {t, T},
                                                          {data_dir, DataDir},
                                                          {round, R},
                                                          {callback, true}])) || Id <- lists:seq(1, N) ],

    [ dkg_relcast_worker:start_round(Worker) || Worker <- Workers ],
    %% wait for DKG to complete
    ok = dkg_ct_utils:wait_until(fun() ->
                                         lists:all(fun(W) -> dkg_relcast_worker:is_done(W) end, Workers)
                                 end, 60*2, 500),

    fprof:trace(stop),
    %fprof:trace([stop]),
    fprof:profile(),
    fprof:analyse([totals, {dest, "/tmp/dkg.analysis"}]),
    fprof:stop(),

    %% check everyone agreed on the same public key
    Keys = [ dkg_relcast_worker:get_pubkey(W) || W <- Workers],
    true = lists:all(fun(X) -> tc_public_key_set:serialize(X) == tc_public_key_set:serialize(hd(Keys)) end, Keys),
    PubKey = tc_public_key_set:public_key(hd(Keys)),
    PubKeySet = hd(Keys),

    %% check threshold signatures work
    MessageToSign= crypto:hash(sha256, crypto:strong_rand_bytes(12)),
    Signatures = [ dkg_relcast_worker:sign_share(W, MessageToSign) || W <- Workers],
    ?assert(lists:all(fun(X) -> X end, [tc_public_key_share:verify_signature_share(tc_public_key_set:public_key_share(PubKeySet, I), Share, MessageToSign) || {I, Share} <- Signatures])),
    {ok, CombinedSignature} = tc_public_key_set:combine_signatures(PubKeySet, dkg_ct_utils:random_n(T+1, Signatures)),
    ?assert(tc_pubkey:verify(PubKey, CombinedSignature, MessageToSign)),

    %% check threshold decryption works
    Message = crypto:hash(sha256, <<"my hovercraft is full of eels">>),
    CipherText = tc_pubkey:encrypt(PubKey, Message),
    true = tc_ciphertext:verify(CipherText),
    DecShares = [ dkg_relcast_worker:dec_share(W, CipherText) || W <- Workers],
    ?assert(lists:all(fun(E) -> E end, [tc_public_key_share:verify_decryption_share(tc_public_key_set:public_key_share(PubKeySet, NodeID), DecShare, CipherText) || {NodeID, DecShare} <- DecShares])),
    ?assertEqual({ok, Message}, tc_public_key_set:decrypt(PubKeySet, dkg_ct_utils:random_n(T+1, DecShares), CipherText)),
    ok.
