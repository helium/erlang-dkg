-module(dkg_relcast_multiproc_SUITE).

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

init_per_testcase(TestCase, Config) ->
    N = list_to_integer(os:getenv("N", "10")),
    T = ((N - 1) div 3) - 1,
    F = 1,
    Module = dkg_hybriddkg,
    Round = 0,
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
    Curve = 'SS512',
    {G1, G2} = dkg_test_utils:generate(Curve),
    run(N, F, T, R, Curve, G1, G2, DataDir).

asymmetric_test(Config) ->
    N = proplists:get_value(n, Config),
    F = proplists:get_value(f, Config),
    T = proplists:get_value(t, Config),
    R = proplists:get_value(round, Config),
    DataDir = proplists:get_value(data_dir, Config),
    Curve = 'MNT224',
    {G1, G2} = dkg_test_utils:generate(Curve),
    run(N, F, T, R, Curve, G1, G2, DataDir).

run(N, F, T, R, Curve, G1, G2, DataDir) ->
    Workers = [ element(2, dkg_relcast_worker:start_link([{id, Id},
                                                          {n, N},
                                                          {f, F},
                                                          {t, T},
                                                          {curve, Curve},
                                                          {g1, G1},
                                                          {g2, G2},
                                                          {data_dir, DataDir},
                                                          {round, R},
                                                          {callback, true}])) || Id <- lists:seq(1, N) ],

    [ dkg_relcast_worker:start_round(Worker) || Worker <- Workers ],
    %% wait for DKG to complete
    ok = dkg_ct_utils:wait_until(fun() ->
                                         lists:all(fun(W) -> dkg_relcast_worker:is_done(W) end, Workers)
                                 end, 60*2, 500),
    %% check everyone agreed on the same public key
    Keys = [ dkg_relcast_worker:get_pubkey(W) || W <- Workers],
    true = lists:all(fun(X) -> tpke_pubkey:serialize(X) == tpke_pubkey:serialize(hd(Keys)) end, Keys),
    PubKey = hd(Keys),

    case erlang_pbc:pairing_is_symmetric(G1) of
        true ->
            %% check threshold signatures work
            Msg = crypto:hash(sha256, crypto:strong_rand_bytes(12)),
            MessageToSign = tpke_pubkey:hash_message(PubKey, Msg),
            Signatures = [ dkg_relcast_worker:sign_share(W, MessageToSign) || W <- Workers],
            ?assert(lists:all(fun(X) -> X end, [tpke_pubkey:verify_signature_share(PubKey, Share, MessageToSign) || Share <- Signatures])),
            {ok, Sig} = tpke_pubkey:combine_signature_shares(PubKey, dealer:random_n(T+1, Signatures), MessageToSign),
            ?assert(tpke_pubkey:verify_signature(PubKey, Sig, MessageToSign));
        false ->
            ok
    end.
