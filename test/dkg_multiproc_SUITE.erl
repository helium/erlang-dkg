-module(dkg_multiproc_SUITE).

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
    Module = dkg_hybriddkg,
    [{n, N}, {f, F}, {t, T}, {module, Module} | Config].

end_per_testcase(_, _Config) ->
    ok.

symmetric_test(Config) ->
    N = proplists:get_value(n, Config),
    F = proplists:get_value(f, Config),
    T = proplists:get_value(t, Config),
    {G1, G2} = dkg_test_utils:generate('SS512'),
    run(N, F, T, 'SS512', G1, G2).

asymmetric_test(Config) ->
    N = proplists:get_value(n, Config),
    F = proplists:get_value(f, Config),
    T = proplists:get_value(t, Config),
    {G1, G2} = dkg_test_utils:generate('MNT224'),
    run(N, F, T, 'MNT224', G1, G2).

run(N, F, T, Curve, G1, G2) ->
    Workers = [ element(2, dkg_worker:start_link(Id, N, F, T, Curve, G1, G2, 0)) || Id <- lists:seq(1, N) ],

    [ dkg_worker:start_round(Worker) || Worker <- Workers ],
    %% wait for DKG to complete
    ok = dkg_ct_utils:wait_until(fun() ->
                                         lists:all(fun(W) -> dkg_worker:is_done(W) end, Workers)
                                 end, 60*2, 500),
    %% check everyone agreed on the same public key
    Keys = [ dkg_worker:get_pubkey(W) || W <- Workers],
    true = lists:all(fun(X) -> tpke_pubkey:serialize(X) == tpke_pubkey:serialize(hd(Keys)) end, Keys),
    PubKey = hd(Keys),

    case erlang_pbc:pairing_is_symmetric(G1) of
        true ->
            %% check threshold signatures work
            Msg = crypto:hash(sha256, crypto:strong_rand_bytes(12)),
            MessageToSign = tpke_pubkey:hash_message(PubKey, Msg),
            Signatures = [ dkg_worker:sign_share(W, MessageToSign) || W <- Workers],
            ?assert(lists:all(fun(X) -> X end, [tpke_pubkey:verify_signature_share(PubKey, Share, MessageToSign) || Share <- Signatures])),
            {ok, Sig} = tpke_pubkey:combine_signature_shares(PubKey, dealer:random_n(T+1, Signatures), MessageToSign),
            ?assert(tpke_pubkey:verify_signature(PubKey, Sig, MessageToSign));
        false ->
            ok
    end.
