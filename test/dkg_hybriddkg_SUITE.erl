-module(dkg_hybriddkg_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("relcast/include/fakecast.hrl").
-include_lib("public_key/include/public_key.hrl").

-export([all/0, init_per_testcase/2, end_per_testcase/2]).
-export([
         symmetric_test/1,
         asymmetric_test/1,
         leader_change_symmetric_test/1,
         leader_change_asymmetric_test/1,
         split_key_test/1,
         sign_verify_test/1,
         sign_verify_fail_test/1
        ]).

all() ->
    [
     symmetric_test,
     asymmetric_test,
     leader_change_symmetric_test,
     leader_change_asymmetric_test,
     split_key_test,
     sign_verify_test,
     sign_verify_fail_test
    ].

init_per_testcase(_, Config) ->
    N = list_to_integer(os:getenv("N", "10")),
    F = 0,
    T = 3,
    Ph = 0,
    Module = dkg_hybriddkg,
    InitialLeader = 1,
    [{n, N}, {f, F}, {module, Module}, {t, T}, {ph, Ph}, {init_leader, InitialLeader} | Config].

end_per_testcase(_, _Config) ->
    ok.

symmetric_test(Config) ->
    N = proplists:get_value(n, Config),
    F = proplists:get_value(f, Config),
    T = proplists:get_value(t, Config),
    InitialLeader = proplists:get_value(init_leader, Config),
    Module = proplists:get_value(module, Config),
    {G1, G2} = dkg_test_utils:generate('SS512'),
    run(N, F, T, Module, 'SS512', G1, G2, InitialLeader).

asymmetric_test(Config) ->
    N = proplists:get_value(n, Config),
    F = proplists:get_value(f, Config),
    T = proplists:get_value(t, Config),
    InitialLeader = proplists:get_value(init_leader, Config),
    Module = proplists:get_value(module, Config),
    {G1, G2} = dkg_test_utils:generate('MNT224'),
    run(N, F, T, Module, 'MNT224', G1, G2, InitialLeader).

leader_change_symmetric_test(Config) ->
    N = proplists:get_value(n, Config),
    F = proplists:get_value(f, Config),
    T = proplists:get_value(t, Config),
    InitialLeader = 2,
    Module = proplists:get_value(module, Config),
    {G1, G2} = dkg_test_utils:generate('SS512'),
    run(N, F, T, Module, 'SS512', G1, G2, InitialLeader, [{elections, true}, {signfun, fun(_) -> <<"lol">> end}, {verifyfun, fun(_, _, _) -> true end}]).

leader_change_asymmetric_test(Config) ->
    N = proplists:get_value(n, Config),
    F = proplists:get_value(f, Config),
    T = proplists:get_value(t, Config),
    InitialLeader = 2,
    Module = proplists:get_value(module, Config),
    {G1, G2} = dkg_test_utils:generate('MNT224'),
    run(N, F, T, Module, 'MNT224', G1, G2, InitialLeader, [{elections, true}, {signfun, fun(_) -> <<"lol">> end}, {verifyfun, fun(_, _, _) -> true end}]).

sign_verify_test(Config) ->
    N = proplists:get_value(n, Config),
    F = proplists:get_value(f, Config),
    T = proplists:get_value(t, Config),
    Keys = [ begin
                 PrivKey = public_key:generate_key({namedCurve,?secp256r1}),
                 #'ECPrivateKey'{parameters=_Params, publicKey=PubKey} = PrivKey,
                 {PrivKey, {#'ECPoint'{point = PubKey}, {namedCurve, ?secp256r1}}}
             end || _ <- lists:seq(1, N)],

    VerifyFun = fun(Id, Msg, Signature) ->
                        {_, PubKey} = lists:nth(Id, Keys),
                        public_key:verify(Msg, sha256, Signature, PubKey)
                end,
    SigFuns = [ fun(Msg) -> public_key:sign(Msg, sha256, PrivKey) end || {PrivKey, _} <- Keys ],
    InitialLeader = 2,
    Module = proplists:get_value(module, Config),
    {G1, G2} = dkg_test_utils:generate('SS512'),
    run(N, F, T, Module, 'SS512', G1, G2, InitialLeader, [{elections, true}, {signfuns, SigFuns}, {verifyfun, VerifyFun}]).

sign_verify_fail_test(Config) ->
    N = proplists:get_value(n, Config),
    F = proplists:get_value(f, Config),
    T = proplists:get_value(t, Config),
    Keys = [ begin
                 PrivKey = public_key:generate_key({namedCurve,?secp256r1}),
                 #'ECPrivateKey'{parameters=_Params, publicKey=PubKey} = PrivKey,
                 {PrivKey, {#'ECPoint'{point = PubKey}, {namedCurve, ?secp256r1}}}
             end || _ <- lists:seq(1, N)],

    VerifyFun = fun(Id, Msg, Signature) ->
                        %% XXX reverse the key order, so nothing can be verified
                        {_, PubKey} = lists:nth(Id, lists:reverse(Keys)),
                        public_key:verify(Msg, sha256, Signature, PubKey)
                end,
    SigFuns = [ fun(Msg) -> public_key:sign(Msg, sha256, PrivKey) end || {PrivKey, _} <- Keys ],
    InitialLeader = 2,
    Module = proplists:get_value(module, Config),
    {G1, G2} = dkg_test_utils:generate('SS512'),
    ?assertException(error, {assertEqual, _}, run(N, F, T, Module, 'SS512', G1, G2, InitialLeader, [{elections, true}, {signfuns, SigFuns}, {verifyfun, VerifyFun}])).


-record(sk_state,
        {
         node_count :: integer(),
         one_stopped :: boolean(),
         results = sets:new() :: sets:set()
        }).

split_key_test(Config) ->
    N = proplists:get_value(n, Config),
    F = proplists:get_value(f, Config),
    T = proplists:get_value(t, Config),
    {G1, G2} = dkg_test_utils:generate('SS512'),

    BaseConfig = [N, F, T, G1, G2, <<0>>, [{elections, true}, {signfun, fun(_) -> <<"lol">> end}, {verifyfun, fun(_, _, _) -> true end}]],

    Init =
        fun() ->
                {ok,
                 #fc_conf{
                    test_mod = dkg_hybriddkg,
                    nodes = [aaa, bbb, ccc, ddd, eee, fff, ggg, hhh, iii, jjj],  %% are names useful?
                    configs = [[Id] ++ BaseConfig || Id <- lists:seq(1, N)], % pull count from config
                    id_start = 1,
                    max_time = 3000
                 },
                 #sk_state{node_count = N - 1,
                           one_stopped = false}
                }
        end,

    run_fake(Init, fun sk_model/7, os:timestamp(), %% {1543,599707,659249}, % known failure case
             [{Node, ignored} || Node <- lists:seq(1, N)], % input
             N, F, T, 'SS512', G1, G2).

%% this model alters the sequence of events to trigger the split keys
%% condition (in the original code).  it does so by making sure that
%% 2 does not have the share for VSS 4, which means that later when it
%% updates its proposal during the leader change, it will propose Q =
%% [1..10], which will then fail because it doesn't have all of the shares.

%% unhappen all messages from vss 4, to manipulate Q on 2
sk_model({{vss,N},_}, _, 2, NodeState, _NewState, _Actions, ModelState) when N == 4 ->
    {actions, [{alter_state, NodeState}], ModelState};
%% drop all messages between 1 and 2 to make sure that local q isn't polluted
sk_model(_, 1, 2, NodeState, _NewState, _Actions, ModelState) ->
    {actions, [{alter_state, NodeState}], ModelState};
%% when node one gets to the send state, allow it to send out some
%% messages, drop others, and fail it
sk_model(_Msg, _from, 1, _NodeState, _NewState, {send,[{multicast,Send}]},
      #sk_state{one_stopped = false} = ModelState) when element(1, Send) == send ->
    %% fakecast:trace("actions: ~p", [_Ac]),
    NewActions = [{unicast, N, Send} || N <- [2,3,4,5]],
    {actions, [{stop_node, 1}, {alter_actions, {send, NewActions}}],
     ModelState#sk_state{one_stopped = true}};
%% collect results
sk_model(_Message, _From, To, _NodeState, _NewState, {result, Result},
      #sk_state{results = Results0} = State) ->
    Results = sets:add_element({result, {To, Result}}, Results0),
    ct:pal("results len ~p ~p", [sets:size(Results), sets:to_list(Results)]),
    case sets:size(Results) == State#sk_state.node_count of
        true ->
            {result, Results};
        false ->
            {actions, [], State#sk_state{results = Results}}
    end;
%% otherwise continue
sk_model(_Message, _From, _To, _NodeState, _NewState, _Actions, ModelState) ->
    {actions, [], ModelState}.


run(N, F, T, Module, Curve, G1, G2, InitialLeader) ->
    run(N, F, T, Module, Curve, G1, G2, InitialLeader, [{signfun, fun(_) -> <<"lol">> end}, {verifyfun, fun(_, _, _) -> true end}]).

run(N, F, T, Module, Curve, G1, G2, InitialLeader, Options) ->
    {StatesWithId, Replies} = lists:unzip(lists:map(fun(E) ->
                                                            %% check if we have a real, per node, sig fun
                                                            SigFun = case proplists:get_value(signfuns, Options) of
                                                                         undefined ->
                                                                             {signfun, S} = lists:keyfind(signfun, 1, Options),
                                                                             S;
                                                                         SigFuns when is_list(SigFuns) ->
                                                                             lists:nth(E, SigFuns)
                                                                     end,
                                                   {State, {send, Replies}} = Module:start(Module:init(E, N, F, T, G1, G2, <<0>>,
                                                                                                       lists:keystore(signfun, 1, Options, {signfun, SigFun}))),
                                                   {{E, State}, {E, {send, Replies}}}
                                           end, lists:seq(InitialLeader, N))),

    {_FinalStates, ConvergedResults} = dkg_test_utils:do_send_outer(Module, Replies, StatesWithId, sets:new()),
    ?assertEqual(N-InitialLeader+1, length(sets:to_list(ConvergedResults))),
    ct:pal("Results ~p", [sets:to_list(ConvergedResults)]),

    verify_results(ConvergedResults, N, F, T, G1, G2, Curve).


run_fake(Init, Model, Seed, Input, N, F, T, Curve, G1, G2) ->

    {ok, Results} = fakecast:start_test(Init, Model, Seed, Input),

    ct:pal("results ~p", [Results]),

    verify_results(Results, N, F, T, G1, G2, Curve).

verify_results(ConvergedResults, N, F, T, G1, G2, Curve) ->
    %% XXX: this is the same as the pubkeyshare test, I'm sure there is more to it
    SecretKeyShares = lists:keysort(1, [ {Node, SecretKey} || {result, {Node, {SecretKey, _VerificationKey, _VerificationKeys}}} <- sets:to_list(ConvergedResults)]),
    VerificationKeys = lists:keysort(1, [ {Node, VerificationKey} || {result, {Node, {_SecretKey, VerificationKey, _VerificationKeys}}} <- sets:to_list(ConvergedResults)]),
    VerificationKeyss = lists:keysort(1, [ {Node, VerificationKeyz} || {result, {Node, {_SecretKey, _VerificationKey, VerificationKeyz}}} <- sets:to_list(ConvergedResults)]),
    ct:pal("Secret key shares ~p", [[ erlang_pbc:element_to_string(S) || {_, S} <- SecretKeyShares]]),
    ct:pal("Public key shares ~p", [[ {I, erlang_pbc:element_to_string(S)} || {I, S} <- VerificationKeys]]),
    ct:pal("Public key shares ~p", [[ lists:map(fun erlang_pbc:element_to_string/1, S) || {_, S} <- VerificationKeyss]]),
    PublicKeySharePoly = [Share || Share <- element(2, hd(VerificationKeyss))],
    KnownSecret = dkg_polynomial:evaluate(PublicKeySharePoly, 0),
    Indices = [ erlang_pbc:element_set(erlang_pbc:element_new('Zr', G1), I) || I <- lists:seq(1, N) ],
    Alpha = erlang_pbc:element_set(erlang_pbc:element_new('Zr', G1), 0),
    CalculatedSecret = dkg_lagrange:interpolate(PublicKeySharePoly, Indices, Alpha),
    ?assert(erlang_pbc:element_cmp(KnownSecret, CalculatedSecret)),

    %% attempt to construct some TPKE keys...
    PrivateKeys = lists:map(fun({result, {Node, {SK, VK, VKs}}}) ->
                                    PK = tpke_pubkey:init(N, F, G1, G2, VK, VKs, Curve),
                                    tpke_privkey:init(PK, SK, Node-1)
                            end, sets:to_list(ConvergedResults)),
    PubKey = tpke_privkey:public_key(hd(PrivateKeys)),
    Msg = crypto:hash(sha256, crypto:strong_rand_bytes(12)),
    MessageToSign = tpke_pubkey:hash_message(PubKey, Msg),
    Signatures = [ tpke_privkey:sign(PrivKey, MessageToSign) || PrivKey <- PrivateKeys],
    ct:pal("~p", [[tpke_pubkey:verify_signature_share(PubKey, Share, MessageToSign) || Share <- Signatures]]),
    ?assert(lists:all(fun(X) -> X end, [tpke_pubkey:verify_signature_share(PubKey, Share, MessageToSign) || Share <- Signatures])),
    {ok, Sig} = tpke_pubkey:combine_signature_shares(PubKey, dealer:random_n(T+1, Signatures), MessageToSign),
    ?assert(tpke_pubkey:verify_signature(PubKey, Sig, MessageToSign)),

    case erlang_pbc:pairing_is_symmetric(G1) of
        true ->
            %% threshold decryption only works with symmetric curve
            Message = crypto:hash(sha256, <<"my hovercraft is full of eels">>),
            CipherText = tpke_pubkey:encrypt(PubKey, Message),
            ?assert(tpke_pubkey:verify_ciphertext(PubKey, CipherText)),
            Shares = [ tpke_privkey:decrypt_share(SK, CipherText) || SK <- PrivateKeys ],
            ct:pal("Decrypted shares ~p", [Shares]),
            ?assert(lists:all(fun(X) -> X end, [tpke_pubkey:verify_share(PubKey, Share, CipherText) || Share <- Shares])),
            ?assertEqual(Message, tpke_pubkey:combine_shares(PubKey, CipherText, dealer:random_n(T+1, Shares)));
        false ->
            ok
    end.
