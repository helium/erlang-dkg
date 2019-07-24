-module(dkg_hybridvss_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("relcast/include/fakecast.hrl").

-export([all/0, init_per_testcase/2, end_per_testcase/2]).
-export([
         symmetric_test/1,
         asymmetric_test/1,
         fake_symmetric/1,
         fake_asymmetric/1
        ]).

all() ->
    [
     symmetric_test,
     asymmetric_test,
     fake_symmetric,
     fake_asymmetric
    ].

init_per_testcase(_, Config) ->
    N = list_to_integer(os:getenv("N", "10")),
    T = ((N - 1) div 3) - 1,
    F = 1,
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

-record(state,
        {
         node_count :: integer(),
         results = sets:new() :: sets:set()
        }).

fake_symmetric(Config) ->
    N = proplists:get_value(n, Config),
    F = proplists:get_value(f, Config),
    T = proplists:get_value(t, Config),
    Curve = 'SS512',
    {G1, G2} = dkg_test_utils:generate(Curve),

    TestArgs = [N, F, T, G1, G2, {1, <<0>>}, false, fun(_) -> <<"lol">> end, fun(_, _, _) -> true end],
    Init = fun() ->
                   {ok,
                    #fc_conf{
                       test_mod = dkg_hybridvss,
                       nodes = [aaa, bbb, ccc, ddd, eee, fff, ggg, hhh, iii, jjj],  %% are names useful?
                       configs = [[Id] ++ TestArgs || Id <- lists:seq(1, N)],
                       id_start = 1
                      },
                    #state{node_count = 10}
                   }
           end,

    Secret = erlang_pbc:element_random(erlang_pbc:element_new('Zr', G1)),


    ok = run_fake(Init, fun model/7, os:timestamp(), [{1, Secret}],
                  N, F, T, Curve, G1, G2, Secret).

%% this model is trivial, we only want to catch outputs and never
%% change anything about the execution
model(_Message, _From, To, _NodeState, _NewState, {result, Result},
      #state{results = Results0} = State) ->
    Results = sets:add_element({result, {To, Result}}, Results0),
    %% ct:pal("results len ~p ~p", [sets:size(Results), sets:to_list(Results)]),
    case sets:size(Results) == State#state.node_count of
        true ->
            {result, Results};
        false ->
            {actions, [], State#state{results = Results}}
    end;
model(_Message, _From, _To, _NodeState, _NewState, _Actions, ModelState) ->
    {actions, [], ModelState}.


fake_asymmetric(Config) ->
    N = proplists:get_value(n, Config),
    F = proplists:get_value(f, Config),
    T = proplists:get_value(t, Config),
    Curve = 'MNT224',
    {G1, G2} = dkg_test_utils:generate(Curve),
    TestArgs = [N, F, T, G1, G2, {1, <<0>>}, false, fun(_) -> <<"lol">> end, fun(_, _, _) -> true end],
    Init = fun() ->
                   {ok,
                    #fc_conf{
                       test_mod = dkg_hybridvss,
                       nodes = [aaa, bbb, ccc, ddd, eee, fff, ggg, hhh, iii, jjj],  %% are names useful?
                       configs = [[Id] ++ TestArgs || Id <- lists:seq(1, N)],
                       id_start = 1
                      },
                    #state{node_count = 10}
                   }
           end,

    Secret = erlang_pbc:element_random(erlang_pbc:element_new('Zr', G1)),


    ok = run_fake(Init, fun model/7, os:timestamp(), [{1, Secret}],
                  N, F, T, Curve, G1, G2, Secret).

run(Module, N, F, T, Curve, G1, G2) ->

    [Dealer | Rest] = [ Module:init(Id, N, F, T, G1, G2, {1, <<0>>}, false, fun(_) -> <<"lol">> end, fun(_, _, _) -> true end) || Id <- lists:seq(1, N) ],

    Secret = erlang_pbc:element_random(erlang_pbc:element_new('Zr', G1)),

    {NewDealerState, {send, MsgsToSend}} = Module:input(Dealer, Secret),

    States = [NewDealerState | Rest],
    StatesWithId = lists:zip(lists:seq(1, length(States)), States),
    {_FinalStates, ConvergedResults} = dkg_test_utils:do_send_outer(Module, [{1, {send, MsgsToSend}}], StatesWithId, sets:new()),
    verify_results(Secret, ConvergedResults, N, F, T, Curve, G1, G2).

run_fake(Init, Model, Seed, Input, N, F, T, Curve, G1, G2, Secret) ->

    {ok, Results} = fakecast:start_test(Init, Model, Seed, Input),

    ct:pal("results ~p", [Results]),

    verify_results(Secret, Results, N, F, T, Curve, G1, G2).

verify_results(Secret, ConvergedResults, N, F, T, Curve, G1, G2) ->

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
