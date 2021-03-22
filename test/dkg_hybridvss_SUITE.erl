-module(dkg_hybridvss_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("relcast/include/fakecast.hrl").

-export([all/0, init_per_testcase/2, end_per_testcase/2]).
-export([
         symmetric_test/1,
         fake_symmetric/1
        ]).

all() ->
    [
     symmetric_test,
     fake_symmetric
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
    run(Module, N, F, T).

-record(state,
        {
         node_count :: integer(),
         results = sets:new() :: sets:set()
        }).

fake_symmetric(Config) ->
    N = proplists:get_value(n, Config),
    F = proplists:get_value(f, Config),
    T = proplists:get_value(t, Config),

    TestArgs = [N, F, T, {1, <<0>>}, false, fun(_) -> <<"lol">> end, fun(_, _, _) -> true end],
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

    Secret = rand:uniform(4096),

    ok = run_fake(Init, fun model/7, os:timestamp(), [{1, Secret}], T).

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

run(Module, N, F, T) ->

    [Dealer | Rest] = [ Module:init(Id, N, F, T, {1, <<0>>}, false, fun(_) -> <<"lol">> end, fun(_, _, _) -> true end) || Id <- lists:seq(1, N) ],

    Secret = 1234,

    {NewDealerState, {send, MsgsToSend}} = Module:input(Dealer, Secret),

    States = [NewDealerState | Rest],
    StatesWithId = lists:zip(lists:seq(1, length(States)), States),
    {_FinalStates, ConvergedResults} = dkg_test_utils:do_send_outer(Module, [{1, {send, MsgsToSend}}], StatesWithId, sets:new()),
    verify_results(ConvergedResults, T).

run_fake(Init, Model, Seed, Input, T) ->

    {ok, Results} = fakecast:start_test(Init, Model, Seed, Input),

    ct:pal("results ~p", [Results]),

    verify_results(Results, T).

verify_results(ConvergedResults, T) ->

    %% check that the shares from nodes can be interpolated to calculate the original secret back
    NodesAndShares = lists:foldl(fun({result, {Node, {_Session, _Commitment, Share}}}, Acc) ->
                                        maps:put(Node, Share, Acc)
                                end, #{}, sets:to_list(ConvergedResults)),

    AllCommitments = [Commitment || {result, {_Node, {_Session, Commitment, _Share}}} <- sets:to_list(ConvergedResults)],
    OutputCommitment = hd(AllCommitments),

    PublicKeySet = dkg_commitment:public_key_set(OutputCommitment),
    PublicKey = tc_public_key_set:public_key(PublicKeySet),

    PrivateKeys = [ {NodeID, tc_secret_key_share:from_fr(Share)} || {NodeID, Share} <- lists:keysort(1, maps:to_list(NodesAndShares))],
    MessageToSign = crypto:hash(sha256, crypto:strong_rand_bytes(12)),
    SignatureShares = [{NodeID - 1, tc_secret_key_share:sign(PrivateKey, MessageToSign)} || {NodeID, PrivateKey} <- PrivateKeys],
    ct:pal("sig results ~p", [[tc_public_key_share:verify_signature_share(tc_public_key_set:public_key_share(PublicKeySet, I), SignatureShare, MessageToSign) || {I, SignatureShare} <- SignatureShares]]),
    ?assert(lists:all(fun(E) -> E end, [tc_public_key_share:verify_signature_share(tc_public_key_set:public_key_share(PublicKeySet, I), SignatureShare, MessageToSign) || {I, SignatureShare} <- SignatureShares])),
    {ok, CombinedSignature} = tc_public_key_set:combine_signatures(PublicKeySet, SignatureShares),
    ?assert(tc_pubkey:verify(PublicKey, CombinedSignature, MessageToSign)),

    Message = crypto:hash(sha256, <<"my hovercraft is full of eels">>),
    CipherText = tc_pubkey:encrypt(PublicKey, Message),
    true = tc_ciphertext:verify(CipherText),
    DecShares = [{NodeID, tc_secret_key_share:decrypt_share(PrivateKey, CipherText)} || {NodeID, PrivateKey} <- PrivateKeys],
    ?assert(lists:all(fun(E) -> E end, [tc_public_key_share:verify_decryption_share(tc_public_key_set:public_key_share(PublicKeySet, NodeID-1), DecShare, CipherText) || {NodeID, DecShare} <- DecShares])),
    ?assertEqual({ok, Message}, tc_public_key_set:decrypt(PublicKeySet, [{NodeID-1, S} || {NodeID, S} <- dkg_ct_utils:random_n(T+1, DecShares)], CipherText)),

    true = lists:all(fun(C) -> dkg_commitment:cmp(hd(AllCommitments), C) end, tl(AllCommitments)),

    ok.
