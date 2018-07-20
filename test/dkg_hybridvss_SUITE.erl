-module(dkg_hybridvss_SUITE).

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
    T = 2,
    Ph = 0,
    Module = dkg_hybridvss,
    [{n, N}, {f, F}, {module, Module}, {t, T}, {ph, Ph} | Config].

end_per_testcase(_, _Config) ->
    ok.

init_test(Config) ->
    N = proplists:get_value(n, Config),
    F = proplists:get_value(f, Config),
    T = proplists:get_value(t, Config),
    Ph = proplists:get_value(ph, Config),
    Module = proplists:get_value(module, Config),

    [Dealer | Rest] = [ Module:init(Id, N, F, T, Ph) || Id <- lists:seq(1, N-1) ],

    {NewDealerState, {send, MsgsToSend}} = Module:handle_msg(Dealer, 1, share),

    States = [NewDealerState | Rest],
    StatesWithId = lists:zip(lists:seq(1, length(States)), States),
    {_FinalStates, _ConvergedResults} = dkg_test_utils:do_send_outer(Module, [{1, {send, MsgsToSend}}], StatesWithId, sets:new()),
    ct:pal("ConvergedResults ~p", [_ConvergedResults]),
    ?assert(false),
    ok.
