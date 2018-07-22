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
    T = 2,
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

    %ct:pal("Replies ~p", [Replies]),

    {_FinalStates, ConvergedResults} = dkg_test_utils:do_send_outer(Module, Replies, StatesWithId, sets:new()),
    ct:pal("Results ~p", [sets:to_list(ConvergedResults)]),
    ?assertEqual(N, length(sets:to_list(ConvergedResults))),
    ok.



