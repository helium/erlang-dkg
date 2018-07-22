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
    Module = proplists:get_value(module, Config),
    Group = erlang_pbc:group_new('SS512'),
    Generator = erlang_pbc:element_from_hash(erlang_pbc:element_new('G1', Group), <<"honeybadger">>),

    [Dealer | Rest] = [ Module:init(Id, N, F, T, Generator, {1, 0}) || Id <- lists:seq(1, N) ],

    Secret = erlang_pbc:element_random(erlang_pbc:element_new('Zr', Generator)),

    {NewDealerState, {send, MsgsToSend}} = Module:input(Dealer, Secret),

    States = [NewDealerState | Rest],
    StatesWithId = lists:zip(lists:seq(1, length(States)), States),
    {_FinalStates, ConvergedResults} = dkg_test_utils:do_send_outer(Module, [{1, {send, MsgsToSend}}], StatesWithId, sets:new()),

    %% check that the shares from nodes can be interpolated to calculate the original secret back
    NodesAndShares = lists:foldl(fun({result, {Node, {_Session, _Commitment, Share}}}, Acc) ->
                                        maps:put(Node, Share, Acc)
                                end, #{}, sets:to_list(ConvergedResults)),

    {Indices0, Elements} = lists:unzip(maps:to_list(NodesAndShares)),
    Indices = [ erlang_pbc:element_set(erlang_pbc:element_new('Zr', hd(Elements)), I) || I <- Indices0 ],
    Shares = lists:foldl(fun(Index, Acc) ->
                                 case maps:is_key(Index, NodesAndShares) of
                                     false ->
                                         %% Node ${Index} has not sent us a share, interpolate it
                                         Alpha = erlang_pbc:element_set(erlang_pbc:element_new('Zr', hd(Elements)), Index),
                                         LagrangePoly = dkg_lagrange:coefficients(Indices, Alpha),
                                         InterpolatedShare = dkg_lagrange:apply_zr(LagrangePoly, Elements),
                                         [ InterpolatedShare | Acc];
                                     true ->
                                         %% Node ${Index} has sent us a share
                                         [ maps:get(Index, NodesAndShares) | Acc]
                                 end
                         end, [], [0 | lists:seq(1,N)]), %% note that we also evaluate at 0

    CalculatedSecret = hd(lists:reverse(Shares)),
    ?assert(erlang_pbc:element_cmp(CalculatedSecret, Secret)),
    ok.