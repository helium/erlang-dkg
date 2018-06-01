-module(dkg_lagrange_SUITE).
-compile({no_auto_import,[apply/2]}).

-include_lib("eunit/include/eunit.hrl").

-export([all/0, init_per_testcase/2, end_per_testcase/2]).
-export([numeric_random_poly_test/1, numeric_fx2_test/1, poly_test/1]).

all() ->
    [numeric_random_poly_test, numeric_fx2_test, poly_test].

init_per_testcase(_, Config) ->
    Config.

end_per_testcase(_, Config) ->
    Config.

numeric_fx2_test(_Config) ->
    Indices = lists:seq(1, 4),
    io:format("Indices: ~p~n", [Indices]),
    Alpha = 5,
    io:format("Alpha: ~p~n", [Alpha]),
    Coefficients = coefficients(Indices, Alpha),
    io:format("Coefficients: ~p~n", [Coefficients]),
    RandomShares = [ math:pow(Index, 2)  || Index <- Indices ],
    io:format("Random Shares: ~p~n", [RandomShares]),
    Applied = apply(Coefficients, RandomShares),
    io:format("Applied: ~p~n", [Applied]),
    Applied = 25.0.

numeric_random_poly_test(_Config) ->
    Indices = lists:seq(1, 4),
    io:format("Indices: ~p~n", [Indices]),
    Alpha = 5,
    io:format("Alpha: ~p~n", [Alpha]),
    Coefficients = coefficients(Indices, Alpha),
    io:format("Coefficients: ~p~n", [Coefficients]),
    %% f(x) = 2x^3 + 7x^2 - 6x +5
    RandomShares = [ (2*math:pow(Index, 3)) + (7*math:pow(Index, 2)) - (6*Index) + 5  || Index <- Indices ],
    io:format("Random Shares: ~p~n", [RandomShares]),
    Applied = apply(Coefficients, RandomShares),
    io:format("Applied: ~p~n", [Applied]),
    Applied = 400.0.

poly_test(_Config) ->
    Pairing = erlang_pbc:group_new('SS512'),
    Poly = dkg_polynomial:generate(Pairing, 2),
    io:format("Poly: ~p~n", [dkg_polynomial:print(Poly)]),
    Indices = [ erlang_pbc:element_set(erlang_pbc:element_new('Zr', Pairing), I) || I <- lists:seq(1, 3) ],
    io:format("Indices: ~p~n", [dkg_polynomial:print(Indices)]),
    Alpha = erlang_pbc:element_set(erlang_pbc:element_new('Zr', Pairing), 4),
    io:format("Alpha: ~p~n", [erlang_pbc:element_to_string(Alpha)]),
    Coefficients = dkg_lagrange:coefficients(Indices, Alpha),
    io:format("Coefficients: ~p~n", [dkg_polynomial:print(Coefficients)]),

    KnownValueAt4 = dkg_polynomial:apply(Poly, 4),
    io:format("KnownValueAt4: ~p~n", [erlang_pbc:element_to_string(KnownValueAt4)]),

    Shares = [ dkg_polynomial:apply(Poly, Index)  || Index <- lists:seq(1, 3) ],
    io:format("Shares: ~p~n", [dkg_polynomial:print(Shares)]),

    Applied = dkg_lagrange:apply_zr(Coefficients, Shares),
    io:format("Applied: ~p~n", [erlang_pbc:element_to_string(Applied)]),

    ?assert(erlang_pbc:element_cmp(KnownValueAt4, Applied)),
    ok.

%% helper functions
coefficients(Indices, Alpha) ->
    One = 1,
    lists:reverse(lists:foldl(fun({I, Index}, Acc1) ->
                                      Num = lists:foldl(fun(E, Acc) ->
                                                                Acc * E
                                                        end, One, [ Idx - Alpha || {J, Idx} <- enumerate(Indices), J /= I]),

                                      Den = lists:foldl(fun(E, Acc) ->
                                                                Acc * E
                                                        end, One, [ Idx - Index  || {J, Idx} <- enumerate(Indices), J /= I]),
                                      [Num/Den | Acc1]
                              end, [], enumerate(Indices))).

apply(Coefficients, Shares) ->
    Zero = 0,
    lists:foldl(fun({Coefficient, Share}, Acc) ->
                        Acc + (Share * Coefficient)
                end, Zero, lists:zip(Coefficients, Shares)).

enumerate(List) ->
    lists:zip(lists:seq(0, length(List) - 1), List).
