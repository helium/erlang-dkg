-module(dkg_bipolynomial_SUITE).
-compile({no_auto_import,[evaluate/2]}).

-include_lib("eunit/include/eunit.hrl").

-export([all/0, init_per_testcase/2, end_per_testcase/2]).

-export([generate_with_constant_term_test/1,
         self_subtract_test/1,
         add_zero_test/1,
         subtract_zero_test/1,
         add_different_sizes_test/1,
         negative_comparison_test/1,
         evaluate_test/1]).

all() ->
    [generate_with_constant_term_test,
     self_subtract_test,
     add_zero_test,
     subtract_zero_test,
     add_different_sizes_test,
     negative_comparison_test,
     evaluate_test].

init_per_testcase(_, Config) ->
    Pairing = erlang_pbc:group_new('SS512'),
    [{pairing, Pairing} | Config].

end_per_testcase(_, Config) ->
    Config.

generate_with_constant_term_test(Config) ->
    Pairing = proplists:get_value(pairing, Config),
    FortyTwo = erlang_pbc:element_set(erlang_pbc:element_new('Zr', Pairing), 42),
    Poly = dkg_bipolynomial:generate(Pairing, 5, FortyTwo),
    ?assertEqual(FortyTwo, dkg_bipolynomial:lookup([1, 1], Poly)).

self_subtract_test(Config) ->
    Pairing = proplists:get_value(pairing, Config),
    Poly = dkg_bipolynomial:generate(Pairing, 5),
    ct:pal("Poly: ~p~n", [dkg_bipolynomial:print(Poly)]),
    %% subtracting a polynomial from itself should yield all 0s
    ZeroPoly = dkg_bipolynomial:sub(Poly, Poly),
    ct:pal("ZeroPoly: ~p~n", [dkg_bipolynomial:print(ZeroPoly)]),
    %% polynomial should trim any trailing empty fields (ie all of them!)
    ?assertEqual(0, tuple_size(ZeroPoly)),
    ok.

add_zero_test(Config) ->
    Pairing = proplists:get_value(pairing, Config),
    Poly = dkg_bipolynomial:generate(Pairing, 5),
    ZeroPoly = dkg_bipolynomial:sub(Poly, Poly),
    %% adding a zeropolynomial to polynomial should be the same polynomial
    ZeroAddedPoly = dkg_bipolynomial:add(Poly, ZeroPoly),
    ct:pal("Poly: ~p~n", [Poly]),
    ct:pal("ZeroAddedPoly: ~p~n", [ZeroAddedPoly]),
    ?assert(dkg_bipolynomial:cmp(Poly, ZeroAddedPoly)),
    ok.

add_different_sizes_test(Config) ->
    Pairing = proplists:get_value(pairing, Config),
    PolyA = dkg_bipolynomial:generate(Pairing, 5),
    PolyB = dkg_bipolynomial:generate(Pairing, 8),
    AddedPoly = dkg_bipolynomial:add(PolyA, PolyB),
    %% result should be of degree 8
    ?assertEqual(8, dkg_bipolynomial:degree(AddedPoly)),
    %% if we subtract B from the result, we should get back A with degree 5
    SubtractedPoly = dkg_bipolynomial:sub(AddedPoly, PolyB),
    ?assertEqual(5, dkg_bipolynomial:degree(SubtractedPoly)),
    ?assert(dkg_bipolynomial:cmp(PolyA, SubtractedPoly)),
    ok.

subtract_zero_test(Config) ->
    Pairing = proplists:get_value(pairing, Config),
    Poly = dkg_bipolynomial:generate(Pairing, 5),
    ZeroPoly = dkg_bipolynomial:sub(Poly, Poly),
    %% subtracting a zeropolynomial to polynomial should be the same polynomial
    ZeroSubtractedPoly = dkg_bipolynomial:sub(Poly, ZeroPoly),
    ct:pal("Poly: ~p~n", [Poly]),
    ct:pal("ZeroSubtractedPoly: ~p~n", [ZeroSubtractedPoly]),
    ?assert(dkg_bipolynomial:cmp(Poly, ZeroSubtractedPoly)),
    ok.

evaluate_test(Config) ->
    Pairing = proplists:get_value(pairing, Config),
    Five = erlang_pbc:element_set(erlang_pbc:element_new('Zr', Pairing), 5),
    Six = erlang_pbc:element_set(erlang_pbc:element_new('Zr', Pairing), 6),
    BiPoly = dkg_bipolynomial:generate(Pairing, 5),

    PolyA = dkg_bipolynomial:evaluate(BiPoly, Five),
    PolyB = dkg_bipolynomial:evaluate(BiPoly, Six),

    ResultA = dkg_polynomial:evaluate(PolyA, Six),
    ResultB = dkg_polynomial:evaluate(PolyB, Five),
    ?assert(erlang_pbc:element_cmp(ResultA, ResultB)),
    ok.

negative_comparison_test(Config) ->
    Pairing = proplists:get_value(pairing, Config),
    PolyA = dkg_bipolynomial:generate(Pairing, 5),
    PolyB = dkg_bipolynomial:add(PolyA, PolyA),
    %% since PolyA /= 2*PolyA
    ?assertEqual(false, dkg_bipolynomial:cmp(PolyA, PolyB)),
    ok.
