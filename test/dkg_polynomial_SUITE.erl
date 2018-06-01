-module(dkg_polynomial_SUITE).
-compile({no_auto_import,[apply/2]}).

-include_lib("eunit/include/eunit.hrl").

-export([all/0, init_per_testcase/2, end_per_testcase/2]).

-export([constant_term_test/1,
         self_subtract_test/1,
         self_add_test/1,
         mul_by_zero_test/1,
         mul_by_one_test/1,
         f_of_x_test/1]).

all() ->
    [constant_term_test,
     self_subtract_test,
     self_add_test,
     mul_by_one_test,
     mul_by_zero_test,
     f_of_x_test].

init_per_testcase(_, Config) ->
    Pairing = erlang_pbc:group_new('SS512'),
    [{pairing, Pairing} | Config].

end_per_testcase(_, Config) ->
    Config.

constant_term_test(Config) ->
    Pairing = proplists:get_value(pairing, Config),
    PolyA = dkg_polynomial:generate(Pairing, 5, 91),
    PolyB = dkg_polynomial:generate(Pairing, 5, 91),
    PolyC = dkg_polynomial:sub(PolyA, PolyB),
    NinetyOne = erlang_pbc:element_set(erlang_pbc:element_new('Zr', Pairing), 91),
    %% first term should now be 0
    ?assert(erlang_pbc:element_is0(hd(PolyC))),
    ?assertEqual("0", hd(dkg_polynomial:print(PolyC))),
    ?assert(erlang_pbc:element_cmp(hd(PolyA), NinetyOne)),
    ?assert(erlang_pbc:element_cmp(hd(PolyB), NinetyOne)).

self_subtract_test(Config) ->
    Pairing = proplists:get_value(pairing, Config),
    Poly = dkg_polynomial:generate(Pairing, 5),
    %% subtracting a polynomial from itself should yield all 0s
    ZeroPoly = dkg_polynomial:sub(Poly, Poly),

    %% polynomial should trim any trailing empty fields (ie all of them!)
    ?assertEqual(0, length(ZeroPoly)),

    ?assert(lists:all(fun erlang_pbc:element_is0/1, ZeroPoly)).

self_add_test(Config) ->
    Pairing = proplists:get_value(pairing, Config),
    Poly = dkg_polynomial:generate(Pairing, 5),
    DoublePoly = dkg_polynomial:add(Poly, Poly),
    %% check the length remains the same
    ?assertEqual(6, length(Poly)),
    ?assertEqual(6, length(DoublePoly)),
    %% check that each element is double the original
    ?assert(lists:all(fun({A, B}) -> erlang_pbc:element_cmp(A, erlang_pbc:element_div(B, 2)) end,
                           lists:zip(Poly, DoublePoly))).

mul_by_zero_test(Config) ->
    Pairing = proplists:get_value(pairing, Config),
    Poly = dkg_polynomial:generate(Pairing, 5),
    MulPoly = dkg_polynomial:mul(Poly, [erlang_pbc:element_set(erlang_pbc:element_new('Zr', Pairing), 0)]),
    %% check the length remains the same
    ?assertEqual(6, length(Poly)),
    ?assertEqual(0, length(MulPoly)),
    %% check that each element is zero
    ?assert(lists:all(fun erlang_pbc:element_is0/1, MulPoly)).

mul_by_one_test(Config) ->
    Pairing = proplists:get_value(pairing, Config),
    Poly = dkg_polynomial:generate(Pairing, 5),
    MulPoly = dkg_polynomial:mul(Poly, [erlang_pbc:element_set(erlang_pbc:element_new('Zr', Pairing), 1)]),
    %% check the length remains the same
    ?assertEqual(6, length(Poly)),
    ?assertEqual(6, length(MulPoly)),
    %% check that each element is the same the original
    ?assert(lists:all(fun({A, B}) -> erlang_pbc:element_cmp(A, B) end,
                           lists:zip(Poly, MulPoly))).

f_of_x_test(Config) ->
    Pairing = proplists:get_value(pairing, Config),
    Five = erlang_pbc:element_set(erlang_pbc:element_new('Zr', Pairing), 5),
    Six = erlang_pbc:element_set(erlang_pbc:element_new('Zr', Pairing), 6),

    Poly = dkg_polynomial:generate(Pairing, 2, Six, Five),
    Ans = dkg_polynomial:apply(Poly, Six),

    ?assert(erlang_pbc:element_cmp(Five, Ans)).
