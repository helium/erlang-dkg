-module(dkg_polynomial_test).

-include_lib("eunit/include/eunit.hrl").

constant_term_test() ->
    Pairing = erlang_pbc:group_new('SS512'),
    PolyA = dkg_polynomial:generate(Pairing, 5, 91),
    PolyB = dkg_polynomial:generate(Pairing, 5, 91),
    PolyC = dkg_polynomial:sub(PolyA, PolyB),
    NinetyOne = erlang_pbc:element_set(erlang_pbc:element_new('Zr', Pairing), 91),
    %% first term should now be 0
    ?assert(erlang_pbc:element_is0(hd(PolyC))),
    ?assertEqual("0", hd(dkg_polynomial:print(PolyC))),
    ?assert(erlang_pbc:element_cmp(hd(PolyA), NinetyOne)),
    ?assert(erlang_pbc:element_cmp(hd(PolyB), NinetyOne)).

self_subtract_test() ->
    Pairing = erlang_pbc:group_new('SS512'),
    Poly = dkg_polynomial:generate(Pairing, 5),
    %% subtracting a polynomial from itself should yield all 0s
    ZeroPoly = dkg_polynomial:sub(Poly, Poly),

    %% polynomial should trim any trailing empty fields (ie all of them!)
    ?assertEqual(0, length(ZeroPoly)),

    ?assert(lists:all(fun erlang_pbc:element_is0/1, ZeroPoly)).

self_add_test() ->
    Pairing = erlang_pbc:group_new('SS512'),
    Poly = dkg_polynomial:generate(Pairing, 5),
    DoublePoly = dkg_polynomial:add(Poly, Poly),
    %% check the length remains the same
    ?assertEqual(6, length(Poly)),
    ?assertEqual(6, length(DoublePoly)),
    %% check that each element is double the original
    ?assert(lists:all(fun({A, B}) -> erlang_pbc:element_cmp(A, erlang_pbc:element_div(B, 2)) end,
                           lists:zip(Poly, DoublePoly))).

mul_by_zero_test() ->
    Pairing = erlang_pbc:group_new('SS512'),
    Poly = dkg_polynomial:generate(Pairing, 5),
    MulPoly = dkg_polynomial:mul(Poly, [erlang_pbc:element_set(erlang_pbc:element_new('Zr', Pairing), 0)]),
    %% check the length remains the same
    ?assertEqual(6, length(Poly)),
    ?assertEqual(0, length(MulPoly)),
    %% check that each element is zero
    ?assert(lists:all(fun erlang_pbc:element_is0/1, MulPoly)).

mul_by_one_test() ->
    Pairing = erlang_pbc:group_new('SS512'),
    Poly = dkg_polynomial:generate(Pairing, 5),
    MulPoly = dkg_polynomial:mul(Poly, [erlang_pbc:element_set(erlang_pbc:element_new('Zr', Pairing), 1)]),
    %% check the length remains the same
    ?assertEqual(6, length(Poly)),
    ?assertEqual(6, length(MulPoly)),
    %% check that each element is the same the original
    ?assert(lists:all(fun({A, B}) -> erlang_pbc:element_cmp(A, B) end,
                           lists:zip(Poly, MulPoly))).

f_of_x_test() ->
    Pairing = erlang_pbc:group_new('SS512'),
    Five = erlang_pbc:element_set(erlang_pbc:element_new('Zr', Pairing), 5),
    Six = erlang_pbc:element_set(erlang_pbc:element_new('Zr', Pairing), 6),

    Poly = dkg_polynomial:generate(Pairing, 2, Six, Five),
    Ans = dkg_polynomial:apply(Poly, Six),

    ?assert(erlang_pbc:element_cmp(Five, Ans)).
