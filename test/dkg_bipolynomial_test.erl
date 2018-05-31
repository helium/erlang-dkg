-module(dkg_bipolynomial_test).

-include_lib("eunit/include/eunit.hrl").

generate_with_constant_term_test() ->
    Pairing = erlang_pbc:group_new('SS512'),
    FortyTwo = erlang_pbc:element_set(erlang_pbc:element_new('Zr', Pairing), 42),
    Poly = dkg_bipolynomial:generate(Pairing, 5, FortyTwo),
    ?assertEqual(FortyTwo, dkg_bipolynomial:lookup([1, 1], Poly)).

self_subtract_test() ->
    Pairing = erlang_pbc:group_new('SS512'),
    Poly = dkg_bipolynomial:generate(Pairing, 5),
    io:format("Poly: ~p~n", [dkg_bipolynomial:print(Poly)]),
    %% subtracting a polynomial from itself should yield all 0s
    ZeroPoly = dkg_bipolynomial:sub(Poly, Poly),
    io:format("ZeroPoly: ~p~n", [dkg_bipolynomial:print(ZeroPoly)]),
    %% polynomial should trim any trailing empty fields (ie all of them!)
    ?assertEqual(0, tuple_size(ZeroPoly)),
    ok.

add_zero_test() ->
    Pairing = erlang_pbc:group_new('SS512'),
    Poly = dkg_bipolynomial:generate(Pairing, 5),
    ZeroPoly = dkg_bipolynomial:sub(Poly, Poly),
    %% adding a zeropolynomial to polynomial should be the same polynomial
    ZeroAddedPoly = dkg_bipolynomial:add(Poly, ZeroPoly),
    io:format("Poly: ~p~n", [Poly]),
    io:format("ZeroAddedPoly: ~p~n", [ZeroAddedPoly]),
    ?assertEqual(dkg_bipolynomial:print(Poly), dkg_bipolynomial:print(ZeroAddedPoly)),
    ok.

add_different_sizes_test() ->
    Pairing = erlang_pbc:group_new('SS512'),
    PolyA = dkg_bipolynomial:generate(Pairing, 5),
    PolyB = dkg_bipolynomial:generate(Pairing, 8),
    AddedPoly = dkg_bipolynomial:add(PolyA, PolyB),
    %% result should be of degree 8
    ?assertEqual(8, dkg_bipolynomial:degree(AddedPoly)),
    %% if we subtract B from the result, we should get back A with degree 5
    SubtractedPoly = dkg_bipolynomial:sub(AddedPoly, PolyB),
    ?assertEqual(5, dkg_bipolynomial:degree(SubtractedPoly)),
    ?assertEqual(dkg_bipolynomial:print(PolyA), dkg_bipolynomial:print(SubtractedPoly)),
    ok.


subrtract_zero_test() ->
    Pairing = erlang_pbc:group_new('SS512'),
    Poly = dkg_bipolynomial:generate(Pairing, 5),
    ZeroPoly = dkg_bipolynomial:sub(Poly, Poly),
    %% subtracting a zeropolynomial to polynomial should be the same polynomial
    ZeroSubtractedPoly = dkg_bipolynomial:sub(Poly, ZeroPoly),
    io:format("Poly: ~p~n", [Poly]),
    io:format("ZeroSubtractedPoly: ~p~n", [ZeroSubtractedPoly]),
    ?assertEqual(dkg_bipolynomial:print(Poly), dkg_bipolynomial:print(ZeroSubtractedPoly)),
    ok.

apply_test() ->
    Pairing = erlang_pbc:group_new('SS512'),
    Five = erlang_pbc:element_set(erlang_pbc:element_new('Zr', Pairing), 5),
    Six = erlang_pbc:element_set(erlang_pbc:element_new('Zr', Pairing), 6),
    BiPoly = dkg_bipolynomial:generate(Pairing, 5),

    PolyA = dkg_bipolynomial:apply(BiPoly, Five),
    PolyB = dkg_bipolynomial:apply(BiPoly, Six),

    ResultA = dkg_polynomial:apply(PolyA, Six),
    ResultB = dkg_polynomial:apply(PolyB, Five),
    ?assert(erlang_pbc:element_cmp(ResultA, ResultB)).
