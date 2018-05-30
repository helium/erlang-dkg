-module(dkg_bipolynomial_test).

-include_lib("eunit/include/eunit.hrl").

self_subtract_test() ->
    Pairing = erlang_pbc:group_new('SS512'),
    Poly = dkg_bipolynomial:generate(Pairing, 5),
    %% subtracting a polynomial from itself should yield all 0s
    ZeroPoly = dkg_bipolynomial:sub(Poly, Poly),

    %% polynomial should trim any trailing empty fields (ie all of them!)
    ?assertEqual(0, tuple_size(ZeroPoly)),
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
