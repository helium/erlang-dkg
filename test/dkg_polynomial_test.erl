-module(dkg_polynomial_test).

-include_lib("eunit/include/eunit.hrl").

simple_test() ->
    Pairing = erlang_pbc:group_new('SS512'),
    Five = erlang_pbc:element_set(erlang_pbc:element_new('Zr', Pairing), 5),
    Six = erlang_pbc:element_set(erlang_pbc:element_new('Zr', Pairing), 6),

    Poly = dkg_polynomial:generate(Pairing, 2, Six, Five),
    Ans = dkg_polynomial:apply(Poly, Six),

    ?assert(erlang_pbc:element_cmp(Five, Ans)).
