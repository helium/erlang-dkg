-module(dkg_polynomial_test).

-include_lib("eunit/include/eunit.hrl").

simple_test() ->
    Pairing = erlang_pbc:group_new('SS512'),
    Five = erlang_pbc:element_set(erlang_pbc:element_new('Zr', Pairing), 5),
    io:format("Five: ~p~n", [erlang_pbc:element_to_string(Five)]),
    Six = erlang_pbc:element_set(erlang_pbc:element_new('Zr', Pairing), 6),
    io:format("Six: ~p~n", [erlang_pbc:element_to_string(Six)]),
    Zero = erlang_pbc:element_set(erlang_pbc:element_new('Zr', Pairing), 0),
    io:format("Zero: ~p~n", [erlang_pbc:element_to_string(Zero)]),

    Poly = dkg_polynomial:generate(Pairing, 2, Six, Five),
    io:format("Poly: ~p~n", [dkg_polynomial:print(Poly)]),
    Ans = dkg_polynomial:apply(Poly, Six),
    io:format("Ans: ~p~n", [erlang_pbc:element_to_string(Ans)]),
    io:format("Ans: ~p~n", [erlang_pbc:element_cmp(Five, Ans)]),

    ?assert(false).
