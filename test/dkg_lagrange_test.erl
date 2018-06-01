-module(dkg_lagrange_test).
-compile({no_auto_import,[apply/2]}).

-include_lib("eunit/include/eunit.hrl").

simple_test() ->
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
