-module(dkg_lagrange).

-export([interpolate/2, apply/2]).

%% interpolate should give back a lagrange polynomial
interpolate(Pairing, Ac) ->
    %% Ac = [{m0, a0}, {m1, a1}, {m2, a2}]
    Ms = [M_j || {M_j, _} <- Ac],
    Zero = erlang_pbc:element_set(erlang_pbc:element_new('Zr', Pairing), 0),
    BasisPolys = [ erlang_pbc:element_mul(Alpha_j, basis_polynomial(Pairing, Ms, J)) || {J, {_, Alpha_j}} <- enumerate(Ac) ],
    lists:foldl(fun(V, Acc) ->
                        erlang_pbc:element_add(Acc, V)
                end, Zero, BasisPolys).

basis_polynomial(Pairing, Ms, J) ->
    One = erlang_pbc:element_set(erlang_pbc:element_new('Zr', Pairing), 1),
    Num = lists:foldl(fun(E, Acc) ->
                              erlang_pbc:element_mul(Acc, E)
                      end, One, [ 0 - M  - 1 || M <- Ms, M /= J]),
    Den = lists:foldl(fun(E, Acc) ->
                              erlang_pbc:element_mul(Acc, E)
                      end, One, [ J - M  || M <- Ms, M /= J]),
    erlang_pbc:element_div(Num, Den).

apply(LagrangePoly, J) ->
    dkg_polynomial:apply(LagrangePoly, J).

enumerate(List) ->
    lists:zip(lists:seq(0, length(List) - 1), List).
