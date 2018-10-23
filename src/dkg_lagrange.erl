-module(dkg_lagrange).

-export([interpolate/3,
         coefficients/2,
         evaluate_g1/2,
         evaluate_zr/2]).

-type indices() :: [integer(),...] | [erlang_pbc:element(),...].

-spec interpolate(dkg_polynomial:polynomial(), indices() , erlang_pbc:element()) -> erlang_pbc:element().
interpolate(Poly, Indices, Alpha) ->
    Coefficients = coefficients(Indices, Alpha),
    Shares = [ dkg_polynomial:evaluate(Poly, Index)  || Index <- lists:seq(1, length(Indices)) ],
    %% NOTE: interpolate is done on G1 since the same group is used in the
    %% public_key_shares in the dkg_commitmentmatrix
    dkg_lagrange:evaluate_g1(Coefficients, Shares).

-spec coefficients(indices(), erlang_pbc:element()) -> dkg_polynomial:polynomial().
coefficients(Indices, Alpha) ->
    One = erlang_pbc:element_set(erlang_pbc:element_new('Zr', Alpha), 1),
    IndicesWithIndex = enumerate(Indices),
    lists:reverse(lists:foldl(fun({I, Index}, Acc1) ->
                                      Num = lists:foldl(fun(E, Acc) ->
                                                                erlang_pbc:element_mul(Acc, E)
                                                        end, One, [ erlang_pbc:element_sub(Idx, Alpha) || {J, Idx} <- IndicesWithIndex, J /= I]),

                                      Den = lists:foldl(fun(E, Acc) ->
                                                                erlang_pbc:element_mul(Acc, E)
                                                        end, One, [ erlang_pbc:element_sub(Idx, Index)  || {J, Idx} <- IndicesWithIndex, J /= I]),
                                      [erlang_pbc:element_div(Num, Den) | Acc1]
                              end, [], IndicesWithIndex)).

-spec evaluate_g1(dkg_polynomial:polynomial(), [erlang_pbc:element(), ...]) -> erlang_pbc:element().
evaluate_g1(Coefficients, Shares) ->
    One = erlang_pbc:element_set(erlang_pbc:element_new('G2', hd(Coefficients)), 1),
    lists:foldl(fun({Coefficient, Share}, Acc) ->
                        erlang_pbc:element_mul(Acc, erlang_pbc:element_pow(Share, Coefficient))
                end, One, lists:zip(Coefficients, Shares)).

-spec evaluate_zr(dkg_polynomial:polynomial(), [erlang_pbc:element(), ...]) -> erlang_pbc:element().
evaluate_zr(Coefficients, Shares) ->
    Zero = erlang_pbc:element_set(erlang_pbc:element_new('Zr', hd(Coefficients)), 0),
    lists:foldl(fun({Coefficient, Share}, Acc) ->
                        erlang_pbc:element_add(Acc, erlang_pbc:element_mul(Share, Coefficient))
                end, Zero, lists:zip(Coefficients, Shares)).

enumerate(List) ->
    lists:zip(lists:seq(1, length(List)), List).
