-module(dkg_lagrange).

-export([coefficients/2, apply_g1/2, apply_zr/2]).

coefficients(Indices, Alpha) ->
    One = erlang_pbc:element_set(erlang_pbc:element_new('Zr', Alpha), 1),
    lists:reverse(lists:foldl(fun({I, Index}, Acc1) ->
                                      Num = lists:foldl(fun(E, Acc) ->
                                                                erlang_pbc:element_mul(Acc, E)
                                                        end, One, [ erlang_pbc:element_sub(Idx, Alpha) || {J, Idx} <- enumerate(Indices), J /= I]),

                                      Den = lists:foldl(fun(E, Acc) ->
                                                                erlang_pbc:element_mul(Acc, E)
                                                        end, One, [ erlang_pbc:element_sub(Idx, Index)  || {J, Idx} <- enumerate(Indices), J /= I]),
                                      [erlang_pbc:element_div(Num, Den) | Acc1]
                              end, [], enumerate(Indices))).

apply_g1(Coefficients, Shares) ->
    Zero = erlang_pbc:element_set(erlang_pbc:element_new('G1', hd(Coefficients)), 1),
    lists:foldl(fun({Coefficient, Share}, Acc) ->
                        erlang_pbc:element_mul(Acc, erlang_pbc:element_pow(Share, Coefficient))
                end, Zero, lists:zip(Coefficients, Shares)).

apply_zr(Coefficients, Shares) ->
    Zero = erlang_pbc:element_set(erlang_pbc:element_new('Zr', hd(Coefficients)), 0),
    lists:foldl(fun({Coefficient, Share}, Acc) ->
                        erlang_pbc:element_add(Acc, erlang_pbc:element_mul(Share, Coefficient))
                end, Zero, lists:zip(Coefficients, Shares)).

enumerate(List) ->
    lists:zip(lists:seq(0, length(List) - 1), List).
