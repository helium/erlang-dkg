-module(dkg_lagrange).

-export([coefficients/3, apply/3]).

coefficients(Pairing, Indices, Alpha) ->
    lists:foldl(fun({I, Index}, Acc1) ->
                        One = erlang_pbc:element_set(erlang_pbc:element_new('Zr', Pairing), 1),
                        Num = lists:foldl(fun(E, Acc) ->
                                                  erlang_pbc:element_mul(Acc, E)
                                          end, One, [ erlang_pbc:element_sub(Idx, Alpha) || {J, Idx} <- enumerate(Indices), J /= I]),

                        Den = lists:foldl(fun(E, Acc) ->
                                                  erlang_pbc:element_mul(Acc, E)
                                          end, One, [ erlang_pbc:element_sub(Idx, Index)  || {J, Idx} <- enumerate(Indices), J /= I]),
                        [erlang_pbc:element_div(Num, Den) | Acc1]
                end, [], enumerate(Indices)).

apply(Pairing, Coefficients, Shares) ->
    Zero = erlang_pbc:element_set(erlang_pbc:element_new('Zr', Pairing), 0),
    lists:foldl(fun({Coefficient, Share}, Acc) ->
                        erlang_pbc:element_add(Acc, erlang_pbc:element_mul(Coefficient, Share))
                end, Zero, lists:zip(Coefficients, Shares)).

enumerate(List) ->
    lists:zip(lists:seq(0, length(List) - 1), List).
