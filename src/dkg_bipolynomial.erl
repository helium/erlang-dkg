-module(dkg_bipolynomial).

-export([generate/2, add/2, sub/2, degree/1, apply/2, print/1]).

-spec generate(erlang_pbc:group(), pos_integer()) -> tuple().
generate(Pairing, T) ->
    lists:foldl(fun(I, Acc) ->
                        RandCoeff = erlang_pbc:element_random(erlang_pbc:element_new('Zr', Pairing)),
                        NewAcc = setelement(I+1, Acc, erlang:append_element(element(I+1, Acc), RandCoeff)),
                        lists:foldl(fun(J, Acc2) ->
                                            RandCoeff2 = erlang_pbc:element_random(erlang_pbc:element_new('Zr', Pairing)),
                                            setelement(I+1,
                                                       setelement(J+1, Acc2,
                                                                  erlang:append_element(element(J+1, Acc2), RandCoeff2)),
                                                       erlang:append_element(element(I+1, Acc2), RandCoeff2))
                                    end, NewAcc, lists:seq(I+1, T))
                end, list_to_tuple(lists:duplicate(T+1, {})), lists:seq(0, T)).

add(PolyA, PolyB) ->
    merge(PolyA, PolyB, fun erlang_pbc:element_add/2).

sub(PolyA, PolyB) ->
    merge(PolyA, PolyB, fun erlang_pbc:element_sub/2).

degree(Poly) ->
    tuple_size(Poly) - 1.

apply(Poly, X) ->
    PolyX = [X], %% polynomial has degree 0
    Result = [], %% empty result polynomial
    lists:foldl(fun(Row, Acc) ->
                        Temp = dkg_polynomial:mul(Acc, PolyX),
                        dkg_polynomial:add(Temp, tuple_to_list(Row))
                end, Poly, Result).

print(Poly) ->
    list_to_tuple(lists:map(fun(R) ->
                      list_to_tuple([ erlang_pbc:element_to_string(X) || X <- tuple_to_list(R)])
              end, Poly)).

merge(PolyA, PolyB, MergeFun) ->
    Degree = max(tuple_size(PolyA), tuple_size(PolyB)),
    %% why can't we just use set0 here?
    Zero = erlang_pbc:element_add(access([1,1], PolyB), erlang_pbc:element_neg(access([1,1], PolyB))),

    %% make sure both matrices are the same size
    ExpandedPolyA = expand(PolyA, Degree, Zero),
    ExpandedPolyB = expand(PolyB, Degree, Zero),

    %% run the merge function on every matrix element
    %% use a cartesian product to simplify the iteration
    MergedPoly = lists:foldl(fun({Row, Col}, Acc) ->
                                     insert([Row, Col], Acc, MergeFun(access([Row, Col], ExpandedPolyA), access([Row, Col], ExpandedPolyB)))
                             end, ExpandedPolyA, [ {R, C} || R <- lists:seq(1, Degree), C <- lists:seq(1, Degree)]),

    %% trim any leading coefficents that are 0
    %% and delete any rows at the end that are all 0s
    prune(MergedPoly).

expand(Poly, Degree, Padding) ->
    case degree(Poly) == Degree of
        true ->
            Poly;
        false ->
            list_to_tuple(lists:map(fun(R) ->
                              pad_row(R, Degree, Padding)
                      end, tuple_to_list(Poly)) ++ lists:duplicate(Degree - degree(Poly), list_to_tuple(lists:duplicate(Degree+1, Padding))))
    end.

pad_row(R, Degree, Padding) ->
    case tuple_size(R) - 1 == Degree of
        true -> R;
        false ->
            pad_row(erlang:append_element(R, Padding), Degree, Padding)
    end.

prune(Poly) ->
    %% we need to find the minimum degree needed to represent this polynomial
    %% essentially this means how many rows and columns can we prune of zeros
    %% while keeping the matrix square
    Width = lists:foldl(fun(Row, MaxWidth) ->
                                W = length(lists:dropwhile(fun erlang_pbc:element_is0/1, lists:reverse(tuple_to_list(Row)))),
                                max(W, MaxWidth)
                        end, 0, tuple_to_list(Poly)),
    %% find how many trailing rows are empty and use that to calculate the minimum height
    Height = tuple_size(Poly) - length(lists:takewhile(fun(Row) ->
                                                               lists:all(fun erlang_pbc:element_is0/1, tuple_to_list(Row))
                                                       end, lists:reverse(tuple_to_list(Poly)))),

    NewDimension = max(Height, Width),
    HeightAdjustedPoly = lists:sublist(tuple_to_list(Poly), NewDimension),
    lists:map(fun(Row) ->
                      list_to_tuple(lists:sublist(tuple_to_list(Row), NewDimension))
              end, HeightAdjustedPoly).

access([Row, Col], Poly) ->
    element(Col, element(Row, Poly)).

insert([Row, Col], Poly, Val) ->
    setelement(Row, Poly, setelement(Col, element(Row, Poly), Val)).
