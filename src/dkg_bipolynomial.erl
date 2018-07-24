-module(dkg_bipolynomial).

-export([generate/2,
         generate/3,
         add/2,
         sub/2,
         degree/1,
         evaluate/2,
         print/1,
         cmp/2,
         is_zero/1,
         lookup/2,
         serialize/1,
         deserialize/2]).

-type row() :: {erlang_pbc:element(), erlang_pbc:element(), erlang_pbc:element()}.
-type bipolynomial() :: {row(), row(), row()}.

-export_type([bipolynomial/0]).

-spec generate(erlang_pbc:element(), pos_integer()) -> bipolynomial().
%% generate a bivariate polynomial of degree T
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

%% generate a bivariate polynomial of degree T with a fixed term
-spec generate(erlang_pbc:element(), pos_integer(), erlang_pbc:element() | integer()) -> bipolynomial().
generate(Pairing, T, Term) ->
    insert([1, 1], generate(Pairing, T), Term).

-spec add(bipolynomial(), bipolynomial()) -> bipolynomial().
add(PolyA, PolyB) ->
    merge(PolyA, PolyB, fun erlang_pbc:element_add/2).

-spec sub(bipolynomial(), bipolynomial()) -> bipolynomial().
sub(PolyA, PolyB) ->
    merge(PolyA, PolyB, fun erlang_pbc:element_sub/2).

-spec is_zero(bipolynomial()) -> boolean().
is_zero(Poly) ->
    tuple_size(Poly) == 0.

-spec degree(bipolynomial()) -> non_neg_integer().
degree(Poly) ->
    tuple_size(Poly) - 1.

-spec cmp(bipolynomial(), bipolynomial()) -> boolean().
cmp(PolyA, PolyB) ->
    %% check whether degree(PolyA) == degree(PolyB)
    %% and all the coefficients should match
    degree(PolyA) == degree(PolyB)
    andalso
    lists:all(fun(X) ->
                      X
              end,
              [ erlang_pbc:element_cmp(lookup([I, J], PolyA), lookup([I, J], PolyB)) || I <- lists:seq(1, degree(PolyA)), J <- lists:seq(1, degree(PolyB))]).

-spec evaluate(bipolynomial(), erlang_pbc:element()) -> dkg_polynomial:polynomial().
evaluate(Poly, X) ->
    PolyX = [X], %% polynomial has degree 0
    Result = [], %% empty result polynomial
    %% go in reverse for coefficient rows
    lists:foldl(fun(Row, Acc) ->
                        Temp = dkg_polynomial:mul(Acc, PolyX),
                        dkg_polynomial:add(Temp, tuple_to_list(Row))
                end, Result, lists:reverse(tuple_to_list(Poly))).

-spec print(bipolynomial()) -> any().
print(Poly) ->
    list_to_tuple(lists:map(fun(R) ->
                                    list_to_tuple([ erlang_pbc:element_to_string(X) || X <- tuple_to_list(R)])
                            end, tuple_to_list(Poly))).

-spec merge(bipolynomial(), bipolynomial(), fun()) -> bipolynomial().
merge(PolyA, PolyB, MergeFun) ->
    Degree = max(degree(PolyA), degree(PolyB)),
    %% find the bigger term
    [LargerPoly|_] = lists:sort(fun(A, B) -> degree(A) > degree(B) end, [PolyA, PolyB]),
    %% why can't we just use set0 here?
    Zero = erlang_pbc:element_add(lookup([1,1], LargerPoly), erlang_pbc:element_neg(lookup([1,1], LargerPoly))),

    %% make sure both matrices are the same size
    ExpandedPolyA = expand(PolyA, Degree, Zero),
    ExpandedPolyB = expand(PolyB, Degree, Zero),

    %% run the merge function on every matrix element
    %% use a cartesian product to simplify the iteration
    MergedPoly = lists:foldl(fun({Row, Col}, Acc) ->
                                     insert([Row, Col], Acc, MergeFun(lookup([Row, Col], ExpandedPolyA), lookup([Row, Col], ExpandedPolyB)))
                             end, ExpandedPolyA, [ {R, C} || R <- lists:seq(1, Degree+1), C <- lists:seq(1, Degree+1)]),

    %% trim any leading coefficients that are 0
    %% and delete any rows at the end that are all 0s
    prune(MergedPoly).

-spec expand(bipolynomial(), non_neg_integer(), erlang_pbc:element()) -> bipolynomial().
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
    list_to_tuple(lists:map(fun(Row) ->
                                    list_to_tuple(lists:sublist(tuple_to_list(Row), NewDimension))
                            end, HeightAdjustedPoly)).

lookup([Row, Col], Poly) ->
    element(Col, element(Row, Poly)).

insert([Row, Col], Poly, Val) ->
    setelement(Row, Poly, setelement(Col, element(Row, Poly), Val)).

serialize(BiPoly) ->
    Degree = degree(BiPoly),
    lists:foldl(fun({Row, Col}, Acc) ->
                        insert([Row, Col], Acc, erlang_pbc:element_to_binary(lookup([Row, Col], Acc)))
                end, BiPoly, [ {R, C} || R <- lists:seq(1, Degree+1), C <- lists:seq(1, Degree+1)]).

deserialize(BiPoly, Element) ->
    Degree = degree(BiPoly),
    lists:foldl(fun({Row, Col}, Acc) ->
                        insert([Row, Col], Acc, erlang_pbc:binary_to_element(Element, lookup([Row, Col], Acc)))
                end, BiPoly, [ {R, C} || R <- lists:seq(1, Degree+1), C <- lists:seq(1, Degree+1)]).
