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

-record(bipolynomial, {
          t :: non_neg_integer(),
          elements :: [erlang_pbc:element(), ...]
         }).

-type bipolynomial() :: #bipolynomial{}.

-export_type([bipolynomial/0]).

-spec generate(erlang_pbc:element(), pos_integer()) -> bipolynomial().
%% generate a bivariate polynomial of degree T
generate(Pairing, T) ->
    R = lists:foldl(fun(I, Acc) ->
                        RandCoeff = erlang_pbc:element_random(erlang_pbc:element_new('Zr', Pairing)),
                        NewAcc = setelement(I+1, Acc, erlang:append_element(element(I+1, Acc), RandCoeff)),
                        lists:foldl(fun(J, Acc2) ->
                                            RandCoeff2 = erlang_pbc:element_random(erlang_pbc:element_new('Zr', Pairing)),
                                            setelement(I+1,
                                                       setelement(J+1, Acc2,
                                                                  erlang:append_element(element(J+1, Acc2), RandCoeff2)),
                                                       erlang:append_element(element(I+1, Acc2), RandCoeff2))
                                    end, NewAcc, lists:seq(I+1, T))
                end, list_to_tuple(lists:duplicate(T+1, {})), lists:seq(0, T)),
    #bipolynomial{t=T, elements=lists:flatten([ tuple_to_list(E) || E <- tuple_to_list(R)])}.

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
is_zero(#bipolynomial{t=T}) ->
    T == 0.

-spec degree(bipolynomial()) -> non_neg_integer().
degree(#bipolynomial{t=T}) ->
    T.

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
                        dkg_polynomial:add(Temp, Row)
                end, Result, lists:reverse(rows(Poly))).

-spec print(bipolynomial()) -> any().
print(Poly) ->
    list_to_tuple(lists:map(fun(R) ->
                                    list_to_tuple([ erlang_pbc:element_to_string(X) || X <- R])
                            end, rows(Poly))).

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
            ExtraRows = lists:duplicate((Degree+1 - degree(Poly)+1)*(Degree+1), Padding),
            Elements = lists:flatten([ R ++ lists:duplicate(Degree - degree(Poly), Padding) || R <- rows(Poly) ] ++ ExtraRows),
            #bipolynomial{t=Degree, elements=Elements}
    end.

prune(Poly = #bipolynomial{t=T}) ->
    %% we need to find the minimum degree needed to represent this polynomial
    %% essentially this means how many rows and columns can we prune of zeros
    %% while keeping the matrix square
    Width = lists:foldl(fun(Row, MaxWidth) ->
                                W = length(lists:dropwhile(fun erlang_pbc:element_is0/1, lists:reverse(Row))),
                                max(W, MaxWidth)
                        end, 0, rows(Poly)),
    %% find how many trailing rows are empty and use that to calculate the minimum height
    Height = T+1 - length(lists:takewhile(fun(Row) ->
                                                  lists:all(fun erlang_pbc:element_is0/1, Row)
                                          end, lists:reverse(rows(Poly)))),

    NewDimension = max(Height, Width),
    Rows = lists:sublist(rows(Poly), NewDimension),
    Elements = lists:map(fun(Row) ->
                                 lists:sublist(Row, NewDimension)
                         end, Rows),
    #bipolynomial{t=NewDimension-1, elements=lists:flatten(Elements)}.

lookup([Row, Col], #bipolynomial{t=T, elements=Elements}) ->
    lists:nth(((Row-1)*(T+1)) + Col, Elements).

insert([Row, Col], BiPoly = #bipolynomial{t=T, elements=Elements}, Val) ->
    {Head, [_|Tail]} = lists:split(((Row-1)*(T+1)) + Col - 1, Elements),
    BiPoly#bipolynomial{elements = Head ++ [Val | Tail]}.

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

rows(#bipolynomial{t=T, elements=Elements}) ->
    rows(T, Elements, []).

rows(_, [], Acc) ->
    lists:reverse(Acc);
rows(T, Elements, Acc) ->
    {Row, Rest} = lists:split(T+1, Elements),
    rows(T, Rest, [Row|Acc]).
