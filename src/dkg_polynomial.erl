-module(dkg_polynomial).
-compile({no_auto_import,[evaluate/2]}).

-export([generate/2,
         generate/3,
         generate/4,
         add/2,
         sub/2,
         mul/2,
         evaluate/2,
         cmp/2,
         degree/1,
         is_zero/1,
         is_equal/2,
         print/1,
         serialize/1,
         deserialize/2]).

-type polynomial() :: [erlang_pbc:element()].
-export_type([polynomial/0]).

%% Create a random polynomial of degree t >= 0
-spec generate(erlang_pbc:element(), pos_integer()) -> polynomial().
generate(Pairing, T) ->
    [ erlang_pbc:element_random(erlang_pbc:element_new('Zr', Pairing)) || _ <- lists:seq(0, T)].

%% Create a random polynomial of degree t with the given constant term
-spec generate(erlang_pbc:element(), pos_integer(), pos_integer() | erlang_pbc:element()) -> polynomial().
generate(Pairing, T, Term) when is_integer(Term) ->
    generate(Pairing, T, erlang_pbc:element_set(erlang_pbc:element_new('Zr', Pairing), Term));
generate(Pairing, T, Term) ->
    [Term | generate(Pairing, T - 1)].

%% generate a random polynomial of degree t such that f(Index) => Term
-spec generate(erlang_pbc:element(), integer(), erlang_pbc:element(), integer() | erlang_pbc:element()) -> polynomial().
generate(Pairing, T, Index, Term) ->
    [Head | Tail] = Poly = generate(Pairing, T),
    Val = evaluate(Poly, Index),
    NewHead = erlang_pbc:element_add(erlang_pbc:element_sub(Head, Val), Term),
    [NewHead | Tail].

-spec degree(polynomial()) -> non_neg_integer().
degree(Poly) ->
    length(Poly) - 1.

-spec is_zero(polynomial()) -> boolean().
is_zero(Poly) ->
    %% XXX: check all elements are 0 which they will be if the length(poly) == 0
    %% I don't understand why this is needed
    %% lists:all(fun erlang_pbc:element_is0/1, Poly) andalso length(Poly) == 0.
    length(Poly) == 0.

-spec is_equal(polynomial(), polynomial()) -> boolean().
is_equal(PolyA, PolyB) ->
    %% check all elements are equal for polyA and polyB
    lists:all(fun({A, B}) ->
                      erlang_pbc:element_cmp(A, B)
              end, lists:zip(PolyA, PolyB)).

-spec add(polynomial(), polynomial()) -> polynomial().
add(PolyA, PolyB) ->
    merge(PolyA, PolyB, fun erlang_pbc:element_add/2).

-spec sub(polynomial(), polynomial()) -> polynomial().
sub(PolyA, PolyB) ->
    merge(PolyA, PolyB, fun erlang_pbc:element_sub/2).

-spec mul(polynomial(), polynomial()) -> polynomial().
mul(PolyA, PolyB) ->
    %% why can't we just use set0 here?
    Zero = erlang_pbc:element_add(hd(PolyA++PolyB), erlang_pbc:element_neg(hd(PolyA++PolyB))),
    lists:foldl(fun(V, Acc0) ->
                        Acc = [Zero|Acc0],
                        Temp = [ erlang_pbc:element_mul(A, V) || A <- PolyA ],
                        add(Acc, Temp)
                end, [], PolyB).

-spec cmp(polynomial(), polynomial()) -> boolean().
cmp(PolyA, PolyB) ->
    %% check the degree(PolyA) == degree(PolyB)
    %% check that each element is the same the original
    degree(PolyA) == degree(PolyB) andalso
    is_equal(PolyA, PolyB).

%% Apply a polynomial at a point x using Horner's rule
-spec evaluate(polynomial(), erlang_pbc:element() | integer()) -> erlang_pbc:element().
evaluate(Poly, X) ->
    %% why can't we just use set0 here?
    Zero = erlang_pbc:element_add(hd(Poly), erlang_pbc:element_neg(hd(Poly))),
    %% go in reverse for coefficients
    lists:foldl(fun(Coeff, Acc) ->
                        erlang_pbc:element_add(erlang_pbc:element_mul(Acc, X), Coeff)
                end, Zero, lists:reverse(Poly)).

-spec print(polynomial()) -> any().
print(Poly) ->
    [ erlang_pbc:element_to_string(X) || X <- Poly].

-spec merge(polynomial(), polynomial(), fun()) -> polynomial().
merge(PolyA, PolyB, MergeFun) ->
    Degree = max(degree(PolyA) + 1, degree(PolyB) + 1),
    %% why can't we just use set0 here?
    Zero = erlang_pbc:element_add(hd(PolyA++PolyB), erlang_pbc:element_neg(hd(PolyA++PolyB))),

    %% pad the shorter polynomial so they're the same length
    ExpandedPolyA = PolyA ++ lists:duplicate(Degree - length(PolyA), Zero),
    ExpandedPolyB = PolyB ++ lists:duplicate(Degree - length(PolyB), Zero),

    MergedPoly = lists:map(fun({A, B}) ->
                                   MergeFun(A, B)
                           end, lists:zip(ExpandedPolyA, ExpandedPolyB)),

    %% remove any trailing 0s
    lists:reverse(lists:dropwhile(fun erlang_pbc:element_is0/1, lists:reverse(MergedPoly))).

serialize(Poly) ->
    erlang_pbc:elements_to_binary(Poly).

deserialize(Poly, Element) ->
    erlang_pbc:binary_to_elements(Element, Poly).
