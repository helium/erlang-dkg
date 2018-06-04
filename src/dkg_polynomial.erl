-module(dkg_polynomial).
-compile({no_auto_import,[apply/2]}).

-export([generate/2,
         generate/3,
         generate/4,
         add/2,
         sub/2,
         mul/2,
         apply/2,
         cmp/2,
         degree/1,
         is_zero/1,
         is_equal/2,
         print/1]).

%% Create a random polynomial of degree t >= 0
generate(Pairing, T) ->
    [ erlang_pbc:element_random(erlang_pbc:element_new('Zr', Pairing)) || _ <- lists:seq(0, T)].

%% Create a random polynomial of degree t with the given constant term
generate(Pairing, T, Term) when is_integer(Term) ->
    generate(Pairing, T, erlang_pbc:element_set(erlang_pbc:element_new('Zr', Pairing), Term));
generate(Pairing, T, Term) ->
    [Term | generate(Pairing, T - 1)].

%% generate a random polynomial of degree t such that f(Index) => Term
generate(Pairing, T, Index, Term) ->
    [Head | Tail] = Poly = generate(Pairing, T),
    Val = apply(Poly, Index),
    NewHead = erlang_pbc:element_add(erlang_pbc:element_sub(Head, Val), Term),
    [NewHead | Tail].

degree(Poly) ->
    length(Poly) - 1.

is_zero(Poly) ->
    %% XXX: check all elements are 0 which they will be if the length(poly) == 0
    %% I don't understand why this is needed
    %% lists:all(fun erlang_pbc:element_is0/1, Poly) andalso length(Poly) == 0.
    length(Poly) == 0.

is_equal(PolyA, PolyB) ->
    %% check all elements are equal for polyA and polyB
    lists:all(fun({A, B}) ->
                      erlang_pbc:element_cmp(A, B)
              end, lists:zip(PolyA, PolyB)).

add(PolyA, PolyB) ->
    merge(PolyA, PolyB, fun erlang_pbc:element_add/2).

sub(PolyA, PolyB) ->
    merge(PolyA, PolyB, fun erlang_pbc:element_sub/2).

mul(PolyA, PolyB) ->
    %% why can't we just use set0 here?
    Zero = erlang_pbc:element_add(hd(PolyA++PolyB), erlang_pbc:element_neg(hd(PolyA++PolyB))),
    lists:foldl(fun(V, Acc0) ->
                        Acc = [Zero|Acc0],
                        Temp = [ erlang_pbc:element_mul(A, V) || A <- PolyA ],
                        add(Acc, Temp)
                end, [], PolyB).

cmp(PolyA, PolyB) ->
    %% check the degree(PolyA) == degree(PolyB)
    %% check f(x) - g(x) = 0,
    %% check that each element is the same the original
    degree(PolyA) == degree(PolyB) andalso
    dkg_polynomial:is_zero(dkg_polynomial:sub(PolyA, PolyB)) andalso
    is_equal(PolyA, PolyB).

%% Apply a polynomial at a point x using Horner's rule
apply(Poly, X) ->
    %% why can't we just use set0 here?
    Zero = erlang_pbc:element_add(hd(Poly), erlang_pbc:element_neg(hd(Poly))),
    %% go in reverse for coefficients
    lists:foldl(fun(Coeff, Acc) ->
                        erlang_pbc:element_add(erlang_pbc:element_mul(Acc, X), Coeff)
                end, Zero, lists:reverse(Poly)).

print(Poly) ->
    [ erlang_pbc:element_to_string(X) || X <- Poly].


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

