-module(dkg_polynomial).
-compile({no_auto_import,[apply/2]}).

-export([generate/2, generate/3, generate/4, add/2, sub/2, mul/2, apply/2, print/1]).

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
    Degree = max(length(PolyA), length(PolyB)),
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

