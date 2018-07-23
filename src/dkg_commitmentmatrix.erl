-module(dkg_commitmentmatrix).

-export([new/2, lookup/2, cmp/2, mul/2, verify_poly/4, verify_point/5, public_key_share/3]).

new(Generator, T) when is_integer(T) ->
    %% generate an empty commitment matrix of degree T
    One = erlang_pbc:element_set(erlang_pbc:element_new('G1', Generator), 1),
    list_to_tuple(lists:foldl(fun(_, Acc) ->
                                      [list_to_tuple(lists:duplicate(T+1, One))| Acc]
                              end, [], lists:seq(0, T)));
new(Generator, BiPoly) when is_tuple(BiPoly) ->
    T = dkg_bipolynomial:degree(BiPoly),
    list_to_tuple([ list_to_tuple([ erlang_pbc:element_pow(Generator, dkg_bipolynomial:lookup([I+1, J+1], BiPoly)) || J <- lists:seq(0, T) ])  || I <- lists:seq(0, T) ]).

lookup([Row, Col], Poly) ->
    element(Col, element(Row, Poly)).

insert([Row, Col], Poly, Val) ->
    setelement(Row, Poly, setelement(Col, element(Row, Poly), Val)).

cmp(MatrixA, MatrixB) ->
    lists:all(fun({I, J}) ->
                      erlang_pbc:element_cmp(lookup([I, J], MatrixA), lookup([I,J], MatrixB))
              end,
      [ {I, J} || I <- lists:seq(1, tuple_size(MatrixA)), J <- lists:seq(1, tuple_size(MatrixA))]).

mul(MatrixA, MatrixB) ->
    %% Here each entry is multiplied with corresponding entry in the other matrix
    %% This is not normal matrix multiplication
    %% It is assumed that both matrices are the same size

    %% run the merge function on every matrix element
    %% use a cartesian product to simplify the iteration
    lists:foldl(fun({Row, Col}, Acc) ->
                        insert([Row, Col], Acc, erlang_pbc:element_mul(lookup([Row, Col], MatrixA), lookup([Row, Col], MatrixB)))
                end, MatrixA, [ {R, C} || R <- lists:seq(1, tuple_size(MatrixA)), C <- lists:seq(1, tuple_size(MatrixA))]).

verify_poly(U, Matrix, VerifierID, Poly) ->
    %% TODO obviously use something appropriate here
    I = erlang_pbc:element_set(hd(Poly), VerifierID),

    lists:foldl(fun(_L, false) ->
                        false;
                   (L, _Acc) ->
                        E1 = erlang_pbc:element_pow(U, lists:nth(L, Poly)),
                        E2 = lists:foldl(fun(J, Acc2) ->
                                                 erlang_pbc:element_mul(erlang_pbc:element_pow(Acc2, I), lookup([J, L], Matrix))
                                         end, erlang_pbc:element_set(U, 1), lists:reverse(lists:seq(1, tuple_size(Matrix)))),
                        erlang_pbc:element_cmp(E1, E2)
                end, true, lists:seq(1, length(Poly))).

verify_point(U, Matrix, SenderID, VerifierID, Point) ->
    M = erlang_pbc:element_set(Point, SenderID),
    I = erlang_pbc:element_set(Point, VerifierID),
    G1 = erlang_pbc:element_set(U, 1),

    Ga = erlang_pbc:element_pow(U, Point),
    Res = lists:foldl(fun(II, Acc) ->
                              R = erlang_pbc:element_pow(Acc, M),
                              Row = lists:foldl(fun(J, Acc2) ->
                                                        erlang_pbc:element_mul(erlang_pbc:element_pow(Acc2, I), lookup([II, J], Matrix))
                                                end, G1, lists:reverse(lists:seq(1, tuple_size(Matrix)))),
                              erlang_pbc:element_mul(R, Row)
                      end, G1, lists:reverse(lists:seq(1, tuple_size(Matrix)))),
    erlang_pbc:element_cmp(Ga, Res).

public_key_share(U, Matrix, NodeID) ->
    %% TODO this shares significant code with verify_point, consider refactoring them to share common code
    M = erlang_pbc:element_set(erlang_pbc:element_new('Zr', U), NodeID),
    I = erlang_pbc:element_set(erlang_pbc:element_new('Zr', U), 0),
    G1 = erlang_pbc:element_set(U, 1),

    %% return the public key share
    %% NOTE: C++ traverses the matrix in reverse, following the same
    lists:foldl(fun(II, Acc) ->
                        R = erlang_pbc:element_pow(Acc, M),
                        Row = lists:foldl(fun(J, Acc2) ->
                                                  erlang_pbc:element_mul(erlang_pbc:element_pow(Acc2, I), lookup([II, J], Matrix))
                                          end, G1, lists:reverse(lists:seq(1, tuple_size(Matrix)))),
                        erlang_pbc:element_mul(R, Row)
                end, G1, lists:reverse(lists:seq(1, tuple_size(Matrix)))).
