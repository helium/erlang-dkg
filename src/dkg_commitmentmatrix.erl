-module(dkg_commitmentmatrix).

-export([new/2,
         lookup/2,
         cmp/2,
         mul/2,
         verify_poly/4,
         verify_point/5,
         public_key_share/3,
         serialize/1,
         deserialize/2
        ]).

-type row() :: {erlang_pbc:element(), erlang_pbc:element(), erlang_pbc:element()}.
-type matrix() :: {row(), row(), row()}.
-type serialized_row() :: {binary(), binary(), binary()}.
-type serialized_matrix() :: {serialized_row(), serialized_row(), serialized_row()}.

-export_type([matrix/0, serialized_matrix/0]).

-spec new(erlang_pbc:element(), integer() | dkg_bipolynomial:bipolynomial()) -> matrix().
new(Generator, T) when is_integer(T) ->
    %% generate an empty commitment matrix of degree T
    One = erlang_pbc:element_set(Generator, 1),
    list_to_tuple(lists:foldl(fun(_, Acc) ->
                                      [list_to_tuple(lists:duplicate(T+1, One))| Acc]
                              end, [], lists:seq(0, T)));
new(Generator, BiPoly) when is_tuple(BiPoly) ->
    T = dkg_bipolynomial:degree(BiPoly),
    erlang_pbc:element_pp_init(Generator),
    list_to_tuple([ list_to_tuple([ erlang_pbc:element_pow(Generator, dkg_bipolynomial:lookup([I+1, J+1], BiPoly)) || J <- lists:seq(0, T) ])  || I <- lists:seq(0, T) ]).

lookup([Row, Col], Poly) ->
    element(Col, element(Row, Poly)).

insert([Row, Col], Poly, Val) ->
    setelement(Row, Poly, setelement(Col, element(Row, Poly), Val)).

-spec cmp(matrix(), matrix()) -> boolean().
cmp(MatrixA, MatrixB) ->
    lists:all(fun({I, J}) ->
                      erlang_pbc:element_cmp(lookup([I, J], MatrixA), lookup([I,J], MatrixB))
              end,
      iter(MatrixA)).

-spec mul(matrix(), matrix()) -> matrix().
mul(MatrixA, MatrixB) ->
    %% Here each entry is multiplied with corresponding entry in the other matrix
    %% This is not normal matrix multiplication
    %% It is assumed that both matrices are the same size

    %% run the merge function on every matrix element
    %% use a cartesian product to simplify the iteration
    lists:foldl(fun({Row, Col}, Acc) ->
                        insert([Row, Col], Acc, erlang_pbc:element_mul(lookup([Row, Col], MatrixA), lookup([Row, Col], MatrixB)))
                end, MatrixA, iter(MatrixA)).

-spec verify_poly(erlang_pbc:element(), matrix(), non_neg_integer(), dkg_polynomial:polynomial()) -> boolean().
verify_poly(U, Matrix, VerifierID, Poly) ->
    %% TODO obviously use something appropriate here
    I = erlang_pbc:element_set(hd(Poly), VerifierID),

    lists:foldl(fun(_L, false) ->
                        false;
                   (L, _Acc) ->
                        E1 = erlang_pbc:element_pow(U, lists:nth(L, Poly)),
                        E2 = lists:foldl(fun(J, Acc2) ->
                                                 erlang_pbc:element_mul(erlang_pbc:element_pow(Acc2, I), lookup([J, L], Matrix))
                                         end, erlang_pbc:element_set(U, 1), lists:reverse(lists:seq(1, sz(Matrix)))),
                        erlang_pbc:element_cmp(E1, E2)
                end, true, lists:seq(1, length(Poly))).

-spec verify_point(erlang_pbc:element(), matrix(), non_neg_integer(), non_neg_integer(), erlang_pbc:element()) -> boolean().
verify_point(U, Matrix, SenderID, VerifierID, Point) ->
    M = erlang_pbc:element_set(Point, SenderID),
    I = erlang_pbc:element_set(Point, VerifierID),
    G1 = erlang_pbc:element_set(U, 1),
    erlang_pbc:element_pp_init(G1),

    Ga = erlang_pbc:element_pow(U, Point),
    Res = lists:foldl(fun(II, Acc) ->
                              R = erlang_pbc:element_pow(Acc, M),
                              Row = lists:foldl(fun(J, Acc2) ->
                                                        erlang_pbc:element_mul(erlang_pbc:element_pow(Acc2, I), lookup([II, J], Matrix))
                                                end, G1, lists:reverse(lists:seq(1, sz(Matrix)))),
                              erlang_pbc:element_mul(R, Row)
                      end, G1, lists:reverse(lists:seq(1, sz(Matrix)))),
    erlang_pbc:element_cmp(Ga, Res).

-spec public_key_share(erlang_pbc:element(), matrix(), non_neg_integer()) -> erlang_pbc:element().
public_key_share(U, Matrix, NodeID) ->
    %% TODO this shares significant code with verify_point, consider refactoring them to share common code
    M = erlang_pbc:element_set(erlang_pbc:element_new('Zr', U), NodeID),
    I = erlang_pbc:element_set(erlang_pbc:element_new('Zr', U), 0),
    G1 = erlang_pbc:element_set(U, 1),
    erlang_pbc:element_pp_init(G1),

    %% return the public key share
    %% NOTE: C++ traverses the matrix in reverse, following the same
    lists:foldl(fun(II, Acc) ->
                        R = erlang_pbc:element_pow(Acc, M),
                        Row = lists:foldl(fun(J, Acc2) ->
                                                  erlang_pbc:element_mul(erlang_pbc:element_pow(Acc2, I), lookup([II, J], Matrix))
                                          end, G1, lists:reverse(lists:seq(1, sz(Matrix)))),
                        erlang_pbc:element_mul(R, Row)
                end, G1, lists:reverse(lists:seq(1, sz(Matrix)))).

-spec serialize(matrix()) -> serialized_matrix().
serialize(Matrix) ->
    lists:foldl(fun({I, J}, Acc) ->
                      insert([I, J], Acc, erlang_pbc:element_to_binary(lookup([I,J], Acc)))
              end, Matrix, iter(Matrix)).

-spec deserialize(serialized_matrix(), erlang_pbc:element()) -> matrix().
deserialize(Matrix, U) ->
    lists:foldl(fun({I, J}, Acc) ->
                      insert([I, J], Acc, erlang_pbc:binary_to_element(U, lookup([I,J], Acc)))
              end, Matrix, iter(Matrix)).

iter(Matrix) ->
    [ {I, J} || I <- lists:seq(1, sz(Matrix)), J <- lists:seq(1, sz(Matrix))].

sz(Matrix) ->
    tuple_size(Matrix).
