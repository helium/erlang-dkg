-module(dkg_commitmentmatrix).

-export([new/2, new/3, access/2, cmp/2, mul/2, verify_poly/3]).

new(Pairing, T) ->
    One = erlang_pbc:element_set(1, erlang_pbc:element_new('G1', Pairing)),
    lists:foldl(fun(_, Acc) ->
                        erlang:append_element(list_to_tuple(lists:duplicate(T+1, One)), Acc)
                end, {}, lists:seq(0, T)).

new(Pairing, T, BiPoly) ->
    T = dkg_bipolynomial:degree(BiPoly),
    G1 = erlang_pbc:element_new('G1', Pairing),
    %% TODO obviously use something appropriate here
    U = erlang_pbc:element_from_hash(G1, <<"lol">>),
    list_to_tuple([ list_to_tuple([ erlang_pbc:element_pow(U, dkg_bipolynomial:access([I+1, J+1], BiPoly)) || J <- lists:seq(0, T) ])  || I <- lists:seq(0, T) ]).

access([Row, Col], Poly) ->
    element(Col, element(Row, Poly)).

insert([Row, Col], Poly, Val) ->
    setelement(Row, Poly, setelement(Col, element(Row, Poly), Val)).

cmp(MatrixA, MatrixB) ->
    lists:all(fun({I, J}) ->
                      erlang_pbc:element_cmp(access([I, J], MatrixA), access([I,J], MatrixB))
              end,
      [ {I, J} || I <- lists:seq(1, tuple_size(MatrixA)), J <- lists:seq(1, tuple_size(MatrixA))]).

mul(MatrixA, MatrixB) ->
    %% Here each entry is multiplied with corresponding entry in the other matrix
    %% This is not normal matrix multiplication
    %% It is assumed that both matrices are the same size

    %% run the merge function on every matrix element
    %% use a cartesian product to simplify the iteration
    lists:foldl(fun({Row, Col}, Acc) ->
                        insert([Row, Col], Acc, erlang_pbc:element_mul(access([Row, Col], MatrixA), access([Row, Col], MatrixB)))
                end, MatrixA, [ {R, C} || R <- lists:seq(1, tuple_size(MatrixA)), C <- lists:seq(1, tuple_size(MatrixA))]).

verify_poly(_Matrix, _VerifierID, _Poly) ->
    ok.
