-module(dkg_commitmentmatrix_SUITE).
-compile({no_auto_import,[apply/2]}).

-include_lib("eunit/include/eunit.hrl").

-export([all/0, init_per_testcase/2, end_per_testcase/2]).
-export([verify_poly_test/1,
         public_key_share_test/1,
         matrix_comparison_test/1,
         verify_point_test/1]).

all() ->
    [verify_poly_test,
     verify_point_test,
     matrix_comparison_test,
     public_key_share_test].

init_per_testcase(_, Config) ->
    Pairing = erlang_pbc:group_new('SS512'),
    [{pairing, Pairing} | Config].

end_per_testcase(_, Config) ->
    Config.

verify_poly_test(Config) ->
    Pairing = proplists:get_value(pairing, Config),
    Secret = erlang_pbc:element_random(erlang_pbc:element_new('Zr', Pairing)),
    BiPoly = dkg_bipolynomial:generate(Pairing, 4, Secret),
    CommitmentMatrix = dkg_commitmentmatrix:new(Pairing, BiPoly),
    TaggedPolys = [ {I, dkg_bipolynomial:apply(BiPoly, erlang_pbc:element_set(erlang_pbc:element_new('Zr', Pairing), I))} || I <- lists:seq(1, 4) ],
    ?assert(lists:all(fun({I, Poly}) ->
                              dkg_commitmentmatrix:verify_poly(CommitmentMatrix, I, Poly)
                      end, TaggedPolys)).

verify_point_test(Config) ->
    Pairing = proplists:get_value(pairing, Config),
    Secret = erlang_pbc:element_random(erlang_pbc:element_new('Zr', Pairing)),
    RandomBiPoly = dkg_bipolynomial:generate(Pairing, 4, Secret),
    CommitmentMatrix = dkg_commitmentmatrix:new(Pairing, RandomBiPoly),
    TaggedPolys = [ {J, dkg_bipolynomial:apply(RandomBiPoly, erlang_pbc:element_set(erlang_pbc:element_new('Zr', Pairing), J))} || J <- lists:seq(1, 4) ],
    Res = lists:map(fun({SenderId, Poly}) ->
                            case dkg_commitmentmatrix:verify_poly(CommitmentMatrix, SenderId, Poly) of
                                true ->
                                    %% verify_poly succeeded, check verify_point for verifiers
                                    lists:map(fun({VerifierId, Poly2}) ->
                                                      Point = dkg_polynomial:apply(Poly2, erlang_pbc:element_set(erlang_pbc:element_new('Zr', Pairing), SenderId)),
                                                      dkg_commitmentmatrix:verify_point(CommitmentMatrix, SenderId, VerifierId, Point)
                                              end, TaggedPolys);
                                false ->
                                    false
                            end
                    end, TaggedPolys),
    io:format("Res: ~p~n", [Res]),
    ?assert(lists:all(fun(X) -> X end, lists:flatten(Res))),
    ok.

public_key_share_test(Config) ->
    Pairing = proplists:get_value(pairing, Config),
    Secret = erlang_pbc:element_random(erlang_pbc:element_new('Zr', Pairing)),
    io:format("Secret: ~p~n", [erlang_pbc:element_to_string(Secret)]),
    RandomBiPoly = dkg_bipolynomial:generate(Pairing, 5, Secret),
    io:format("RandomBiPoly: ~p~n", [dkg_bipolynomial:print(RandomBiPoly)]),
    CommitmentMatrix = dkg_commitmentmatrix:new(Pairing, RandomBiPoly),
    io:format("CommitmentMatrix: ~p~n", [dkg_bipolynomial:print(CommitmentMatrix)]),
    PublicKeySharePolynomial = [ dkg_commitmentmatrix:public_key_share(CommitmentMatrix, NodeId) || NodeId <- lists:seq(1, 6)],
    io:format("PublicKeyShares: ~p~n", [dkg_polynomial:print(PublicKeySharePolynomial)]),
    KnownSecret = dkg_polynomial:apply(PublicKeySharePolynomial, 0),
    io:format("KnownSecret: ~p~n", [erlang_pbc:element_to_string(KnownSecret)]),
    Indices = [ erlang_pbc:element_set(erlang_pbc:element_new('Zr', Pairing), I) || I <- lists:seq(1, 6) ],
    io:format("Indices: ~p~n", [dkg_polynomial:print(Indices)]),
    Alpha = erlang_pbc:element_set(erlang_pbc:element_new('Zr', Pairing), 0),
    io:format("Alpha: ~p~n", [erlang_pbc:element_to_string(Alpha)]),
    CalculatedSecret = dkg_lagrange:interpolate(PublicKeySharePolynomial, Indices, Alpha),
    io:format("CalculatedSecret: ~p~n", [erlang_pbc:element_to_string(CalculatedSecret)]),
    ?assert(erlang_pbc:element_cmp(KnownSecret, CalculatedSecret)),
    ok.

matrix_comparison_test(Config) ->
    Pairing = proplists:get_value(pairing, Config),
    Secret1 = erlang_pbc:element_random(erlang_pbc:element_new('Zr', Pairing)),
    io:format("Secret1: ~p~n", [erlang_pbc:element_to_string(Secret1)]),
    RandomBiPoly1 = dkg_bipolynomial:generate(Pairing, 5, Secret1),
    io:format("RandomBiPoly1: ~p~n", [dkg_bipolynomial:print(RandomBiPoly1)]),
    CommitmentMatrix1 = dkg_commitmentmatrix:new(Pairing, RandomBiPoly1),
    io:format("CommitmentMatrix1: ~p~n", [dkg_bipolynomial:print(CommitmentMatrix1)]),
    Secret2 = erlang_pbc:element_random(erlang_pbc:element_new('Zr', Pairing)),
    io:format("Secret2: ~p~n", [erlang_pbc:element_to_string(Secret2)]),
    RandomBiPoly2 = dkg_bipolynomial:generate(Pairing, 5, Secret2),
    io:format("RandomBiPoly2: ~p~n", [dkg_bipolynomial:print(RandomBiPoly2)]),
    CommitmentMatrix2 = dkg_commitmentmatrix:new(Pairing, RandomBiPoly2),
    io:format("CommitmentMatrix2: ~p~n", [dkg_bipolynomial:print(CommitmentMatrix2)]),
    ?assertEqual(false, dkg_commitmentmatrix:cmp(CommitmentMatrix1, CommitmentMatrix2)),
    ok.
