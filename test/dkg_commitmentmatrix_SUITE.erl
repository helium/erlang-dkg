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
    Generator = erlang_pbc:element_from_hash(erlang_pbc:element_new('G1', Pairing), <<"honeybadger">>),
    Secret = erlang_pbc:element_random(erlang_pbc:element_new('Zr', Pairing)),
    BiPoly = dkg_bipolynomial:generate(Generator, 4, Secret),
    CommitmentMatrix = dkg_commitmentmatrix:new(Generator, BiPoly),
    TaggedPolys = [ {I, dkg_bipolynomial:apply(BiPoly, erlang_pbc:element_set(erlang_pbc:element_new('Zr', Pairing), I))} || I <- lists:seq(1, 4) ],
    ?assert(lists:all(fun({I, Poly}) ->
                              dkg_commitmentmatrix:verify_poly(Generator, CommitmentMatrix, I, Poly)
                      end, TaggedPolys)).

verify_point_test(Config) ->
    Pairing = proplists:get_value(pairing, Config),
    Secret = erlang_pbc:element_random(erlang_pbc:element_new('Zr', Pairing)),
    Generator = erlang_pbc:element_from_hash(erlang_pbc:element_new('G1', Pairing), <<"honeybadger">>),
    RandomBiPoly = dkg_bipolynomial:generate(Pairing, 4, Secret),
    CommitmentMatrix = dkg_commitmentmatrix:new(Generator, RandomBiPoly),
    TaggedPolys = [ {J, dkg_bipolynomial:apply(RandomBiPoly, erlang_pbc:element_set(erlang_pbc:element_new('Zr', Pairing), J))} || J <- lists:seq(1, 4) ],
    Res = lists:map(fun({SenderId, Poly}) ->
                            case dkg_commitmentmatrix:verify_poly(Generator, CommitmentMatrix, SenderId, Poly) of
                                true ->
                                    %% verify_poly succeeded, check verify_point for verifiers
                                    lists:map(fun({VerifierId, Poly2}) ->
                                                      Point = dkg_polynomial:apply(Poly2, erlang_pbc:element_set(erlang_pbc:element_new('Zr', Pairing), SenderId)),
                                                      dkg_commitmentmatrix:verify_point(Generator, CommitmentMatrix, SenderId, VerifierId, Point)
                                              end, TaggedPolys);
                                false ->
                                    false
                            end
                    end, TaggedPolys),
    ct:pal("Res: ~p~n", [Res]),
    ?assert(lists:all(fun(X) -> X end, lists:flatten(Res))),
    ok.

public_key_share_test(Config) ->
    Pairing = proplists:get_value(pairing, Config),
    Secret = erlang_pbc:element_random(erlang_pbc:element_new('Zr', Pairing)),
    Generator = erlang_pbc:element_from_hash(erlang_pbc:element_new('G1', Pairing), <<"honeybadger">>),
    ct:pal("Secret: ~p~n", [erlang_pbc:element_to_string(Secret)]),
    RandomBiPoly = dkg_bipolynomial:generate(Pairing, 5, Secret),
    ct:pal("RandomBiPoly: ~p~n", [dkg_bipolynomial:print(RandomBiPoly)]),
    CommitmentMatrix = dkg_commitmentmatrix:new(Generator, RandomBiPoly),
    ct:pal("CommitmentMatrix: ~p~n", [dkg_bipolynomial:print(CommitmentMatrix)]),
    PublicKeySharePolynomial = [ dkg_commitmentmatrix:public_key_share(CommitmentMatrix, NodeId) || NodeId <- lists:seq(1, 6)],
    ct:pal("PublicKeyShares: ~p~n", [dkg_polynomial:print(PublicKeySharePolynomial)]),
    KnownSecret = dkg_polynomial:apply(PublicKeySharePolynomial, 0),
    ct:pal("KnownSecret: ~p~n", [erlang_pbc:element_to_string(KnownSecret)]),
    Indices = [ erlang_pbc:element_set(erlang_pbc:element_new('Zr', Pairing), I) || I <- lists:seq(1, 6) ],
    ct:pal("Indices: ~p~n", [dkg_polynomial:print(Indices)]),
    Alpha = erlang_pbc:element_set(erlang_pbc:element_new('Zr', Pairing), 0),
    ct:pal("Alpha: ~p~n", [erlang_pbc:element_to_string(Alpha)]),
    CalculatedSecret = dkg_lagrange:interpolate(PublicKeySharePolynomial, Indices, Alpha),
    ct:pal("CalculatedSecret: ~p~n", [erlang_pbc:element_to_string(CalculatedSecret)]),
    ?assert(erlang_pbc:element_cmp(KnownSecret, CalculatedSecret)),
    ok.

matrix_comparison_test(Config) ->
    Pairing = proplists:get_value(pairing, Config),
    Generator = erlang_pbc:element_from_hash(erlang_pbc:element_new('G1', Pairing), <<"honeybadger">>),
    Secret1 = erlang_pbc:element_random(erlang_pbc:element_new('Zr', Pairing)),
    ct:pal("Secret1: ~p~n", [erlang_pbc:element_to_string(Secret1)]),
    RandomBiPoly1 = dkg_bipolynomial:generate(Pairing, 5, Secret1),
    ct:pal("RandomBiPoly1: ~p~n", [dkg_bipolynomial:print(RandomBiPoly1)]),
    CommitmentMatrix1 = dkg_commitmentmatrix:new(Generator, RandomBiPoly1),
    ct:pal("CommitmentMatrix1: ~p~n", [dkg_bipolynomial:print(CommitmentMatrix1)]),
    Secret2 = erlang_pbc:element_random(erlang_pbc:element_new('Zr', Pairing)),
    ct:pal("Secret2: ~p~n", [erlang_pbc:element_to_string(Secret2)]),
    RandomBiPoly2 = dkg_bipolynomial:generate(Pairing, 5, Secret2),
    ct:pal("RandomBiPoly2: ~p~n", [dkg_bipolynomial:print(RandomBiPoly2)]),
    CommitmentMatrix2 = dkg_commitmentmatrix:new(Generator, RandomBiPoly2),
    ct:pal("CommitmentMatrix2: ~p~n", [dkg_bipolynomial:print(CommitmentMatrix2)]),
    ?assertEqual(false, dkg_commitmentmatrix:cmp(CommitmentMatrix1, CommitmentMatrix2)),
    ok.
