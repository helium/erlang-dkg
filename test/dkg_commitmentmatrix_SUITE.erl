-module(dkg_commitmentmatrix_SUITE).
-compile({no_auto_import,[apply/2]}).

-include_lib("eunit/include/eunit.hrl").

-export([all/0, init_per_testcase/2, end_per_testcase/2]).
-export([verify_poly_test/1, verify_point_test/1]).

all() ->
    [verify_poly_test, verify_point_test].

init_per_testcase(_, Config) ->
    Config.

end_per_testcase(_, Config) ->
    Config.

verify_poly_test(_Config) ->
    Pairing = erlang_pbc:group_new('SS512'),
    Secret = erlang_pbc:element_random(erlang_pbc:element_new('Zr', Pairing)),
    BiPoly = dkg_bipolynomial:generate(Pairing, 4, Secret),
    CommitmentMatrix = dkg_commitmentmatrix:new(Pairing, BiPoly),
    TaggedPolys = [ {I, dkg_bipolynomial:apply(BiPoly, erlang_pbc:element_set(erlang_pbc:element_new('Zr', Pairing), I))} || I <- lists:seq(1, 4) ],
    ?assert(lists:all(fun({I, Poly}) ->
                              dkg_commitmentmatrix:verify_poly(CommitmentMatrix, I, Poly)
                      end, TaggedPolys)).

verify_point_test(_Config) ->
    Pairing = erlang_pbc:group_new('SS512'),
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
