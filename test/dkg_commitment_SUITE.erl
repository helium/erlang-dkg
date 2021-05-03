-module(dkg_commitment_SUITE).

-compile({no_auto_import, [evaluate/2]}).

-include_lib("eunit/include/eunit.hrl").

-export([all/0, init_per_testcase/2, end_per_testcase/2]).
-export([
    verify_poly_test/1,
    commitment_cmp_test/1,
    verify_point_test/1
]).

all() ->
    [
        verify_poly_test,
        verify_point_test,
        commitment_cmp_test
    ].

init_per_testcase(_, Config) ->
    Config.

end_per_testcase(_, Config) ->
    Config.

verify_poly_test(_Config) ->
    Secret = rand:uniform(4096),
    BiPoly = tc_bipoly:with_secret(Secret, 4),
    BiCommitment = tc_bipoly:commitment(BiPoly),
    NodeIDs = lists:seq(1, 4),
    TaggedPolys = [{I, tc_bipoly:row(BiPoly, I)} || I <- NodeIDs],
    DKGCommitment = dkg_commitment:new(NodeIDs, BiCommitment, dkg_util:commitment_cache_fun()),
    ?assert(
        lists:all(
            fun({I, Poly}) ->
                dkg_commitment:verify_poly(DKGCommitment, I, Poly)
            end,
            TaggedPolys
        )
    ).

verify_point_test(_Config) ->
    Secret = rand:uniform(4096),
    RandomBiPoly = tc_bipoly:with_secret(Secret, 4),
    BiCommitment = tc_bipoly:commitment(RandomBiPoly),
    NodeIDs = lists:seq(1, 4),
    TaggedPolys = [{I, tc_bipoly:row(RandomBiPoly, I)} || I <- NodeIDs],
    DKGCommitment = dkg_commitment:new(NodeIDs, BiCommitment, dkg_util:commitment_cache_fun()),
    Res = lists:map(
        fun({SenderId, Poly}) ->
            case dkg_commitment:verify_poly(DKGCommitment, SenderId, Poly) of
                true ->
                    %% verify_poly succeeded, check verify_point for verifiers
                    lists:map(
                        fun({VerifierId, Poly2}) ->
                            Point = tc_poly:eval(Poly2, SenderId),
                            SPoint = tc_fr:serialize(Point),
                            dkg_commitment:verify_point(DKGCommitment, SenderId, VerifierId, SPoint)
                        end,
                        TaggedPolys
                    );
                false ->
                    false
            end
        end,
        TaggedPolys
    ),
    ct:pal("Res: ~p~n", [Res]),
    ?assert(lists:all(fun(X) -> X end, lists:flatten(Res))),
    ok.

commitment_cmp_test(_Config) ->
    Secret1 = rand:uniform(4096),
    RandomBiPoly1 = tc_bipoly:with_secret(Secret1, 5),
    C1 = dkg_commitment:new(1, tc_bipoly:commitment(RandomBiPoly1), dkg_util:commitment_cache_fun()),
    Secret2 = rand:uniform(4096),
    RandomBiPoly2 = tc_bipoly:with_secret(Secret2, 5),
    C2 = dkg_commitment:new(2, tc_bipoly:commitment(RandomBiPoly2), dkg_util:commitment_cache_fun()),
    ?assertEqual(false, dkg_commitment:cmp(C1, C2)),
    ok.
