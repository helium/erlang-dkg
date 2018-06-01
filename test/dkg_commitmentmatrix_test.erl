-module(dkg_commitmentmatrix_test).

-include_lib("eunit/include/eunit.hrl").

verify_poly_test() ->
    Pairing = erlang_pbc:group_new('SS512'),
    Secret = erlang_pbc:element_random(erlang_pbc:element_new('Zr', Pairing)),
    BiPoly = dkg_bipolynomial:generate(Pairing, 4, Secret),
    CommitmentMatrix = dkg_commitmentmatrix:new(Pairing, BiPoly),
    TaggedPolys = [ {I, dkg_bipolynomial:apply(BiPoly, erlang_pbc:element_set(erlang_pbc:element_new('Zr', Pairing), I))} || I <- lists:seq(1, 4) ],
    ?assert(lists:all(fun({I, Poly}) ->
                              dkg_commitmentmatrix:verify_poly(CommitmentMatrix, I, Poly)
                      end, TaggedPolys)).

