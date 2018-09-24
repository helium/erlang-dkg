-module(dkg_commitment).

-export([new/3,
         cmp/2,
         mul/2,
         verify_poly/3,
         public_key_share/2,
         verify_point/4,
         interpolate/3,
         add_echo/3,
         add_ready/3,
         num_echoes/1,
         num_readies/1,
         matrix/1,
         set_matrix/2,
         serialize/1,
         deserialize/2,
         status/1
        ]).

-record(commitment, {
          matrix :: dkg_commitmentmatrix:matrix(),
          generator :: erlang_pbc:element(),
          nodes = [] :: [pos_integer()],
          echoes = #{} :: #{pos_integer() => erlang_pbc:element()},
          readies =#{} :: #{pos_integer() => erlang_pbc:element()}
         }).

-record(serialized_commitment, {
          matrix :: dkg_commitmentmatrix:serialized_matrix(),
          generator :: binary(),
          nodes = [] :: [pos_integer()],
          echoes = #{} :: #{pos_integer() => binary()},
          readies = #{} :: #{pos_integer() => binary()}
         }).

-type commitment() :: #commitment{}.
-type serialized_commitment() :: #serialized_commitment{}.

-export_type([commitment/0, serialized_commitment/0]).

-spec new([pos_integer(),...], erlang_pbc:element(), integer() | dkg_bipolynomial:bipolynomial()) -> commitment().
new(NodeIDs, Generator, Degree) when is_integer(Degree) ->
    #commitment{nodes=NodeIDs, matrix=dkg_commitmentmatrix:new(Generator, Degree), generator=Generator};
new(NodeIDs, Generator, BiPoly) ->
    #commitment{nodes=NodeIDs, matrix=dkg_commitmentmatrix:new(Generator, BiPoly), generator=Generator}.

-spec cmp(commitment(), commitment()) -> boolean().
cmp(CommitmentA, CommitmentB) ->
    dkg_commitmentmatrix:cmp(CommitmentA#commitment.matrix, CommitmentB#commitment.matrix).

-spec mul(commitment(), commitment()) -> commitment().
mul(CommitmentA, CommitmentB) ->
    NewMatrix = dkg_commitmentmatrix:mul(CommitmentA#commitment.matrix, CommitmentB#commitment.matrix),
    CommitmentA#commitment{matrix=NewMatrix}.

-spec verify_poly(commitment(), pos_integer(), dkg_polynomial:polynomial()) -> boolean().
verify_poly(Commitment, VerifierID, Poly) ->
    dkg_commitmentmatrix:verify_poly(Commitment#commitment.generator, Commitment#commitment.matrix, VerifierID, Poly).

-spec public_key_share(commitment(), pos_integer()) -> erlang_pbc:element().
public_key_share(Commitment, NodeID) ->
    dkg_commitmentmatrix:public_key_share(Commitment#commitment.generator, Commitment#commitment.matrix, NodeID).

-spec verify_point(commitment(), pos_integer(), pos_integer(), erlang_pbc:element()) -> boolean().
verify_point(Commitment, SenderID, VerifierID, Point) ->
    dkg_commitmentmatrix:verify_point(Commitment#commitment.generator, Commitment#commitment.matrix, SenderID, VerifierID, Point).

-spec interpolate(commitment(), echo | ready, [pos_integer()]) -> [erlang_pbc:element()].
interpolate(Commitment, EchoOrReady, ActiveNodeIDs) ->
    Map = case EchoOrReady of
               echo -> Commitment#commitment.echoes;
               ready -> Commitment#commitment.readies
           end,
    {Indices0, Elements} = lists:unzip(maps:to_list(Map)),
    %% turn the node IDs into PBC elements
    Indices = [ erlang_pbc:element_set(erlang_pbc:element_new('Zr', hd(Elements)), I) || I <- Indices0 ],
    Shares = lists:foldl(fun(Index, Acc) ->
                                 case maps:is_key(Index, Map) of
                                     false ->
                                         %% Node ${Index} has not sent us a share, interpolate it
                                         Alpha = erlang_pbc:element_set(erlang_pbc:element_new('Zr', hd(Elements)), Index),
                                         LagrangePoly = dkg_lagrange:coefficients(Indices, Alpha),
                                         InterpolatedShare = dkg_lagrange:evaluate_zr(LagrangePoly, Elements),
                                         [ InterpolatedShare | Acc];
                                     true ->
                                         %% Node ${Index} has sent us a share
                                         [ maps:get(Index, Map) | Acc]
                                 end
                         end, [], [0 | ActiveNodeIDs]), %% note that we also evaluate at 0
    lists:reverse(Shares).

-spec add_echo(commitment(), pos_integer(), erlang_pbc:element()) -> {true | false, commitment()}.
add_echo(Commitment = #commitment{nodes=Nodes, echoes=Echoes}, NodeID, Echo) when NodeID /= 0 ->
    case lists:member(NodeID, Nodes) andalso maps:is_key(NodeID, Echoes) of
        true ->
            {false, Commitment};
        false ->
            {true, Commitment#commitment{echoes = maps:put(NodeID, Echo, Echoes)}}
    end.

-spec add_ready(commitment(), pos_integer(), erlang_pbc:element()) -> {true | false, commitment()}.
add_ready(Commitment = #commitment{nodes=Nodes, readies=Readies}, NodeID, Ready) when NodeID /= 0 ->
    case lists:member(NodeID, Nodes) andalso maps:is_key(NodeID, Readies) of
        true ->
            {false, Commitment};
        false ->
            {true, Commitment#commitment{readies = maps:put(NodeID, Ready, Readies)}}
    end.

-spec num_echoes(commitment()) -> non_neg_integer().
num_echoes(#commitment{echoes=Echoes}) ->
    maps:size(Echoes).

-spec num_readies(commitment()) -> non_neg_integer().
num_readies(#commitment{readies=Readies}) ->
    maps:size(Readies).

-spec matrix(commitment()) -> dkg_commitmentmatrix:matrix().
matrix(#commitment{matrix=Matrix}) ->
    Matrix.

-spec set_matrix(commitment(), dkg_commitmentmatrix:matrix()) -> commitment().
set_matrix(C = #commitment{}, Matrix) ->
    C#commitment{matrix=Matrix}.

-spec serialize(commitment()) -> serialized_commitment().
serialize(#commitment{matrix=Matrix,
                      generator=Generator,
                      nodes=Nodes,
                      echoes=Echoes,
                      readies=Readies}) ->
    #serialized_commitment{matrix=dkg_commitmentmatrix:serialize(Matrix),
                           generator=erlang_pbc:element_to_binary(Generator),
                           nodes=Nodes,
                           echoes=maps:map(fun(_K, V) -> erlang_pbc:element_to_binary(V) end, Echoes),
                           readies=maps:map(fun(_K, V) -> erlang_pbc:element_to_binary(V) end, Readies)}.

-spec deserialize(serialized_commitment(), erlang_pbc:element()) -> commitment().
deserialize(#serialized_commitment{matrix=SerializedMatrix,
                                   generator=SerializedGenerator,
                                   nodes=Nodes,
                                   echoes=Echoes,
                                   readies=Readies}, U) ->
    #commitment{matrix=dkg_commitmentmatrix:deserialize(SerializedMatrix, U),
                generator=erlang_pbc:binary_to_element(U, SerializedGenerator),
                nodes=Nodes,
                echoes=maps:map(fun(_K, V) -> erlang_pbc:binary_to_element(U, V) end, Echoes),
                readies=maps:map(fun(_K, V) -> erlang_pbc:binary_to_element(U, V) end, Readies)}.
