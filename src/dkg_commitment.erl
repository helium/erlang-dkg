-module(dkg_commitment).

-export([new/3,
         generator/1,
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
         echoes/1,
         readies/1,
         matrix/1,
         set_matrix/2,
         serialize/1,
         deserialize/2
        ]).

-record(commitment, {
          matrix :: dkg_commitmentmatrix:matrix() | binary(),
          generator :: erlang_pbc:element() | binary(),
          nodes = [] :: [pos_integer()],
          echoes = #{} :: echoes(),
          readies = #{} :: readies()
         }).

-type commitment() :: #commitment{}.
-type echoes() :: #{pos_integer() => erlang_pbc:element()} | #{pos_integer() => binary()}.
-type readies() :: #{pos_integer() => erlang_pbc:element()} | #{pos_integer() => binary()}.

-export_type([commitment/0, readies/0]).

-spec new(NodeIDs :: [pos_integer(),...],
          Generator :: erlang_pbc:element(),
          Degree :: integer() | dkg_bipolynomial:bipolynomial()) -> commitment().
new(NodeIDs, Generator, Degree) when is_integer(Degree) ->
    Matrix = dkg_commitmentmatrix:new(Generator, Degree),
    #commitment{nodes=NodeIDs, matrix=Matrix, generator=Generator};
new(NodeIDs, Generator, BiPoly) ->
    Matrix = dkg_commitmentmatrix:new(Generator, BiPoly),
    #commitment{nodes=NodeIDs, matrix=Matrix,  generator=Generator}.

-spec generator(commitment()) -> erlang_pbc:element() | binary().
generator(Commitment) -> Commitment#commitment.generator.

-spec cmp(commitment(), commitment()) -> boolean().
cmp(CommitmentA, CommitmentB) ->
    dkg_commitmentmatrix:cmp({generator(CommitmentA), matrix(CommitmentA)}, {generator(CommitmentB), matrix(CommitmentB)}).

-spec mul(commitment(), commitment()) -> commitment().
mul(CommitmentA, CommitmentB) ->
    NewMatrix = dkg_commitmentmatrix:mul(matrix(CommitmentA), matrix(CommitmentB)),
    CommitmentA#commitment{matrix=NewMatrix}.

-spec verify_poly(commitment(), pos_integer(), dkg_polynomial:polynomial()) -> boolean().
verify_poly(Commitment, VerifierID, Poly) ->
    dkg_commitmentmatrix:verify_poly(generator(Commitment), matrix(Commitment), VerifierID, Poly).

-spec public_key_share(commitment(), pos_integer()) -> erlang_pbc:element().
public_key_share(Commitment, NodeID) ->
    dkg_commitmentmatrix:public_key_share(generator(Commitment), matrix(Commitment), NodeID).

-spec verify_point(commitment(), pos_integer(), pos_integer(), erlang_pbc:element()) -> boolean().
verify_point(Commitment, SenderID, VerifierID, Point) ->
    dkg_commitmentmatrix:verify_point(generator(Commitment), matrix(Commitment), SenderID, VerifierID, Point).

-spec interpolate(commitment(), echo | ready, [pos_integer()]) -> [erlang_pbc:element()].
interpolate(Commitment, EchoOrReady, ActiveNodeIDs) ->
    Map = case EchoOrReady of
              echo -> echoes(Commitment);
              ready -> readies(Commitment)
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

-spec echoes(commitment()) -> echoes().
echoes(#commitment{echoes=Echoes}) ->
    Echoes.

-spec readies(commitment()) -> readies().
readies(#commitment{readies=Readies}) ->
    Readies.

-spec matrix(commitment()) -> dkg_commitmentmatrix:matrix().
matrix(#commitment{matrix=Matrix}) ->
    Matrix.

-spec set_matrix(commitment(), dkg_commitmentmatrix:matrix()) -> commitment().
set_matrix(C = #commitment{}, Matrix) ->
    C#commitment{matrix=dkg_commitmentmatrix:serialize(Matrix)}.

-spec serialize(commitment()) -> commitment().
serialize(C=#commitment{matrix=Matrix,
                        generator=Generator,
                        echoes=Echoes,
                        readies=Readies}) ->
    C#commitment{matrix=dkg_commitmentmatrix:serialize(Matrix),
                 generator=erlang_pbc:element_to_binary(Generator),
                 echoes=maps:map(fun(_K, V) -> erlang_pbc:element_to_binary(V) end, Echoes),
                 readies=maps:map(fun(_K, V) -> erlang_pbc:element_to_binary(V) end, Readies)}.

-spec deserialize(commitment(), erlang_pbc:element()) -> commitment().
deserialize(C=#commitment{matrix=Matrix,
                          generator=SerializedGenerator,
                          echoes=Echoes,
                          readies=Readies}, U) ->
    C#commitment{matrix=dkg_commitmentmatrix:deserialize(Matrix, U),
                 generator=erlang_pbc:binary_to_element(U, SerializedGenerator),
                 echoes=maps:map(fun(_K, V) -> erlang_pbc:binary_to_element(U, V) end, Echoes),
                 readies=maps:map(fun(_K, V) -> erlang_pbc:binary_to_element(U, V) end, Readies)}.
