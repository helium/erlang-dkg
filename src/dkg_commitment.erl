-module(dkg_commitment).

-record(commitment, {
          matrix :: dkg_commitmentmatrix:matrix(),
          echoes= #{} :: map(),
          readies=#{} :: map()
         }).

-export([new/1, new/2, cmp/2, mul/2, verify_poly/3, public_key_share/2, verify_point/4, interpolate/3, add_echo/3, add_ready/3, num_echoes/1, num_readies/1]).

new(_NodeIDs) ->
    #commitment{matrix=dkg_commitmentmatrix:new()}.

new(_NodeIDs, BiPoly) ->
    #commitment{matrix=dkg_commitmentmatrix:new(BiPoly)}.

cmp(CommitmentA, CommitmentB) ->
    dkg_commitmentmatrix:cmp(CommitmentA#commitment.matrix, CommitmentB#commitment.matrix).

mul(CommitmentA, CommitmentB) ->
    dkg_commitmentmatrix:mul(CommitmentA#commitment.matrix, CommitmentB#commitment.matrix).

verify_poly(Commitment, VerifierID, Poly) ->
    dkg_commitmentmatrix:verify_poly(Commitment#commitment.matrix, VerifierID, Poly).

public_key_share(Commitment, NodeID) ->
    dkg_commitmentmatrix:public_key_share(Commitment#commitment.matrix, NodeID).

verify_point(Commitment, SenderID, VerifierID, Point) ->
    dkg_commitmentmatrix:verify_point(Commitment#commitment.matrix, SenderID, VerifierID, Point).

interpolate(Commitment, EchoOrReady, _ActiveNodes) ->
    _Map = case EchoOrReady of
              echo -> Commitment#commitment.echoes; 
              ready -> Commitment#commitment.readies
          end,
    %% TODO
    ok.

add_echo(Commitment = #commitment{echoes=Echoes}, NodeID, Echo) ->
    case maps:is_key(NodeID, Echoes) of
        true ->
            {false, Commitment};
        false ->
            {true, Commitment#commitment{echoes = maps:put(NodeID, Echo, Echoes)}}
    end.

add_ready(Commitment = #commitment{readies=Readies}, NodeID, Ready) ->
    case maps:is_key(NodeID, Readies) of
        true ->
            {false, Commitment};
        false ->
            {true, Commitment#commitment{readies = maps:put(NodeID, Ready, Readies)}}
    end.

num_echoes(#commitment{echoes=Echoes}) ->
    maps:size(Echoes).

num_readies(#commitment{readies=Readies}) ->
    maps:size(Readies).
