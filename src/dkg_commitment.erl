-module(dkg_commitment).

-export([new/4,
         new/3,
         cmp/2,
         verify_poly/3,
         public_key_share/2,
         public_key_set/1,
         verify_point/4,
         interpolate/3,
         add_echo/3,
         add_ready/3,
         add_ready_proof/3,
         num_echoes/1,
         num_readies/1,
         echoes/1,
         ready_proofs/1,
         matrix/1,
         hash/1,
         serialize/1,
         deserialize/2
        ]).

-record(commitment, {
          commitment :: binary(),
          nodes = [] :: [pos_integer()],
          echoes = #{} :: echoes(),
          readies =#{} :: readies(),
          proofs =#{} :: ready_proofs(),
          commitment_cache_fun :: fun((binary() | {binary(), tc_bicommitment:bicommitment()}) -> tc_bicommitment:bicommitment() | ok)
         }).

-record(serialized_commitment, {
          commitment :: binary(),
          nodes = [] :: [pos_integer()],
          echoes = #{} :: #{pos_integer() => binary()},
          readies = #{} :: #{pos_integer() => binary()},
          proofs =#{} :: ready_proofs()
         }).

-type commitment() :: #commitment{}.
-type echoes() :: #{pos_integer() => tc_fr:fr()}.
-type readies() :: #{pos_integer() => tc_fr:fr()}.
-type serialized_commitment() :: #serialized_commitment{}.
-type ready_proofs() :: #{pos_integer() => binary()}.

-export_type([commitment/0, ready_proofs/0, serialized_commitment/0]).

-spec new([pos_integer(),...], tc_bicommitment:bicommitment(), binary(), fun()) -> commitment().
new(NodeIDs, Commitment, SerializedCommitment, CacheFun) ->
    %% seed the cache since we have both here
    ok = CacheFun({SerializedCommitment, Commitment}),
    #commitment{nodes=NodeIDs, commitment=SerializedCommitment, commitment_cache_fun=CacheFun}.

-spec new([pos_integer(),...], tc_bicommitment:bicommitment(), fun()) -> commitment().
new(NodeIDs, Commitment, CacheFun) ->
    SerializedCommitment = tc_bicommitment:serialize(Commitment),
    %% seed the cache since we have both here
    ok = CacheFun({SerializedCommitment, Commitment}),
    #commitment{nodes=NodeIDs, commitment=SerializedCommitment, commitment_cache_fun=CacheFun}.

-spec cmp(commitment(), commitment()) -> boolean().
cmp(CommitmentA, CommitmentB) ->
    CommitmentA#commitment.commitment == CommitmentB#commitment.commitment.

-spec verify_poly(commitment(), pos_integer(), tc_poly:poly()) -> boolean().
verify_poly(Commitment, VerifierID, Poly) ->
    tc_bicommitment:verify_poly(gc(Commitment), Poly, VerifierID).

-spec public_key_share(commitment(), pos_integer()) -> tc_public_key_share:pk_share().
public_key_share(Commitment, NodeID) ->
    tc_public_key_set:public_key_share(tc_public_key_set:from_commitment(tc_bicommitment:row(gc(Commitment), 0)), NodeID-1).

-spec public_key_set(commitment()) -> tc_public_key_set:pk_set().
public_key_set(Commitment) ->
    tc_public_key_set:from_commitment(tc_bicommitment:row(gc(Commitment), 0)).

-spec verify_point(commitment(), pos_integer(), pos_integer(), binary()) -> boolean().
verify_point(Commitment, SenderID, VerifierID, Point) ->
    case maps:get(SenderID, Commitment#commitment.echoes, undefined) == Point orelse
         maps:get(SenderID, Commitment#commitment.readies, undefined) == Point of
        true ->
            true;
        false ->
            tc_bicommitment:validate_point(gc(Commitment), SenderID, VerifierID, tc_fr:deserialize(Point))
    end.

-spec interpolate(commitment(), pos_integer(), echo | ready) -> tc_poly:poly().
interpolate(Commitment, T, EchoOrReady) ->
    Map = case EchoOrReady of
               echo -> Commitment#commitment.echoes;
               ready -> Commitment#commitment.readies
           end,
    Received = [
                {tc_fr:into(Index), tc_fr:deserialize(Val)}
                || {Index, Val} <- lists:sublist(maps:to_list(Map), T+1)
               ],
    tc_poly:interpolate_from_fr(Received).

-spec add_echo(commitment(), pos_integer(), tc_fr:fr()) -> {true | false, commitment()}.
add_echo(Commitment = #commitment{nodes=Nodes, echoes=Echoes}, NodeID, Echo) when NodeID /= 0 ->
    case lists:member(NodeID, Nodes) of
        true ->
            case maps:is_key(NodeID, Echoes) of
                true ->
                    {false, Commitment};
                false ->
                    {true, Commitment#commitment{echoes = maps:put(NodeID, Echo, Echoes)}}
            end;
        false ->
            %% unknown node ID
            {false, Commitment}
    end.

-spec add_ready(commitment(), pos_integer(), tc_fr:fr()) -> {true | false, commitment()}.
add_ready(Commitment = #commitment{nodes=Nodes, readies=Readies}, NodeID, Ready) when NodeID /= 0 ->
    case lists:member(NodeID, Nodes) of
        true ->
            case maps:is_key(NodeID, Readies) of
                true ->
                    {false, Commitment};
                false ->
                    {true, Commitment#commitment{readies = maps:put(NodeID, Ready, Readies)}}
            end;
        false ->
            %% unknown node ID
            {false, Commitment}
    end.

-spec add_ready_proof(commitment(), pos_integer(), binary()) -> {true | false, commitment()}.
add_ready_proof(Commitment = #commitment{nodes=Nodes, proofs=Proofs}, NodeID, ReadyProof) when NodeID /= 0 ->
    case lists:member(NodeID, Nodes) of
        true ->
            case maps:is_key(NodeID, Proofs) of
                true ->
                    {false, Commitment};
                false ->
                    {true, Commitment#commitment{proofs = maps:put(NodeID, ReadyProof, Proofs)}}
            end;
        false ->
            %% unknown node ID
            {false, Commitment}
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

-spec ready_proofs(commitment()) -> ready_proofs().
ready_proofs(#commitment{proofs=Proofs}) ->
    Proofs;
ready_proofs(#serialized_commitment{proofs=Proofs}) ->
    Proofs.

-spec matrix(commitment()) -> binary().
matrix(#commitment{commitment=Commitment}) ->
    Commitment.

hash(#commitment{commitment=Commitment}) ->
    erlang:phash2(Commitment).

-spec serialize(commitment()) -> serialized_commitment().
serialize(#commitment{commitment=Commitment,
                      nodes=Nodes,
                      echoes=Echoes,
                      readies=Readies,
                      proofs=Proofs}) ->
    #serialized_commitment{commitment=Commitment,
                           nodes=Nodes,
                           echoes=Echoes,
                           readies=Readies,
                           proofs=Proofs}.

-spec deserialize(serialized_commitment(), fun()) -> commitment().
deserialize(#serialized_commitment{commitment=Commitment,
                                   nodes=Nodes,
                                   echoes=Echoes,
                                   readies=Readies,
                                   proofs=Proofs}, CCacheFun) ->
    #commitment{commitment=Commitment,
                commitment_cache_fun=CCacheFun,
                nodes=Nodes,
                proofs=Proofs,
                echoes=Echoes,
                readies=Readies}.

gc(#commitment{commitment=Commitment, commitment_cache_fun=Fun}) ->
    Fun(Commitment).

