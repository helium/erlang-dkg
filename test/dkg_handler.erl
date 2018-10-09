-module(dkg_handler).

-behavior(relcast).

-export([
         init/1,
         handle_command/2,
         handle_message/3,
         callback_message/3,
         serialize/1,
         deserialize/1,
         restore/2
        ]).

-record(state, {
          dkg :: dkg_hybriddkg:dkg() | dkg_hybriddkg:serialized_dkg(),
          round :: integer(),
          curve :: 'SS512' | 'MNT224',
          g1 :: erlang_pbc:element() | binary(),
          g2 :: erlang_pbc:element() | binary(),
          privkey :: undefined | tpke_privkey:privkey() | tpke_privkey:privkey_serialized(),
          n :: pos_integer(),
          t :: pos_integer(),
          id :: pos_integer()
         }).

init(DKGArgs) ->
    ID = proplists:get_value(id, DKGArgs),
    N = proplists:get_value(n, DKGArgs),
    F = proplists:get_value(f, DKGArgs),
    T = proplists:get_value(t, DKGArgs),
    Curve = proplists:get_value(curve, DKGArgs),
    G1 = proplists:get_value(g1, DKGArgs),
    G2 = proplists:get_value(g2, DKGArgs),
    Round = proplists:get_value(round, DKGArgs),
    {G1_Prime, G2_Prime} = case is_binary(G1) andalso is_binary(G2) of
                   true ->
                       Group = erlang_pbc:group_new(Curve),
                       {erlang_pbc:binary_to_element(Group, G1), erlang_pbc:binary_to_element(Group, G2)};
                   false ->
                       {G1, G2}
               end,
    DKG = dkg_hybriddkg:init(ID, N, F, T, G1_Prime, G2_Prime, Round, [{callback, true}]),
    {ok, #state{round=Round, id=ID, n=N, t=T, dkg=DKG, curve=Curve, g1=G1_Prime, g2=G2_Prime}}.

handle_command(start_round, State) ->
    {DKG, {send, ToSend}} = dkg_hybriddkg:start(State#state.dkg),
    {reply, ok, fixup_msgs(ToSend), State#state{dkg=DKG}};
handle_command(is_done, State) ->
    {reply, State#state.privkey /= undefined, ignore};
handle_command(get_pubkey, #state{privkey=PrivKey}) when PrivKey /= undefined ->
    {reply, tpke_privkey:public_key(PrivKey), ignore};
handle_command({sign_share, MessageToSign}, #state{privkey=PrivKey}) when PrivKey /= undefined ->
    {reply, tpke_privkey:sign(PrivKey, MessageToSign), ignore};
handle_command({dec_share, CipherText}, #state{privkey=PrivKey}) when PrivKey /= undefined ->
    {reply, tpke_privkey:decrypt_share(PrivKey, CipherText), ignore};
handle_command(Msg, _State) ->
    ct:pal("unhandled handle_command, Msg: ~p", [Msg]),
    {reply, ok, ignore}.

handle_message(Msg, Actor, State) ->
    case dkg_hybriddkg:handle_msg(State#state.dkg, Actor, binary_to_term(Msg)) of
        {_DKG, ignore} -> ignore;
        {DKG, ok} ->
            {State#state{dkg=DKG}, []};
        {DKG, {result, {Shard, VerificationKey, VerificationKeys}}} ->
            PubKey = tpke_pubkey:init(State#state.n,
                                      State#state.t,
                                      State#state.g1,
                                      State#state.g2,
                                      VerificationKey,
                                      VerificationKeys,
                                      State#state.curve),
            PrivKey = tpke_privkey:init(PubKey, Shard, State#state.id - 1),

            {State#state{dkg=DKG, privkey=PrivKey}, []};
        {DKG, {send, ToSend}} ->
            {State#state{dkg=DKG}, fixup_msgs(ToSend)};
        {DKG, start_timer} ->
            {State#state{dkg=DKG}, []}
    end.

callback_message(Actor, Message, _State) ->
    case binary_to_term(Message) of
        {Id, {send, {Session, SerializedCommitment, Shares}}} ->
            term_to_binary({Id, {send, {Session, SerializedCommitment, lists:nth(Actor, Shares)}}});
        {Id, {echo, {Session, SerializedCommitment, Shares}}} ->
            term_to_binary({Id, {echo, {Session, SerializedCommitment, lists:nth(Actor, Shares)}}});
        {Id, {ready, {Session, SerializedCommitment, Shares}}} ->
            term_to_binary({Id, {ready, {Session, SerializedCommitment, lists:nth(Actor, Shares)}}})
    end.


serialize(State) ->
    SerializedDKG = dkg_hybriddkg:serialize(State#state.dkg),
    G1 = erlang_pbc:element_to_binary(State#state.g1),
    G2 = erlang_pbc:element_to_binary(State#state.g2),
    PrivKey = case State#state.privkey of
                  undefined ->
                      undefined;
                  Other ->
                      tpke_privkey:serialize(Other)
              end,
    term_to_binary(State#state{dkg=SerializedDKG, privkey=PrivKey, g1=G1, g2=G2}).

deserialize(Binary) ->
    State = binary_to_term(Binary),
    Group = erlang_pbc:group_new(State#state.curve),
    G1 = erlang_pbc:binary_to_element(Group, State#state.g1),
    G2 = erlang_pbc:binary_to_element(Group, State#state.g2),
    DKG = dkg_hybriddkg:deserialize(State#state.dkg, G1),
    PrivKey = case State#state.privkey of
        undefined ->
            undefined;
        Other ->
            tpke_privkey:deserialize(Other)
    end,
    #state{dkg=DKG, privkey=PrivKey, g1=G1, g2=G2}.

restore(OldState, _NewState) ->
    {ok, OldState}.

%% helper funcs
fixup_msgs(Msgs) ->
    lists:map(fun({unicast, J, NextMsg}) ->
                      {unicast, J, term_to_binary(NextMsg)};
                 ({multicast, NextMsg}) ->
                      {multicast, term_to_binary(NextMsg)};
                 ({callback, NextMsg}) ->
                      {callback, term_to_binary(NextMsg)}
              end, Msgs).
