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
          privkey :: undefined | tc_secret_key_share:sk_share() | binary(),
          pubkey_set :: undefined | tc_public_key_set:pk_set() | binary(),
          n :: pos_integer(),
          t :: pos_integer(),
          id :: pos_integer()
         }).

init(DKGArgs) ->
    ID = proplists:get_value(id, DKGArgs),
    N = proplists:get_value(n, DKGArgs),
    F = proplists:get_value(f, DKGArgs),
    T = proplists:get_value(t, DKGArgs),
    Round = proplists:get_value(round, DKGArgs),
    DKG = dkg_hybriddkg:init(ID, N, F, T, Round, [{callback, true}, {signfun, fun(_) -> <<"lol">> end}, {verifyfun, fun(_, _, _) -> true end}]),
    {ok, #state{round=Round, id=ID, n=N, t=T, dkg=DKG}}.

handle_command(start_round, State) ->
    {DKG, {send, ToSend}} = dkg_hybriddkg:start(State#state.dkg),
    {reply, ok, fixup_msgs(ToSend), State#state{dkg=DKG}};
handle_command(is_done, State) ->
    {reply, State#state.privkey /= undefined, ignore};
handle_command(get_pubkey, #state{pubkey_set=PubKeySet}) when PubKeySet /= undefined ->
    {reply, PubKeySet, ignore};
handle_command({sign_share, MessageToSign}, #state{privkey=PrivKey, id=ID}) when PrivKey /= undefined ->
    {reply, {ID - 1, tc_secret_key_share:sign(PrivKey, MessageToSign)}, ignore};
handle_command({dec_share, CipherText}, #state{privkey=PrivKey, id=ID}) when PrivKey /= undefined ->
    {reply, {ID - 1, tc_secret_key_share:decrypt_share(PrivKey, CipherText)}, ignore};
handle_command(status, #state{dkg=DKG}) ->
    {reply, dkg_hybriddkg:status(DKG), ignore};
handle_command(Msg, _State) ->
    ct:pal("unhandled handle_command, Msg: ~p", [Msg]),
    {reply, ok, ignore}.

handle_message(Msg, Actor, State) ->
    case dkg_hybriddkg:handle_msg(State#state.dkg, Actor, binary_to_term(Msg)) of
        {_DKG, ignore} -> ignore;
        {DKG, ok} ->
            {State#state{dkg=DKG}, []};
        {DKG, {result, KeyShare}} ->
            {State#state{dkg=DKG, privkey=tc_key_share:secret_key_share(KeyShare), pubkey_set=tc_key_share:public_key_set(KeyShare)}, []};
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
        {Id, {ready, {Session, SerializedCommitment, Shares, Proof}}} ->
            term_to_binary({Id, {ready, {Session, SerializedCommitment, lists:nth(Actor, Shares), Proof}}})
    end.


serialize(State) ->
    SerializedDKG = dkg_hybriddkg:serialize(State#state.dkg),
    PrivKey = case State#state.privkey of
        undefined ->
            undefined;
        Other ->
            tc_secret_key_share:serialize(Other)
    end,

    PubKeySet = case State#state.pubkey_set of
        undefined ->
            undefined;
        Other2 ->
            tc_public_key_set:serialize(Other2)
    end,

    #{dkg => SerializedDKG,
      round => term_to_binary(State#state.round),
      n => term_to_binary(State#state.n),
      t => term_to_binary(State#state.t),
      privkey => term_to_binary(PrivKey),
      pubkey_set => term_to_binary(PubKeySet),
      id => term_to_binary(State#state.id)}.

deserialize(Map) ->
    #{round := Round0,
      n := N0,
      t := T0,
      id := ID0,
      privkey := PrivKey0,
      pubkey_set := PubKeySet0,
      dkg := DKG0} = Map,
    Round = binary_to_term(Round0),
    N = binary_to_term(N0),
    T = binary_to_term(T0),
    ID = binary_to_term(ID0),

    DKG = dkg_hybriddkg:deserialize(DKG0, fun(_) -> <<"lol">> end, fun(_, _, _) -> true end),
    PrivKey = case binary_to_term(PrivKey0) of
        undefined ->
            undefined;
        Other ->
            tc_secret_key_share:deserialize(Other)
    end,
    PubKeySet = case binary_to_term(PubKeySet0) of
        undefined ->
            undefined;
        Other2 ->
            tc_public_key_set:deserialize(Other2)
    end,

    #state{dkg=DKG,
           privkey=PrivKey,
           pubkey_set=PubKeySet,
           n = N,
           t = T,
           id = ID,
           round = Round}.

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
