-module(dkg_worker).

-behaviour(gen_server).

-export([start_link/8, start_round/1, is_done/1, get_pubkey/1, sign_share/2, dec_share/2]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2]).

-record(state, {
          n :: pos_integer(),
          f :: pos_integer(),
          t :: pos_integer(),
          id :: pos_integer(),
          dkg :: dkg_hybriddkg:dkg(),
          curve :: 'SS512' | 'MNT224',
          g1 :: erlang_pbc:element(),
          g2 :: erlang_pbc:element(),
          round :: non_neg_integer(),
          privkey :: undefined | tpke_privkey:privkey()
         }).

start_round(Pid) ->
    gen_server:cast(Pid, start_round).

is_done(Pid) ->
    gen_server:call(Pid, is_done, infinity).

get_pubkey(Pid) ->
    gen_server:call(Pid, get_pubkey, infinity).

sign_share(Pid, Msg) ->
    gen_server:call(Pid, {sign_share, Msg}, infinity).

dec_share(Pid, Share) ->
    gen_server:call(Pid, {dec_share, Share}, infinity).

start_link(Id, N, F, T, Curve, G1, G2, Round) ->
    gen_server:start_link({global, name(Id)}, ?MODULE, [Id, N, F, T, Curve, G1, G2, Round], []).

init([Id, N, F, T, Curve, G10, G20, Round]) ->
    {G1, G2} = case is_binary(G10) andalso is_binary(G20) of
                   true ->
                       Group = erlang_pbc:group_new(Curve),
                       {erlang_pbc:binary_to_element(Group, G10), erlang_pbc:binary_to_element(Group, G20)};
                   false ->
                       {G10, G20}
               end,
    DKG = dkg_hybriddkg:init(Id, N, F, T, G1, G2, Round, []),
    {ok, #state{n=N, f=F, t=T, id=Id, curve=Curve, g1=G1, g2=G2, round=Round, dkg=DKG}}.

handle_call(is_done, _From, State) ->
    {reply, State#state.privkey /= undefined, State};
handle_call(get_pubkey, _From, State) ->
    {reply, tpke_privkey:public_key(State#state.privkey), State};
handle_call({sign_share, MessageToSign}, _From, State) ->
    {reply, tpke_privkey:sign(State#state.privkey, MessageToSign), State};
handle_call({dec_share, CipherText}, _From, State) ->
    {reply, tpke_privkey:decrypt_share(State#state.privkey, CipherText), State};
handle_call(Msg, _From, State) ->
    io:format("unhandled msg ~p~n", [Msg]),
    {reply, ok, State}.

handle_cast(start_round, State) ->
    NewState = dispatch(dkg_hybriddkg:start(State#state.dkg), State),
    {noreply, NewState};
handle_cast({dkg, PeerID, Msg}, State = #state{dkg=DKG}) ->
    NewState = dispatch(dkg_hybriddkg:handle_msg(DKG, PeerID, Msg), State),
    {noreply, NewState};
handle_cast(Msg, State) ->
    io:format("unhandled msg ~p~n", [Msg]),
    {noreply, State}.

handle_info(Msg, State) ->
    io:format("unhandled msg ~p~n", [Msg]),
    {noreply, State}.

dispatch({NewDKG, {result, {Shard, VerificationKey, VerificationKeys}}}, State) ->
    %ct:pal("~p finished", [State#state.id]),
    PubKey = tpke_pubkey:init(State#state.n, State#state.t, State#state.g1, State#state.g2, VerificationKey, VerificationKeys, State#state.curve),
    PrivKey = tpke_privkey:init(PubKey, Shard, State#state.id - 1),
    update_dkg(NewDKG, State#state{privkey=PrivKey});
dispatch({NewDKG, {send, ToSend}}, State) ->
    do_send(ToSend, State),
    update_dkg(NewDKG, State);
dispatch({NewDKG, ok}, State) ->
    update_dkg(NewDKG, State);
dispatch({NewDKG, Other}, State) ->
    io:format("UNHANDLED ~p~n", [Other]),
    State#state{dkg=NewDKG};
dispatch(Other, State) ->
    io:format("UNHANDLED2 ~p~n", [Other]),
    State.

do_send([], _) ->
    ok;
do_send([{unicast, Dest, Msg}|T], State) ->
    gen_server:cast({global, name(Dest)}, {dkg, State#state.id, Msg}),
    do_send(T, State);
do_send([{multicast, Msg}|T], State) ->
    %io:format("~p multicasting ~p to ~p~n", [State#state.id, Msg, [global:whereis_name(name(Dest)) || Dest <- lists:seq(1, State#state.n)]]),
    [ gen_server:cast({global, name(Dest)}, {dkg, State#state.id, Msg}) || Dest <- lists:seq(1, State#state.n)],
    do_send(T, State).

%% helper functions
update_dkg(DKG, State)->
    NewDKG = dkg_hybriddkg:deserialize(dkg_hybriddkg:serialize(DKG), State#state.g1),
    State#state{dkg=NewDKG}.

name(N) ->
    list_to_atom(lists:flatten(["dkg_worker_", integer_to_list(N)])).

