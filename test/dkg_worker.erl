-module(dkg_worker).

-behaviour(gen_server).

-export([start_link/8, start_round/1, is_done/1, get_pubkey/1, sign_share/2, dec_share/2]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2]).

-record(state, {
          n :: pos_integer(),
          f :: pos_integer(),
          t :: pos_integer(),
          id :: pos_integer(),
          dkg :: any(),
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

init([Id, N, F, T, Curve, G1, G2, Round]) ->
    DKG = dkg_hybriddkg:init(Id, N, F, T, G1, G2, Round),
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
    State#state{privkey=PrivKey, dkg=NewDKG};
dispatch({NewDKG, {send, ToSend}}, State) ->
    do_send(ToSend, State),
    State#state{dkg=NewDKG};
dispatch({NewDKG, ok}, State) ->
    State#state{dkg=NewDKG};
dispatch({NewDKG, Other}, State) ->
    io:format("UNHANDLED ~p~n", [Other]),
    State#state{dkg=NewDKG};
dispatch(Other, State) ->
    io:format("UNHANDLED2 ~p~n", [Other]),
    State.

do_send([], _) ->
    ok;
do_send([{unicast, Dest, Msg}|T], State) ->
    %io:format("~p unicasting ~p to ~p~n", [State#state.id, Msg, global:whereis_name(name(Dest))]),
    gen_server:cast({global, name(Dest)}, {dkg, State#state.id, Msg}),
    do_send(T, State);
do_send([{multicast, Msg}|T], State) ->
    %io:format("~p multicasting ~p to ~p~n", [State#state.id, Msg, [global:whereis_name(name(Dest)) || Dest <- lists:seq(1, State#state.n)]]),
    [ gen_server:cast({global, name(Dest)}, {dkg, State#state.id, Msg}) || Dest <- lists:seq(1, State#state.n)],
    do_send(T, State).


%% helper functions
name(N) ->
    list_to_atom(lists:flatten(["dkg_worker_", integer_to_list(N)])).

