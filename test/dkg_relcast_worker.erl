-module(dkg_relcast_worker).

-behaviour(gen_server).

-export([start_link/1, start_round/1, is_done/1, get_pubkey/1, sign_share/2, dec_share/2, status/1]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2]).

-record(state, {
          relcast :: term(),
          id :: integer(),
          peers :: map()
         }).

start_link(Args) ->
    ID = proplists:get_value(id, Args),
    gen_server:start_link({global, name(ID)}, ?MODULE, Args, []).

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

status(Pid) ->
    gen_server:call(Pid, status, infinity).

init(Args) ->
    N = proplists:get_value(n, Args),
    ID = proplists:get_value(id, Args),
    DataDir = proplists:get_value(data_dir, Args),
    Members = lists:seq(1, N),
    {ok, Relcast} = relcast:start(ID, Members, dkg_handler, Args, [{data_dir, DataDir ++ integer_to_list(ID)}]),
    Peers = maps:from_list([{I, undefined} || I <- Members, I /= ID ]),
    {ok, do_send(#state{relcast=Relcast, id=ID, peers=Peers})}.

handle_call(is_done, _From, State) ->
    {Resp, Relcast} = relcast:command(is_done, State#state.relcast),
    {reply, Resp, State#state{relcast=Relcast}};
handle_call(get_pubkey, _From, State) ->
    {Resp, Relcast} = relcast:command(get_pubkey, State#state.relcast),
    {reply, Resp, State#state{relcast=Relcast}};
handle_call({sign_share, MessageToSign}, _From, State) ->
    {Resp, Relcast} = relcast:command({sign_share, MessageToSign}, State#state.relcast),
    {reply, Resp, State#state{relcast=Relcast}};
handle_call({dec_share, CipherText}, _From, State) ->
    {Resp, Relcast} = relcast:command({dec_share, CipherText}, State#state.relcast),
    {reply, Resp, State#state{relcast=Relcast}};
handle_call(status, _From, State) ->
    {Resp, Relcast} = relcast:command(status, State#state.relcast),
    {reply, Resp, State#state{relcast=Relcast}};
handle_call(Msg, _From, State) ->
    io:format("unhandled msg ~p~n", [Msg]),
    {reply, ok, State}.

handle_cast(start_round, State) ->
    {ok, Relcast} = relcast:command(start_round, State#state.relcast),
    {noreply, do_send(State#state{relcast=Relcast})};
handle_cast({ack, Sender}, State) ->
    %% ct:pal("ack, Sender: ~p", [Sender]),
    {ok, NewRelcast} = relcast:ack(Sender, maps:get(Sender, State#state.peers, undefined), State#state.relcast),
    {noreply, do_send(State#state{relcast=NewRelcast, peers=maps:put(Sender, undefined, State#state.peers)})};
handle_cast({dkg, FromId, Msg}, State) ->
    case relcast:deliver(Msg, FromId, State#state.relcast) of
        {ok, NewRelcast} ->
            gen_server:cast({global, name(FromId)}, {ack, State#state.id}),
            {noreply, do_send(State#state{relcast=NewRelcast})};
        _ ->
            {noreply, State}
    end;
handle_cast(Msg, State) ->
    io:format("unhandled msg ~p~n", [Msg]),
    {noreply, State}.

handle_info(Msg, State) ->
    io:format("unhandled msg ~p~n", [Msg]),
    {noreply, State}.

%% helper functions
name(N) ->
    list_to_atom(lists:flatten(["dkg_relcast_worker_", integer_to_list(N)])).

do_send(State) ->
    do_send(maps:to_list(State#state.peers), State).

do_send([], State) ->
    State;
do_send([{I, undefined} | Tail], State) ->
    case relcast:take(I, State#state.relcast) of
        {not_found, NewRelcast} ->
            do_send(Tail, State#state{relcast=NewRelcast});
        {ok, Ref, Msg, NewRelcast} ->
            gen_server:cast({global, name(I)}, {dkg, State#state.id, Msg}),
            do_send(Tail, State#state{relcast=NewRelcast, peers=maps:put(I, Ref, State#state.peers)})
    end;
do_send([_ | Tail], State) ->
    do_send(Tail, State).
