-module(dkg_worker).

-behaviour(gen_server).

-export([
    start_link/5,
    start_round/1,
    is_done/1,
    dkg_status/1,
    get_pubkey/1,
    sign_share/2,
    dec_share/2,
    verify_signature_share/3,
    combine_signature_shares/2,
    verify_decryption_share/3,
    combine_decryption_shares/3
]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2]).

-record(state, {
    n :: pos_integer(),
    f :: pos_integer(),
    t :: pos_integer(),
    id :: pos_integer(),
    dkg :: dkg_hybriddkg:dkg(),
    round :: binary(),
    key_share :: undefined | tc_key_share:tc_key_share(),
    commitment_cache_fun
}).

start_round(Pid) ->
    gen_server:cast(Pid, start_round).

is_done(Pid) ->
    gen_server:call(Pid, is_done, infinity).

dkg_status(Pid) ->
    gen_server:call(Pid, dkg_status, infinity).

get_pubkey(Pid) ->
    gen_server:call(Pid, get_pubkey, infinity).

sign_share(Pid, Msg) ->
    gen_server:call(Pid, {sign_share, Msg}, infinity).

dec_share(Pid, Share) ->
    gen_server:call(Pid, {dec_share, Share}, infinity).

verify_signature_share(Pid, Share, Msg) ->
    gen_server:call(Pid, {verify_signature_share, Share, Msg}, infinity).

combine_signature_shares(Pid, Shares) ->
    gen_server:call(Pid, {combine_signature_shares, Shares}, infinity).

verify_decryption_share(Pid, Share, CipherText) ->
    gen_server:call(Pid, {verify_decryption_share, Share, CipherText}, infinity).

combine_decryption_shares(Pid, Shares, CipherText) ->
    gen_server:call(Pid, {combine_decryption_shares, Shares, CipherText}, infinity).

start_link(Id, N, F, T, Round) ->
    gen_server:start_link({global, name(Id)}, ?MODULE, [Id, N, F, T, Round], []).

init([Id, N, F, T, Round]) ->
    CCacheFun = dkg_util:commitment_cache_fun(),
    DKG = dkg_hybriddkg:init(Id, N, F, T, Round, [
        {signfun, fun(_) -> <<"lol">> end},
        {verifyfun, fun(_, _, _) -> true end},
        {commitment_cache_fun, CCacheFun}
    ]),
    {ok, #state{
        n = N,
        f = F,
        t = T,
        id = Id,
        round = Round,
        dkg = DKG,
        commitment_cache_fun = CCacheFun
    }}.

handle_call(is_done, _From, State) ->
    {reply, State#state.key_share /= undefined, State};
handle_call(dkg_status, _From, #state{dkg = DKG} = State) ->
    {reply, dkg_hybriddkg:status(DKG), State};
handle_call(get_pubkey, _From, State) ->
    {reply, tc_key_share:public_key(State#state.key_share), State};
handle_call({sign_share, MessageToSign}, _From, State) ->
    {reply, tc_key_share:sign_share(State#state.key_share, MessageToSign), State};
handle_call({dec_share, CipherText}, _From, State) ->
    {reply, tc_key_share:decrypt_share(State#state.key_share, CipherText), State};
handle_call({combine_signature_shares, Shares}, _From, State) ->
    {reply, tc_key_share:combine_signature_shares(State#state.key_share, Shares), State};
handle_call({verify_signature_share, Share, Msg}, _From, State) ->
    {reply, tc_key_share:verify_signature_share(State#state.key_share, Share, Msg), State};
handle_call({combine_decryption_shares, Shares, CipherText}, _From, State) ->
    {reply, tc_key_share:combine_decryption_shares(State#state.key_share, Shares, CipherText),
        State};
handle_call({verify_decryption_share, Share, CipherText}, _From, State) ->
    {reply, tc_key_share:verify_decryption_share(State#state.key_share, Share, CipherText), State};
handle_call(Msg, _From, State) ->
    io:format("unhandled msg ~p~n", [Msg]),
    {reply, ok, State}.

handle_cast(start_round, State) ->
    NewState = dispatch(dkg_hybriddkg:start(State#state.dkg), State),
    {noreply, NewState};
handle_cast({dkg, PeerID, Msg}, State = #state{dkg = DKG}) ->
    NewState = dispatch(dkg_hybriddkg:handle_msg(DKG, PeerID, Msg), State),
    {noreply, NewState};
handle_cast(Msg, State) ->
    io:format("unhandled msg ~p~n", [Msg]),
    {noreply, State}.

handle_info(Msg, State) ->
    io:format("unhandled msg ~p~n", [Msg]),
    {noreply, State}.

dispatch({NewDKG, {result, KeyShare}}, State) ->
    update_dkg(NewDKG, State#state{key_share = KeyShare});
dispatch({NewDKG, {send, ToSend}}, State) ->
    do_send(ToSend, State),
    update_dkg(NewDKG, State);
dispatch({NewDKG, ok}, State) ->
    update_dkg(NewDKG, State);
dispatch({NewDKG, Other}, State) ->
    io:format("UNHANDLED ~p~n", [Other]),
    State#state{dkg = NewDKG};
dispatch(Other, State) ->
    io:format("UNHANDLED2 ~p~n", [Other]),
    State.

do_send([], _) ->
    ok;
do_send([{unicast, Dest, Msg} | T], State) ->
    gen_server:cast({global, name(Dest)}, {dkg, State#state.id, Msg}),
    do_send(T, State);
do_send([{multicast, Msg} | T], State) ->
    %io:format("~p multicasting ~p to ~p~n", [State#state.id, Msg, [global:whereis_name(name(Dest)) || Dest <- lists:seq(1, State#state.n)]]),
    [
        gen_server:cast({global, name(Dest)}, {dkg, State#state.id, Msg})
        || Dest <- lists:seq(1, State#state.n)
    ],
    do_send(T, State).

%% helper functions
update_dkg(DKG, State) ->
    NewDKG = dkg_hybriddkg:deserialize(
        dkg_hybriddkg:serialize(DKG),
        fun(_) -> <<"lol">> end,
        fun(_, _, _) -> true end,
        State#state.commitment_cache_fun
    ),
    State#state{dkg = NewDKG}.

name(N) ->
    list_to_atom(lists:flatten(["dkg_worker_", integer_to_list(N)])).
