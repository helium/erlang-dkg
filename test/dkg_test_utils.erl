-module(dkg_test_utils).

-export([do_send_outer/4, random_n/2]).

do_send_outer(Mod, [], States, Acc) ->
    case get_timers() of
        [] ->
            {States, Acc};
        Timers ->
            ct:pal("Timers ~p", [Timers]),
            {R, NewStates} = do_send(Mod, {0, {send, [ {unicast, J, timeout} || J <- Timers]}}, [], States),
            erlang:put(timers, []),
            do_send_outer(Mod, R, NewStates, Acc)
    end;
do_send_outer(Mod, [{result, {Id, Result}} | T], Pids, Acc) ->
    do_send_outer(Mod, T, Pids, sets:add_element({result, {Id, Result}}, Acc));
do_send_outer(Mod, [H|T], States, Acc) ->
    {R, NewStates} = do_send(Mod, H, [], States),
    do_send_outer(Mod, T++R, NewStates, Acc).

do_send(_Mod, {Id, start_timer}, Acc, States) ->
    set_timer(Id),
    {Acc, States};
do_send(_Mod, {Id, {result, Result}}, Acc, States) ->
    cancel_timer(Id),
    {[{result, {Id, Result}} | Acc], States};
do_send(_Mod, {_, ok}, Acc, States) ->
    {Acc, States};
do_send(_Mod, {_, {send, []}}, Acc, States) ->
    {Acc, States};
do_send(Mod, {Id, {send, [{unicast, J, Msg}|T]}}, Acc, States) ->
    case lists:keyfind(J, 1, States) of
        false ->
            do_send(Mod, {Id, {send, T}}, Acc, States);
    {J, State} ->
            {NewState, Result} = Mod:handle_msg(State, Id, Msg),
            do_send(Mod, {Id, {send, T}}, [{J, Result}|Acc], lists:keyreplace(J, 1, States, {J, NewState}))
    end;
do_send(Mod, {Id, {send, [{multicast, Msg}|T]}}, Acc, States) ->
    Res = lists:map(fun({J, State}) ->
                            {NewState, Result} = Mod:handle_msg(State, Id, Msg),
                            {{J, NewState}, {J, Result}}
                    end, States),
    {NewStates, Results} = lists:unzip(Res),
    do_send(Mod, {Id, {send, T}}, Results ++ Acc, lists:ukeymerge(1, NewStates, States));
do_send(_, Bleh, _, _) ->
    erlang:error(Bleh).

set_timer(Id) ->
    Timers = get_timers(),
    erlang:put(timers, lists:usort([Id|Timers])).

cancel_timer(Id) ->
    Timers = get_timers(),
    erlang:put(timers, Timers -- [Id]).

get_timers() ->
    case erlang:get(timers) of
        undefined -> [];
        R -> R
    end.

random_n(N, List) ->
    lists:sublist(shuffle(List), N).

shuffle(List) ->
    [X || {_,X} <- lists:sort([{rand:uniform(), N} || N <- List])].
