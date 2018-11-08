-module(dkg_util).

-export([allnodes/1, wrap/2]).

allnodes(N) -> lists:seq(1, N).

%% wrap a subprotocol's outbound messages with a protocol identifier
-spec wrap(Tag :: {vss, non_neg_integer()}, [{multicast, Msg :: any()} | {unicast, non_neg_integer(),  Msg :: any()}]) -> [{multicast, {Tag, Msg}} | {unicast, non_neg_integer(), {Tag, Msg}}].
wrap(_, []) ->
    [];
wrap(Id, [{multicast, Msg}|T]) ->
    [{multicast, {Id, Msg}}|wrap(Id, T)];
wrap(Id, [{unicast, Dest, Msg}|T]) ->
    [{unicast, Dest, {Id, Msg}}|wrap(Id, T)];
wrap(Id, [{callback, Msg}|T]) ->
    [{callback, {Id, Msg}}|wrap(Id, T)].

