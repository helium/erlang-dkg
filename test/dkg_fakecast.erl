-module(dkg_fakecast).

-behaviour(fakecast).

-export([
         init/1,
         model/7 %,
         %% terminate/2
        ]).

-record(state,
        {
         node_count :: integer(),
         one_stopped :: boolean(),
         results = sets:new() :: sets:set()
        }).

init(Config) ->
    {ok,
     {
      dkg_hybriddkg,
      random,
      favor_concurrent,
      [aaa, bbb, ccc, ddd, eee, fff, ggg, hhh, iii, jjj],  %% are names useful?
      [[Id] ++ Config || Id <- lists:seq(1, 10)], % pull count from config
      5000
     },
     #state{node_count = 9,
            one_stopped = false}
    }.

%% unhappen all messages from vss 3 and 4, to manipulate Qhat on 2
model({{vss,N},_}, _, 2, NodeState, _NewState, _Actions, ModelState) when %N == 3;
                                                                          N == 4 ->
     {actions, [{alter_state, NodeState}], ModelState};
model(_, 1, 2, NodeState, _NewState, _Actions, ModelState) ->
     {actions, [{alter_state, NodeState}], ModelState};
model(_Msg, _from, 1, _NodeState, _NewState, {send,[{multicast,Send}]},
      #state{one_stopped = false} = ModelState) when element(1, Send) == send ->
    %% fakecast:trace("actions: ~p", [_Ac]),
    NewActions = [{unicast, N, Send} || N <- [2,3,4,5]],
    {actions, [{stop_node, 1}, {alter_actions, {send, NewActions}}],
     ModelState#state{one_stopped = true}};
model(_Message, _From, To, _NodeState, _NewState, {result, Result},
      #state{results = Results0} = State) ->
    Results = sets:add_element({result, {To, Result}}, Results0),
    ct:pal("results len ~p ~p", [sets:size(Results), sets:to_list(Results)]),
    case sets:size(Results) == State#state.node_count of
        true ->
            {result, Results};
        false ->
            {continue, State#state{results = Results}}
    end;
model(_Message, _From, _To, _NodeState, _NewState, _Actions, ModelState) ->
    {continue, ModelState}.

