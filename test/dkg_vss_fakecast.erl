-module(dkg_vss_fakecast).

-behaviour(fakecast).

-export([
         init/1,
         model/7 %,
         %% terminate/2
        ]).

-record(state,
        {
         node_count :: integer(),
         results = sets:new() :: sets:set()
        }).

init(Config) ->
    {ok,
     {
      dkg_hybridvss,
      random,
      favor_concurrent,
      [aaa, bbb, ccc, ddd, eee, fff, ggg, hhh, iii, jjj],  %% are names useful?
      [[Id] ++ Config || Id <- lists:seq(1, 10)], % pull count from config
      5000
     },
     #state{node_count = 10}
    }.

%% this model is trivial, we only want to catch outputs and never
%% change anything about the execution
model(_Message, _From, To, _NodeState, _NewState, {result, Result},
      #state{results = Results0} = State) ->
    Results = sets:add_element({result, {To, Result}}, Results0),
    %% ct:pal("results len ~p ~p", [sets:size(Results), sets:to_list(Results)]),
    case sets:size(Results) == State#state.node_count of
        true ->
            {result, Results};
        false ->
            {continue, State#state{results = Results}}
    end;
model(_Message, _From, _To, _NodeState, _NewState, _Actions, ModelState) ->
    {continue, ModelState}.

