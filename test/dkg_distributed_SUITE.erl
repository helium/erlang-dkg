-module(dkg_distributed_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("kernel/include/inet.hrl").

-export([
         init_per_suite/1,
         end_per_suite/1,
         init_per_testcase/2,
         end_per_testcase/2,
         all/0
        ]).

-export([symmetric_test/1]).

%% common test callbacks

all() -> [symmetric_test].

init_per_suite(Config) ->
    os:cmd(os:find_executable("epmd")++" -daemon"),
    {ok, Hostname} = inet:gethostname(),
    case net_kernel:start([list_to_atom("runner@"++Hostname), shortnames]) of
        {ok, _} -> ok;
        {error, {already_started, _}} -> ok;
        {error, {{already_started, _},_}} -> ok
    end,
    Config.

end_per_suite(Config) ->
    %% per suite cleanup, placeholder
    Config.

init_per_testcase(TestCase, Config) ->
    %% assuming each testcase will work with 7 nodes for now
    NodeNames = [eric, kenny, kyle, ike, stan, randy, butters],
    Nodes = dkg_ct_utils:pmap(fun(Node) ->
                                        dkg_ct_utils:start_node(Node, Config, TestCase)
                                end, NodeNames),

    _ = [dkg_ct_utils:connect(Node) || Node <- NodeNames],

    N = length(Nodes),
    F = 0,
    T = 2,
    {ok, _} = ct_cover:add_nodes(Nodes),
    [{nodes, Nodes}, {n, N}, {f, F}, {t, T} | Config].

end_per_testcase(_TestCase, Config) ->
    Nodes = proplists:get_value(nodes, Config),
    dkg_ct_utils:pmap(fun(Node) -> catch ct_slave:stop(Node) end, Nodes),
    ok.

%% test cases

symmetric_test(Config) ->
    Nodes = proplists:get_value(nodes, Config),
    N = proplists:get_value(n, Config),
    F = proplists:get_value(f, Config),
    T = proplists:get_value(t, Config),
    run(N, F, T, Nodes),
    ok.

run(N, F, T, Nodes) ->
    %% load dkg_worker on each node
    {Mod, Bin, _} = code:get_object_code(dkg_worker),
    _ = dkg_ct_utils:pmap(fun(Node) ->
                                    rpc:call(Node, erlang, load_module, [Mod, Bin])
                            end, Nodes),

    %% start a dkg_worker on each node
    Workers = [{Node, rpc:call(Node,
                               dkg_worker,
                               start_link,
                               [I, N, F, T, <<0>>])} || {I, Node} <- dkg_test_utils:enumerate(Nodes)],
    ok = global:sync(),

    ct:pal("workers ~p", [Workers]),

    [ link(W) || {_, {ok, W}} <- Workers ],

    %% begin the DKG
    [ dkg_worker:start_round(W) || {_ ,{ok, W}} <- Workers  ],

    %% wait for DKG to complete
    ok = dkg_ct_utils:wait_until(fun() ->
                                         lists:all(fun({_Node, {ok, W}}) ->
                                                           dkg_worker:is_done(W)
                                                   end, Workers)
                                 end, 60*2, 1000),

    _ = [ ct:pal("~p is_done? :~p", [Node, dkg_worker:is_done(W)]) || {Node, {ok, W}} <- Workers],

    true = check_status(Workers),

    [ unlink(W) || {_, {ok, W}} <- Workers ].

%% helper functions

check_status(Workers) ->
    Statuses = [dkg_worker:dkg_status(W) || {_Node, {ok, W}} <- Workers],

    Check1 = lists:all(fun(Status) ->
                               5 == maps:get(echoes_required, Status) andalso
                               3 == maps:get(readies_required, Status)
                       end,
                       Statuses),

    CountShares = lists:foldl(fun(Status, Acc) ->
                                      SharesMap = maps:get(shares_map, Status),

                                      CountDone = maps:fold(fun(_ID, Result, Acc2) ->
                                                                    case maps:get(done, Result, false) of
                                                                        true -> Acc2 + 1;
                                                                        false -> Acc2
                                                                    end
                                                            end, 0, SharesMap),

                                      CountDone + Acc
                              end,
                              0,
                              Statuses),

    %% for each worker, we expect i - 1 shares worse case
    MinimumExpectedShares = (length(Workers) - 1) * length(Workers),
    Check2 = CountShares >= MinimumExpectedShares,

    Check1 andalso Check2.
