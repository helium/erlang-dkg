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

-export([symmetric_test/1, asymmetric_test/1]).

%% common test callbacks

all() -> [symmetric_test, asymmetric_test].

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
    %% assuming each testcase will work with 5 nodes for now
    NodeNames = [eric, kenny, kyle, ike, stan, randy, butters, token, jimmy, timmy],
    Nodes = dkg_ct_utils:pmap(fun(Node) ->
                                        dkg_ct_utils:start_node(Node, Config, TestCase)
                                end, NodeNames),

    _ = [dkg_ct_utils:connect(Node) || Node <- NodeNames],

    N = length(Nodes),
    F = 3,
    T = 1,
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
    {G1, G2} = generate('SS512'),
    run(N, F, T, 'SS512', G1, G2, Nodes),
    ok.

asymmetric_test(Config) ->
    Nodes = proplists:get_value(nodes, Config),
    N = proplists:get_value(n, Config),
    F = proplists:get_value(f, Config),
    T = proplists:get_value(t, Config),
    {G1, G2} = generate('MNT224'),
    run(N, F, T, 'MNT224', G1, G2, Nodes),
    ok.

enumerate(List) ->
    lists:zip(lists:seq(1, length(List)), List).

generate(Curve) ->
    Group = erlang_pbc:group_new(Curve),
    G1 = erlang_pbc:element_from_hash(erlang_pbc:element_new('G1', Group), crypto:strong_rand_bytes(32)),
    G2 = case erlang_pbc:pairing_is_symmetric(Group) of
             true -> G1;
             false -> erlang_pbc:element_from_hash(erlang_pbc:element_new('G2', Group), crypto:strong_rand_bytes(32))
         end,
    {G1, G2}.

run(N, F, T, Curve, G1, G2, Nodes) ->
    %% load dkg_worker on each node
    {Mod, Bin, _} = code:get_object_code(dkg_worker),
    _ = dkg_ct_utils:pmap(fun(Node) ->
                                    rpc:call(Node, erlang, load_module, [Mod, Bin])
                            end, Nodes),

    %% start a dkg_worker on each node
    Workers = [{Node, rpc:call(Node,
                               dkg_worker,
                               start_link,
                               [I, N, F, T, Curve, erlang_pbc:element_to_binary(G1), erlang_pbc:element_to_binary(G2), 0])} || {I, Node} <- enumerate(Nodes)],
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

    [ unlink(W) || {_, {ok, W}} <- Workers ].
