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

-export([simple_test/1]).

%% common test callbacks

all() -> [simple_test].

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
    NodeNames = [eric, kenny, kyle, ike, stan],
    Nodes = dkg_ct_utils:pmap(fun(Node) ->
                                        dkg_ct_utils:start_node(Node, Config, TestCase)
                                end, NodeNames),

    _ = [dkg_ct_utils:connect(Node) || Node <- NodeNames],

    {ok, _} = ct_cover:add_nodes(Nodes),
    [{nodes, Nodes} | Config].

end_per_testcase(_TestCase, Config) ->
    Nodes = proplists:get_value(nodes, Config),
    dkg_ct_utils:pmap(fun(Node) -> ct_slave:stop(Node) end, Nodes),
    ok.

%% test cases

simple_test(Config) ->
    Nodes = proplists:get_value(nodes, Config),

    %% master starts the dealer
    N = length(Nodes),
    F = (N - 1 div 3),
    T = F,
    Group = erlang_pbc:group_new('SS512'),
    G1 = erlang_pbc:element_from_hash(erlang_pbc:element_new('G1', Group), crypto:strong_rand_bytes(32)),
    G2 = case erlang_pbc:pairing_is_symmetric(Group) of
             true -> G1;
             false -> erlang_pbc:element_from_hash(erlang_pbc:element_new('G2', Group), crypto:strong_rand_bytes(32))
         end,

    %% load dkg_worker on each node
    {Mod, Bin, _} = code:get_object_code(dkg_worker),
    _ = dkg_ct_utils:pmap(fun(Node) ->
                                    rpc:call(Node, erlang, load_module, [Mod, Bin])
                            end, Nodes),

    %% start a hbbft_worker on each node
    Workers = [{Node, rpc:call(Node, dkg_worker, start_link, [I, N, F, T, 'SS512', erlang_pbc:element_to_binary(G1), erlang_pbc:element_to_binary(G2), 0])} || {I, Node} <- enumerate(Nodes)],
    ok = global:sync(),

    ct:pal("workers ~p", [Workers]),

    [ link(W) || {_, {ok, W}} <- Workers ],

    %% begin the DKG
    [ dkg_worker:start_round(W) || {_ ,{ok, W}} <- Workers  ],

    %% wait for DKG to complete
    ok = dkg_ct_utils:wait_until(fun() ->
                                         [ ct:pal("~p is done? ~p", [Node, dkg_worker:is_done(W)]) || {Node, {ok, W}} <- Workers],
                                         lists:all(fun({_Node, {ok, W}}) ->
                                                           dkg_worker:is_done(W)
                                                   end, Workers)
                                 end, 60*2, 500),

    %Keys = [ dkg_worker:get_pubkey(W) || W <- Workers],

    ok.

enumerate(List) ->
    lists:zip(lists:seq(1, length(List)), List).
