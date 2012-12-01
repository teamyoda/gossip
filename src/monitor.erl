-module(monitor).
-export([start/0, execute/0, get_min/1, get_max/1, get_average/1, 
        make_neighbors/2, make_neighbors_multiple/2, make_neighbors_graph/2, 
        step/1, create_nodes/1, create_nodes_remote/1,
        get_median/1, get_fragments/1, 
        store_fragment/3, request_fragment/2, query_node_one/1, step/2,
        create_network/1, create_network_remote/1]).

-export([read_fragment_file/2]).
-include("include/message.hrl").

%% Monitor for checking the status of any gossip node

%% Start the monitor
start() ->
    Monitor = spawn(monitor, execute, []),
    register(monitor, Monitor),
    ok.

%% Ask any node what its min is
get_min(To_PID) ->
    To_PID ! #request{from=monitor, field=min},
    ok.

%% Ask any node what its max is
get_max(To_PID) ->
    To_PID ! #request{from=monitor, field=max},
    ok.

get_median(To_PID) ->
    To_PID ! #request{from=monitor, field=median},
    ok.

%% Ask any node what its average is
get_average(To_PID) ->
    To_PID ! #request{from=monitor, field=average},
    ok.

get_fragments(To_PID) ->
    To_PID ! #request{from=monitor, field=fragments},
    ok.

store_fragment(Number, Data, Nodes) when is_list(Nodes) ->
    DestNode = lists:nth(Number, Nodes),
    Node_1 = lists:nth(1, Nodes),
    Node_1 ! #fragment{owner=DestNode, sender=Node_1, data=Data, method=store},
    ok.

request_fragment(Number, Nodes) when is_list(Nodes) ->
    Node = lists:nth(Number, Nodes),
    Node_1 = lists:nth(1, Nodes),
    Node_1 ! #fragment{owner=Node, sender=Node_1, replyto=Node_1, method=request},
    ok.

query_node_one(Nodes) when is_list(Nodes) ->
    Node_1 = lists:nth(1, Nodes),
    get_fragments(Node_1),
    ok.

%% Makes nodes into  neighbors
make_neighbors(PID_1, PID_2) ->
    PID_1 ! #add_neighbor{neighbor=PID_2},
    PID_2 ! #add_neighbor{neighbor=PID_1},
    ok.

%% Makes a node neighbors with all nodes in a list
make_neighbors_multiple(PID_1, []) ->
    ok;

make_neighbors_multiple(PID_1, [Head | Rest]) ->
    make_neighbors(PID_1, Head),
    make_neighbors_multiple(PID_1, Rest).

%% Assigns neighbors to each node based on a graph. Takes in the graph and the node list.
make_neighbors_graph(Graph, []) ->
    ok;

make_neighbors_graph(Graph, [Head | Rest]) ->
    Neighbors = generate_graph:get_neighbors(Graph, Head),
    make_neighbors_multiple(Head, Neighbors),
    make_neighbors_graph(Graph, Rest).

read_fragment_file(Fname, Number) ->
    case file:open(Fname, [read, raw, binary]) of
 {ok, Fd} ->
     case file:read(Fd, Number) of 
        {ok, Data} ->
            file:close(Fd),
            Data;
        {error, Reason} ->
            {error, Reason}
        end;
 {error, Reason} ->
     {error, Reason}
    end.

generate_fragment() ->
    generate_fragment_helper(20, []).
generate_fragment_helper(0, Fragment) ->
    Fragment;
generate_fragment_helper(Number, Fragment) ->
    Entry = random:uniform(100),
    generate_fragment_helper(Number-1, [Entry|Fragment]).

create_nodes(Number) ->
    create_nodes_helper(Number, []).
create_nodes_helper(0, Nodes) ->
    Nodes;
create_nodes_helper(Number, Nodes) ->
    Fragment = generate_fragment(),
    PID = node:start_node(Fragment),
    create_nodes_helper(Number-1, [PID|Nodes]).

create_nodes_remote(Number) ->
    create_nodes_remote_helper(Number, []).
create_nodes_remote_helper(0, Nodes) ->
    Nodes;
create_nodes_remote_helper(Number, Nodes) ->
    Fragment = generate_fragment(),
    PID = node:start_remote_node(Fragment),
    create_nodes_remote_helper_2(Number-1, [PID|Nodes]).
create_nodes_remote_helper_2(0, Nodes) ->
    Nodes;
create_nodes_remote_helper_2(Number, Nodes) ->
    Fragment = generate_fragment(),
    PID = node:start_node(Fragment),
    create_nodes_remote_helper(Number-1, [PID|Nodes]).

create_network(Size) ->
    Number = generate_graph:calc_nodes(Size),
    Nodes = create_nodes(Number),
    Graph = generate_graph:build_graph(Nodes),
    make_neighbors_graph(Graph, Nodes),
    Nodes.

create_network_remote(Size) ->
    Number = generate_graph:calc_nodes(Size),
    Nodes = create_nodes_remote(Number),
    Graph = generate_graph:build_graph(Nodes),
    make_neighbors_graph(Graph, Nodes),
    Nodes.

step([]) ->
    ok;

step([Head | Rest]) ->
    Head ! step,
    step(Rest).

step(_, 0) -> ok;
step(Nodes, Number) ->
    timer:sleep(1),
    step(Nodes),
    step(Nodes, Number - 1).

execute() ->
    receive
        kill ->
            exit(normal);

        Data ->
            io:format("Received: ~p~n", [Data]),
            execute()
    end.

