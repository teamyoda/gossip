-module(monitor).
-export([start/0, 
        %% Basic node queries
        get_min/1, 
        get_max/1, 
        get_average/1, 
        get_median/1, 
        get_fragments/1, 

        %% More advanced queries
        query_node_one/1, 
        query_node/2,
        query_node/1,

        %% Node manipulation
        create_nodes/1, 
        create_nodes_remote/1,
        make_neighbors/2, 
        make_neighbors_multiple/2, 
        make_neighbors_graph/2, 

        %% Network manipulation
        create_network/1, 
        create_network/2,
        create_network_remote/1,

        %% Fragment creation, store, retreive, and analytics
        read_fragment_file/2,
        generate_fragments/1,
        store_fragment/3, 
        request_fragment/2, 
        get_file_stats/1,

        %% Node stepping
        step/1, 
        step/2]).

-include("include/message.hrl").

%% Monitor for checking the status of any gossip node

%% Deprecated. Now kept only for backward's compat.
start() ->
    ok.

%% Ask any node what its min is
%%
%% Arguments:
%%      To_PID: The PID of the node you wish to query
%%
%% Returns:
%%      The minimum value seen by the node
%%      The atom "error" if something goes wrong
get_min(To_PID) ->
    To_PID ! #request{from=self(), field=min},
    receive
        #message{function=min, data=Value} ->
            Value;
        _ -> error
    end.

%% Ask any node what its max is
%%
%% Arguments:
%%      To_PID: The PID of the node you wish to query
%%
%% Returns:
%%      The maximum value seen by the node.
%%      The atom "error" if something goes wrong
get_max(To_PID) ->
    To_PID ! #request{from=self(), field=max},
    receive
        #message{function=max, data=Value} ->
            Value;
        _ -> error
    end.

%% Get the median from a given node
%%
%% Arguments:
%%      To_PID: The PID of the node you wish to query
%%
%% Returns:
%%      The float value of the median as known by the node
%%      The atom "error" if something goes wrong
get_median(To_PID) ->
    To_PID ! #request{from=self(), field=median},
    receive
        #message{function=median, data=Value} ->
            Value;
        _ -> error
    end.

%% Ask any node what its average is
%%
%% Arguments:
%%      To_PID: The PID of the node you wish to query
%%
%% Returns:
%%      The float value of the average as known by the node
%%      The atom "error" if something goes wrong
get_average(To_PID) ->
    To_PID ! #request{from=self(), field=average},
    receive
        #message{function=average, data=Value} ->
            Value;
        _ -> error
    end.

%% Pull back all stored fragments from a node specified by PID
%%
%% Arguments:
%%      To_PID: The PID of the node you wish to query
%%
%% Returns:
%%      The list of fragments. Fragments are defined by the #fragment record. 
%%      The atom "error" if something goes wrong
get_fragments(To_PID) ->
    To_PID ! #request{from=self(), field=fragments},
    receive
        Fragments when is_list(Fragments) ->
            Fragments;
        _ -> error
    end.

%% Store a list of numbers as fragment Number
%%
%% Arguments:
%%      Number: Fragment number
%%      Data: A list of  numbers to store
%%      Nodes: List of nodes in the network
%%
%% Returns:
%%      ok
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

%% Just a shortcut to get the info from node one.
%%
%% Arguments:
%%      Nodes: The list of nodes in the network
%%
%% Returns:
%%      A list of tuples containing min, max, average, median, and the stored
%%      fragments.
query_node_one(Nodes) when is_list(Nodes) ->
    query_node(1, Nodes).

%% Pull back all the info from a given node.
%%
%% The node to query is determined by finding the node at Nodes[Number]
%%
%% Arguments:
%%      Number: The node number to query
%%      Nodes: The list of nodes in the network
%%
%% Returns:
%%      A list of tuples containing min, max, average, median, and the stored
%%      fragments.
query_node(Number, Nodes) ->
    Node = lists:nth(Number, Nodes),
    query_node(Node).

%% Pull back all info from a given node
%%
%% Arguments:
%%      Node: The PID of the node you wish to query
%%
%% Returns:
%%      A list of tuples containing min, max, average, median, and the stored
%%      fragments.
query_node(Node) ->
    D1 = dict:new(),
    D2 = dict:store("Min", get_min(Node), D1),
    D3 = dict:store("Max", get_max(Node), D2),
    D4 = dict:store("Average", get_average(Node), D3),
    D5 = dict:store("Median", get_median(Node), D4),
    D6 = dict:store("Fragments", get_fragments(Node), D5),

    dict:to_list(D6).

%% Given a "file", this will compute the actual min, max, average, and median
%%
%% Arguments:
%%      File: A list of values to crunch
%%
%% Returns:
%%      A list of tuples containing min, max, average, median, and the stored
%%      fragments.
get_file_stats(File) when is_list(File) ->
    Min = lists:min(File),
    Max = lists:max(File),
    Ave = node:average(File),
    Med = node:median(File),
    [{min, Min}, {max, Max}, {average, Ave}, {median, Med}].

%% Makes nodes into  neighbors
%%
%% Arguments:
%%      PID_1: The PID of one of the nodes to make neighbors
%%      PID_2: The PID of one of the nodes to make neighbors
%%
%% Returns:
%%      ok
make_neighbors(PID_1, PID_2) ->
    PID_1 ! #add_neighbor{neighbor=PID_2},
    PID_2 ! #add_neighbor{neighbor=PID_1},
    ok.

%% Makes a node neighbors with all nodes in a list
%% 
%% Arguments:
%%      PID_1: The PID of the node to make a neighbor
%%      List: The list of PIDs to neighbor to PID_1
make_neighbors_multiple(_, []) -> ok;
make_neighbors_multiple(PID_1, [Head | Rest]) ->
    make_neighbors(PID_1, Head),
    make_neighbors_multiple(PID_1, Rest).

%% Assigns neighbors to each node based on a graph.
%%
%% Arguments:
%%      Graph: A digraph such as that returned by generate_graph
%%      List: The list of node PIDs to use
make_neighbors_graph(_, []) -> ok;
make_neighbors_graph(Graph, [Head | Rest]) ->
    Neighbors = generate_graph:get_neighbors(Graph, Head),
    make_neighbors_multiple(Head, Neighbors),
    make_neighbors_graph(Graph, Rest).

%% Read in some bytes from a file
%%
%% Arguments:
%%      Fname: The path to the file
%%      Number: Number of bytes to read in
%%
%% Returns:
%%      The raw data from the file
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

%% Generate a list of fragments. Fragments here mean a list of numbers.
%% The size and contents of each fragment is random.
%%
%% Arguments:
%%      Number: The number of fragments to generate
%%
%% Returns:
%%      A list containing Number fragments
generate_fragments(Number) -> generate_fragments_helper(Number, []).
generate_fragments_helper(0, Fragments) -> Fragments;
generate_fragments_helper(Number, Fragments) ->
    New = generate_fragment(),
    generate_fragments_helper(Number - 1, [New | Fragments]).

%% Generate a single random fragment. Fragment here means a list of numbers.
%%
%% Returns:
%%      A list of random length and random numbers
generate_fragment() ->
    Size = random:uniform(100),
    {B1, B2, B3} = now(),
    random:seed(B1, B2, B3),
    generate_fragment_helper(Size, []).
generate_fragment_helper(0, Fragment) ->
    Fragment;
generate_fragment_helper(Number, Fragment) ->
    Entry = random:uniform(1000),
    generate_fragment_helper(Number-1, [Entry|Fragment]).

%% Create a list of gossip nodes. Each node is given a random fragment.
%%
%% Arguments:
%%      Number: The number of nodes to create
%%
%% Returns:
%%      A list of PIDs for each node created
create_nodes(Number) ->
    Fragments = generate_fragments(Number), 
    create_nodes_helper(Number, [], Fragments).

%% Create a list of gossip nodes. Each node is given one of the fragments from
%% the specified list of Fragments.
%%
%% Arguments:
%%      Number: The number of nodes to create
%%      Fragments: The list of fragments. Generate this with 
%%      generate_fragments/1
%%
%% Returns:
%%      A list of PIDs for each node created
create_nodes(Number, Fragments) ->
    create_nodes_helper(Number, [], Fragments).

create_nodes_helper(0, Nodes, _) ->
    Nodes;
create_nodes_helper(Number, Nodes, [Head | Fragments]) ->
    PID = node:start_node(Head),
    create_nodes_helper(Number-1, [PID|Nodes], Fragments).

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

%% A helper function to create a full network of approximately Size nodes. 
%% This variant takes the atom "fragment" as a second argument and will
%% return a tuple containing the list of nodes generated and the full
%% "file" used for initializing this network.
%%
%% Arguments:
%%      Size: The approximate number of nodes to create
%%      fragment: flag to indicate that you want this version of the function
%%
%% Returns:
%%      A tuple of the form {[PIDs], File}, where File is the combination of
%%      all fragments distributed to the network. File then is suitable to get
%%      the actual values that the gossip should be calculating.
create_network(Size, fragments) ->
    Number = generate_graph:calc_nodes(Size),
    Fragments = generate_fragments(Number),
    Nodes = create_nodes(Number, Fragments),
    Graph = generate_graph:build_graph(Nodes),
    make_neighbors_graph(Graph, Nodes),
    {Nodes, lists:append(Fragments)}.

%% A helper function to create a full network of approximately Size nodes. 
%%
%% Arguments:
%%      Size: The approximate number of nodes to create
%%
%% Returns:
%%      A list of nodes in the network
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

%% Step a list of nodes once
%%
%% Arguments:
%%      List: The list of nodes to step
step([]) -> ok;
step([Head | Rest]) ->
    Head ! step,
    step(Rest).

%% Step a list of nodes Number times
%%
%% Arguments:
%%      Nodes: The list of nodes to step
%%      Number: The number of steps to do
step(_, 0) -> ok;
step(Nodes, Number) ->
    timer:sleep(1),
    step(Nodes),
    step(Nodes, Number - 1).

