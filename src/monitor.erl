-module(monitor).
-export([start/0, get_min/1, get_max/1, get_average/1, 
        make_neighbors/2, make_neighbors_multiple/2, make_neighbors_graph/2, 
        step/1, create_nodes/1, create_nodes_remote/1,
        get_median/1, get_fragments/1, 
        store_fragment/3, request_fragment/2, query_node_one/1, step/2,
        create_network/1, create_network_remote/1]).

-export([read_fragment_file/2]).
-export([generate_fragments/1]).
-export([create_network/2]).
-export([get_file_stats/1]).
-include("include/message.hrl").

%% Monitor for checking the status of any gossip node

start() ->
    ok.

%% Ask any node what its min is
get_min(To_PID) ->
    To_PID ! #request{from=self(), field=min},
    receive
        #message{function=min, data=Value} ->
            Value;
        _ -> error
    end.

%% Ask any node what its max is
get_max(To_PID) ->
    To_PID ! #request{from=self(), field=max},
    receive
        #message{function=max, data=Value} ->
            Value;
        _ -> error
    end.

get_median(To_PID) ->
    To_PID ! #request{from=self(), field=median},
    receive
        #message{function=median, data=Value} ->
            Value;
        _ -> error
    end.

%% Ask any node what its average is
get_average(To_PID) ->
    To_PID ! #request{from=self(), field=average},
    receive
        #message{function=average, data=Value} ->
            Value;
        _ -> error
    end.

get_fragments(To_PID) ->
    To_PID ! #request{from=self(), field=fragments},
    receive
        Fragments when is_list(Fragments) ->
            Fragments;
        _ -> error
    end.

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

    D1 = dict:new(),
    D2 = dict:store("Min", get_min(Node_1), D1),
    D3 = dict:store("Max", get_max(Node_1), D2),
    D4 = dict:store("Average", get_average(Node_1), D3),
    D5 = dict:store("Median", get_median(Node_1), D4),
    D6 = dict:store("Fragments", get_fragments(Node_1), D5),

    dict:to_list(D6).

get_file_stats(File) when is_list(File) ->
    Min = lists:min(File),
    Max = lists:max(File),
    Ave = node:average(File),
    Med = node:median(File),
    [{min, Min}, {max, Max}, {average, Ave}, {median, Med}].

%% Makes nodes into  neighbors
make_neighbors(PID_1, PID_2) ->
    PID_1 ! #add_neighbor{neighbor=PID_2},
    PID_2 ! #add_neighbor{neighbor=PID_1},
    ok.

%% Makes a node neighbors with all nodes in a list
make_neighbors_multiple(_, []) ->
    ok;

make_neighbors_multiple(PID_1, [Head | Rest]) ->
    make_neighbors(PID_1, Head),
    make_neighbors_multiple(PID_1, Rest).

%% Assigns neighbors to each node based on a graph. Takes in the graph and the node list.
make_neighbors_graph(_, []) ->
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

generate_fragments(Number) ->
    generate_fragments_helper(Number, []).

generate_fragments_helper(0, Fragments) -> Fragments;
generate_fragments_helper(Number, Fragments) ->
    New = generate_fragment(),
    generate_fragments_helper(Number - 1, [New | Fragments]).

generate_fragment() ->
    Size = random:uniform(100),
    generate_fragment_helper(Size, []).
generate_fragment_helper(0, Fragment) ->
    Fragment;
generate_fragment_helper(Number, Fragment) ->
    Entry = random:uniform(100),
    generate_fragment_helper(Number-1, [Entry|Fragment]).

create_nodes(Number) ->
    Fragments = generate_fragments(Number), 
    create_nodes_helper(Number, [], Fragments).

create_nodes(Number, Fragments) ->
    create_nodes_helper(Number, [], Fragments).

create_nodes_helper(0, Nodes, _) ->
    Nodes;
create_nodes_helper(Number, Nodes, [Head | Fragments]) ->
    PID = node:start_node(Head),
    create_nodes_helper(Number-1, [PID|Nodes], Fragments).

create_network(Size, fragments) ->
    Number = generate_graph:calc_nodes(Size),
    Fragments = generate_fragments(Number),
    Nodes = create_nodes(Number, Fragments),
    Graph = generate_graph:build_graph(Nodes),
    make_neighbors_graph(Graph, Nodes),
    {Nodes, lists:append(Fragments)}.

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

