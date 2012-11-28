-module(generate_graph).
% @function calc_nodes 
% @details used to calculate number of nodes that will be used in the network.
% @param ballpark number of desired nodes
% @return number of nodes to be used in the network
-export([calc_nodes/1]).
% @function build_graph
% @details this will build the network 
% @param actual number of nodes in the network, should be the output of calc_nodes
% @return digraph of network
-export([build_graph/1]).

showValues( Values ) ->
        lists:foreach(
                fun(I)-> io:format("~w\n", [I]) end,
                Values
        ).

print_graph(Graph) ->
    lists:foreach( fun(I) ->
            io:format("Node ~w points to ~w \n", [I, digraph:out_neighbours(Graph, I)])
            end, digraph:vertices(Graph)
        ).

list_length([]) ->
    0;  
list_length([First | Rest]) ->
    1 + list_length(Rest).

ceiling(X) ->
    T = erlang:trunc(X),
    case (X - T) of
        Neg when Neg < 0 -> T;
        Pos when Pos > 0 -> T + 1;
        _ -> T
    end. 

calc_optimal_depth(NumNodes) ->
    ceiling((math:log10(NumNodes + 1) / math:log10(2)) - 1)
.

calc_nodes_in_tree(Depth) ->
    erlang:trunc(math:pow(2, Depth + 1) - 1).

calc_nodes_in_level(Level) ->
    ceiling(math:pow(2, Level)).

calc_nodes(NumNodes) ->
    Depth = calc_optimal_depth(trunc(NumNodes/2)),
    %io:format("Depth ~w\n", [Depth]),
    NumNodesOptimal = calc_nodes_in_tree(Depth),
    %io:format("NumNodes in one tree ~w\n", [NumNodesOptimal]),
    NumNodesOptimal * 2.

fill_source_queue(Queue, Offset, MaxCount) ->
    % possible better implementation
    %TempList = list:seq(1 + Offset, MaxCount),
    %queue:from_list(TempList).

    QueueLength = queue:len(Queue),
    %io:format("Length ~w\n", [QueueLength]),
    %io:format("MaxCount ~w\n", [MaxCount]),
    if 
        QueueLength == 0 ->
            Element = 1 + Offset,
            QueueNew = queue:in(Element, Queue),
            %io:format("Inserting ~w\n", [Element]),
            %io:format("MaxCount ~w\n", [MaxCount]),
            fill_source_queue(QueueNew, Offset, MaxCount);
        QueueLength < MaxCount ->
            Element = queue:len(Queue) + 1 + Offset,
            QueueNew = queue:in(Element, Queue),
            %io:format("Inserting ~w\n", [Element]),
            %io:format("MaxCount ~w\n", [MaxCount]),
            fill_source_queue(QueueNew, Offset, MaxCount);
        QueueLength == MaxCount -> Queue
    end.

relink(Graph, Vertex, Vertices) ->
    lists:foreach( fun(Node) ->
                digraph:add_edge(Graph, Node, Vertex)
                end, Vertices).
 
join_bigraphs(Left, Right) ->
    Offset = lists:max(digraph:vertices(Left)),
    Depth = calc_optimal_depth(Offset),
    io:format("Depth is ~w\n", [Depth]),
    % add all entries in right digraph to the left one
    lists:foreach( fun(Node) ->
                digraph:add_vertex(Left, Node) end, digraph:vertices(Right)),
    lists:foreach( fun(Node) ->
                relink(Left, Node, digraph:out_neighbours(Right, Node)) end, digraph:vertices(Right)),

    % link the leaf nodes
    LeftLeafList = queue:to_list(fill_source_queue(queue:new(), trunc(math:pow(2, Depth) - 1), calc_nodes_in_level(Depth))),
    RightLeafList = queue:to_list(fill_source_queue(queue:new(), trunc(math:pow(2, Depth) - 1) + Offset, calc_nodes_in_level(Depth))),
    %io:format("LeftLeafList :\n"),
    %showValues(LeftLeafList),
    %io:format("RightLeafList :\n"),
    %showValues(RightLeafList),
    LeafList = lists:zip(LeftLeafList, RightLeafList),
    %io:format("LeafList:\n"),
    %showValues(LeafList),
    lists:foreach( fun(Nodes) ->
                digraph:add_edge(Left, element(1, Nodes), element(2, Nodes)),
                digraph:add_edge(Left, element(2, Nodes), element(1, Nodes))
                end, LeafList),
    Left.

build_bigraph(Digraph, SourceQueue, WorkQueue, Level, Count, MaxLevel) when Count == -1 ->
    % pop node of source queue 
    % put node in work queue
    {Node, NewSourceQueue} = queue:out(SourceQueue),
    NodeNumber = element(2, Node),
    digraph:add_vertex(Digraph, NodeNumber),
    NewWorkQueue = queue:in(NodeNumber, WorkQueue),
    build_bigraph(Digraph, NewSourceQueue, NewWorkQueue, Level, 1, MaxLevel);
build_bigraph(Digraph, SourceQueue, WorkQueue, Level, Count, MaxLevel) when Level == MaxLevel ->
    % return digraph here
    Digraph;
build_bigraph(Digraph, SourceQueue, WorkQueue, Level, Count, MaxLevel) ->
    % io:format("Current level ~w, current count ~w, max level ~w \n", [Level,Count, MaxLevel]),
    % io:format("Work queue begin ~w \n", [WorkQueue]),
    if 
        Count > 0 ->
            % create root node
            HeadNodeNum = element(2, queue:peek(WorkQueue)),
            HeadNode = digraph:vertex(Digraph, HeadNodeNum),

            % io:format("Head node ~w\n", [HeadNode]),
            % io:format("Work queue before ~w \n", [WorkQueue]),

            % pop node off source queue
            % put node at tail of work queue
            % link node to head of work queue
            {LeftNode, NewSourceQueue1} = queue:out(SourceQueue),
            LeftNodeNum = element(2, LeftNode),
            NewWorkQueue1 = queue:in(LeftNodeNum, WorkQueue),
            digraph:add_vertex(Digraph, LeftNodeNum),
            digraph:add_edge(Digraph, HeadNodeNum, LeftNodeNum),
            digraph:add_edge(Digraph, LeftNodeNum, HeadNodeNum),

            % io:format("Work queue add left ~w \n", [NewWorkQueue1]),

            % pop node off source queue
            % link node to head of work queue
            {RightNode, NewSourceQueue2} = queue:out(NewSourceQueue1),
            RightNodeNum = element(2, RightNode),
            NewWorkQueue2 = queue:in(RightNodeNum, NewWorkQueue1),
            digraph:add_vertex(Digraph, RightNodeNum),
            digraph:add_edge(Digraph, HeadNodeNum, RightNodeNum),
            digraph:add_edge(Digraph, RightNodeNum, HeadNodeNum),

            % io:format("Work queue add right ~w \n", [NewWorkQueue2]),
            % pop node off work queue
            {OldHead, NewWorkQueue3} = queue:out(NewWorkQueue2),

            % io:format("Work queue removed head ~w \n", [NewWorkQueue3]),
            % recursive call with decremented count
            build_bigraph(Digraph, NewSourceQueue2, NewWorkQueue3, Level, Count - 1, MaxLevel);

        Count == 0 ->
            % increase level
            % set count to number of nodes in next level
            NextLevelCount = calc_nodes_in_level(Level + 1),
            build_bigraph(Digraph, SourceQueue, WorkQueue, Level + 1, NextLevelCount, MaxLevel)
    end.
    

build_graph(NumNodes) ->
    Depth = calc_optimal_depth(trunc(NumNodes/2)),
    QueueLeft = fill_source_queue(queue:new(), 0, NumNodes),
    %io:format("Queue ~w\n", [QueueLeft]),
    DigraphLeft = build_bigraph(digraph:new(), QueueLeft, queue:new(), 0, -1, Depth),
    print_graph(DigraphLeft),
    io:format("Final node: ~w\n", [lists:max(digraph:vertices(DigraphLeft))]),
    QueueRight = fill_source_queue(queue:new(), lists:max(digraph:vertices(DigraphLeft)), NumNodes),
    DigraphRight = build_bigraph(digraph:new(), QueueRight, queue:new(), 0, -1, Depth),
    print_graph(DigraphRight),
    io:format("Final node: ~w\n", [lists:max(digraph:vertices(DigraphRight))]),
    FinalDigraph = join_bigraphs(DigraphLeft, DigraphRight),
    print_graph(FinalDigraph),
    FinalDigraph.

