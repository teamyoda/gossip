-module(generate_graph).
-export([calc_optimal_depth/1]).
-export([calc_nodes_in_tree/1]).
-export([calc_nodes_in_level/1]).
-export([build_graph/1]).
-export([generate_graph/1]).

showValues( Values ) ->
        lists:foreach(
                fun(I)-> io:format("~w\n", [I]) end,
                Values
        ).

add_node(0, T) -> T;
add_node(N, T) when N > 0 -> add_node(N-1, gb_trees:enter(N, N, T)).

iterate_tree(It) ->
    Item = gb_trees:next(It),
    if Item /= none ->
        {Key, Value, It1} = Item,
        %io:format("Item ~w\n", [Key]),
        iterate_tree(It1);
    true -> ok
    end
.
 
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

fill_source_queue(Queue, MaxCount) ->
    QueueLength = queue:len(Queue),
    %io:format("Length ~w\n", [QueueLength]),
    %io:format("MaxCount ~w\n", [MaxCount]),
    if 
        QueueLength == 0 ->
            Element = 1,
            QueueNew = queue:in(Element, Queue),
            %io:format("Inserting ~w\n", [Element]),
            %io:format("MaxCount ~w\n", [MaxCount]),
            fill_source_queue(QueueNew, MaxCount);
        QueueLength < MaxCount ->
            Element = queue:len(Queue) + 1,
            QueueNew = queue:in(Element, Queue),
            %io:format("Inserting ~w\n", [Element]),
            %io:format("MaxCount ~w\n", [MaxCount]),
            fill_source_queue(QueueNew, MaxCount);
        QueueLength == MaxCount -> Queue
    end.

build(Digraph, SourceQueue, WorkQueue, Level, Count, MaxLevel) when Count == -1 ->
    %io:format("first case \n"),
    % pop node of source queue 
    % make fist node
    build(Digraph, SourceQueue, WorkQueue, Level, 1, MaxLevel);
build(Digraph, SourceQueue, WorkQueue, Level, Count, MaxLevel) when Level == MaxLevel ->
    % return digraph here
    io:format("terminal case \n"),
    Digraph;
build(Digraph, SourceQueue, WorkQueue, Level, Count, MaxLevel) ->
    %io:format("Current level ~w, current count ~w, max level ~w \n", [Level,Count, MaxLevel]),
    % create root node
    %
    if 
        Count > 0 ->
            % pop node off source queue
            % link node to head of work queue
            % pop node off source queue
            % link node to head of work queue

            % pop node off work queue
            % recursive call with decremented count
            build(Digraph, SourceQueue, WorkQueue, Level, Count - 1, MaxLevel);
        Count == 0 ->
            % increase level
            % set count to number of nodes in next level
            NextLevel = calc_nodes_in_level(Level + 1),
            build(Digraph, SourceQueue, WorkQueue, Level + 1, NextLevel, MaxLevel)
    end.
    

build_graph(NumNodes) ->
    Depth = calc_optimal_depth(NumNodes),
    Digraph = digraph:new(),
    Queue = queue:new(),
    %io:format("Depth ~w\n", [Depth]),
    NumNodesOptimal = calc_nodes_in_tree(Depth),
    %io:format("NumNodes ~w\n", [NumNodesOptimal]),
    QueueNew = fill_source_queue(Queue, NumNodesOptimal),
    %io:format("Queue ~w\n", [QueueNew]),
    FinalDigraph = build(Digraph, QueueNew, Queue, 0, -1, Depth),
    io:format("Final graph ~w\n", [FinalDigraph]),
   ok. 

print_tree(Tree) ->
    Values = gb_trees:values(Tree),
    showValues(Values),
    It = gb_trees:iterator(Tree),
    iterate_tree(It)
.

generate_graph(NumNodesRequested) ->
    Tree = gb_trees:empty(),
    NumNodes = trunc(math:pow(2,calc_optimal_depth(NumNodesRequested)+1) - 1),
    io:format("Number of nodes ~w\n", [NumNodes]),
    FinalTree = add_node(NumNodes, Tree),
    print_tree(FinalTree),
    gb_trees:to_list(FinalTree).
    %Values = gb_trees:values(FinalTree),
    %showValues(Values),
    %It = gb_trees:iterator(FinalTree),
    %iterate_tree(It).

