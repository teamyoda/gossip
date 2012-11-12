% @author Stanislav Bobovych
% @file d_ex.erl
% @details This is an example of using a standard Erlang package
% called digraph to create directed graphs.
% @notes
% http://www.erlang.org/doc/man/digraph.html 
% http://contrib.basho.com/digraph_importer.html
% http://easyerl.blogspot.com/2007/11/digraph-and-your-network-too-easy.html
% http://www.techrepublic.com/article/modelling-graphs-with-processes-in-erlang/6310733 
%
-module(digraph_example).
-export([run/0]).
%-on_load(d_ex_run/0). % this does not work, it's an experimental feature
list_length([]) ->
	0;
list_length([First | Rest]) ->
	1 + list_length(Rest).


run() ->
	D = digraph:new(), % create a cyclic graph, acyclic won't allow us to simulate undirected graphs
	digraph:add_vertex(D, "1"),
	digraph:add_vertex(D, "2"),
	digraph:add_vertex(D, "3"),
	Vertices = digraph:vertices(D),
	io:format("Vertices: ~p ~n", [Vertices]), % print vertices
	% since it's a directed graph, need vertices to point at each other
	digraph:add_edge(D, "1", "2"), % 1 -> 2
	digraph:add_edge(D, "2", "1"), % 2 -> 1
	digraph:add_edge(D, "1", "3"), % 1 -> 3
	digraph:add_edge(D, "3", "1"), % 3 -> 1

	Edges = digraph:edges(D, "1"),
	io:format("Number of edges eminating from and incident on \"1\": ~w ~n", [list_length(Edges)]), % print edges
	io:format("Edges eminating from and incident on \"1\" : ~p ~n", [Edges]), % print edges
	InDegree1 = digraph:in_degree(D, "1"),
	io:format("In-Degree of \"1\": ~p ~n", [InDegree1]),
	OutDegree1 = digraph:in_degree(D, "1"),
	io:format("Out-Degree of \"1\": ~p ~n", [OutDegree1]),
	digraph:delete(D). % explicit delete is needed
% end run

	
