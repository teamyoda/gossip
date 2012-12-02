-module(exp1).
-export([run/0]).
-export([ceiling/1]).

ceiling(X) when X < 0 ->
        trunc(X);
ceiling(X) ->
    T = trunc(X),
    case X - T == 0 of
        true -> T;
        false -> T + 1
end.

run() ->
    io:format("Starting monitor~n"),
    {Network, File} = monitor:create_network(10000, fragments),
    io:format("Created network of ~p nodes~n", [length(Network)]),
    io:format("Will now run at most 1000 rounds~n"),
    {ok, FileDescriptor} = file:open("exp1.txt", [write]), 
    looper(FileDescriptor, Network, 0, 200),
    FileInfo = monitor:get_file_stats(File),
    file:close(FileDescriptor),
    io:format("Actual values are:~n~p~n", [FileInfo]).

dump_nodes(Network, FileDescriptor, Rounds) ->
    lists:foreach(
            fun(Node) ->
                Results = monitor:query_node(Node),
                dump_to_file(FileDescriptor, Rounds, Node, Results) end,
                Network
            ).

dump_to_file(FileDescriptor, Rounds, Node, Results) ->
    [Min|Rest] = Results,
    [Average|Rest2] = Rest,
    [Fragments|Rest3] = Rest2,
    [Median|Rest4] = Rest3,
    [Max|Rest5] = Rest4,
    io:format(FileDescriptor, "~p, ~p, ~p, ~p, ~p, ~p\n", [Rounds, Node, element(2, Min), element(2, Average), element(2, Median), element(2, Max)]),
    ok.

looper(_, _, Rounds, MaxRounds) when Rounds > MaxRounds -> ok;
looper(FileDescriptor, Network, Rounds, MaxRounds) ->
    monitor:step(Network),
    dump_nodes(Network, FileDescriptor, Rounds),
    timer:sleep(1),
    looper(FileDescriptor, Network, Rounds + 1, MaxRounds).
