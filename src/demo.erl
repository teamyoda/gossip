-module(demo).
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
    looper(Network, 0, 200),
    FileInfo = monitor:get_file_stats(File),
    io:format("Actual values are:~n~p~n", [FileInfo]).

looper(_, Rounds, MaxRounds) when Rounds > MaxRounds -> ok;
looper(Network, Rounds, MaxRounds) ->
    monitor:step(Network),
    Results = monitor:query_node_one(Network),
    io:format("Results from node 1 after ~p rounds: ~p~n", [Rounds, Results]),
    timer:sleep(1),
    looper(Network, Rounds + 1, MaxRounds).
