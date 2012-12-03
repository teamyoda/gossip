-module(exp1).
-export([run_experiment/0]).
-export([run/3]).
-export([ceiling/1]).

ceiling(X) when X < 0 ->
        trunc(X);
ceiling(X) ->
    T = trunc(X),
    case X - T == 0 of
        true -> T;
        false -> T + 1
end.

calc_error(Value, EstValue) ->
    %io:format("~p ~p \n", [Value, EstValue]),
    100*(Value - EstValue) / Value.

run(NumNodes, NumRounds, FileName) ->
    io:format("Starting monitor~n"),
    {Network, File} = monitor:create_network(NumNodes, fragments),
    io:format("Created network of ~p nodes~n", [length(Network)]),
    io:format("Will now run at most ~p rounds~n", [NumRounds]),
    {ok, FileDescriptor} = file:open(FileName, [write]), 
    FileInfo = monitor:get_file_stats(File),
    io:format("Actual values are:~n~p~n", [FileInfo]),
    io:format("Rounds, Node, Min, Average, Median, Max, MinError, AvgError, MedError, MaxError\n"),
    looper(FileDescriptor, Network, FileInfo, 0, NumRounds),
    file:close(FileDescriptor),
    % record actual numbers to file
    {ok, FD} = file:open("Exp1.log", [append]),
    io:format(FD, "~p,~w~n", [FileName, FileInfo]),
    file:close(FD).

dump_nodes(FileDescriptor, Network, FileInfo, Rounds) ->
    lists:foreach(
            fun(Node) ->
                Results = monitor:query_node(Node),
                dump_to_file(FileDescriptor, Rounds, FileInfo, Node, Results) end,
                Network
            ).

dump_to_file(FileDescriptor, Rounds, FileInfo, Node, Results) ->
    [Min|Rest] = Results,
    [Average|Rest2] = Rest,
    [Fragments|Rest3] = Rest2,
    [Median|Rest4] = Rest3,
    [Max|Rest5] = Rest4,
    MinError = calc_error(element(2,Min), element(2, lists:nth(1, FileInfo))),
    MaxError = calc_error(element(2,Max), element(2, lists:nth(2, FileInfo))),
    AvgError = calc_error(element(2,Average), element(2, lists:nth(3, FileInfo))),
    MedianError = calc_error(element(2, Median), element(2, lists:nth(4, FileInfo))),
    io:format(FileDescriptor, "~p, ~p, ~p, ~p, ~p, ~p, ~p, ~p, ~p, ~p\n", [Rounds, Node, element(2, Min), element(2, Average), element(2, Median), element(2, Max),
                                                                                            MinError, AvgError, MedianError, MaxError]),
    ok.

looper(_, _, _, Rounds, MaxRounds) when Rounds > MaxRounds -> ok;
looper(FileDescriptor, Network, FileInfo, Rounds, MaxRounds) ->
    monitor:step(Network),
    case Rounds rem 50 == 0 of
        true ->
            dump_nodes(FileDescriptor, Network, FileInfo, Rounds);
        false-> false
    end,
    timer:sleep(1),
    looper(FileDescriptor, Network, FileInfo, Rounds + 1, MaxRounds).

run_experiment() ->
    random:seed(erlang:now()),
    Nodes = 10000,
    Rounds = 200,
    run(Nodes, Rounds, "10k200_a.csv").
