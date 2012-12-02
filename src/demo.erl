-module(demo).
-export([run_calc/0]).
-export([run_comms/0]).
-export([ceiling/1]).

-include("include/message.hrl").

ceiling(X) when X < 0 ->
        trunc(X);
ceiling(X) ->
    T = trunc(X),
    case X - T == 0 of
        true -> T;
        false -> T + 1
end.

run_calc() ->
    io:format("Creating network~n"),
    {Network, File} = monitor:create_network(10000, fragments),
    io:format("Created network of ~p nodes~n", [length(Network)]),
    io:format("Will now run at most 200 rounds~n"),
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

run_comms() ->
    io:format("Begining comms test~n"),
    Network = monitor:create_network(10000),
    io:format("Created network of ~p nodes~n", [length(Network)]),
    Fragment = [1,2,3,4,5,6],
    Dest = random:uniform(length(Network)),
    io:format("Will now store fragment ~p at node ~p~n", [Fragment, Dest]),

    monitor:store_fragment(Dest, Fragment, Network),
    Rounds = gossip_until_found(Dest, Fragment, Network),
    io:format("It took ~p gossip rounds to store this fragment~n", [Rounds]),

    io:format("Now requesting same fragment back from node ~p~n", [Dest]),
    monitor:request_fragment(Dest, Network),
    ReplyRounds = gossip_until_found(1, Fragment, Network),
    io:format("It took ~p gossip rounds to receive the fragment at node 1~n", [ReplyRounds]).

gossip_until_found(Dest, FragmentData, Network) ->
    Dest_PID = lists:nth(Dest, Network),
    node_data_wait(Dest_PID, FragmentData, Network, 0).
    
node_data_wait(Dest, FragmentData, Network, Rounds) ->
    DestFragments = monitor:get_fragments(Dest),
    HasFragment = lists:keysearch(FragmentData, #fragment.data, DestFragments),

    if
        HasFragment /= false ->
            io:format("Found it!~n"),
            Rounds;
        true ->
            monitor:step(Network),
            node_data_wait(Dest, FragmentData, Network, Rounds+1)
    end.

    
