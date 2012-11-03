-module(node).
-include("include/message.hrl").
-export([start/0, execute/2]).


%% Main execution loop
execute(Neighbors, Secret) ->
    io:format("Executing~n"),

    send(Neighbors, Secret),
    receive
        #message{from=From, data=Data} ->
            io:format("Got message~n"),
            New_Secret = process(From, Data),
            execute(Neighbors, New_Secret);

        {add, Neighbor} ->
            io:format("Adding neighbor~n"),
            New_Neighbors = add_neighbor(Neighbor, Neighbors),
            execute(New_Neighbors, Secret);

        kill ->
            exit(normal)
    end,

    execute(Neighbors, Secret).

process(From, Data) ->
    io:format("~p: Secret is: ~p~n", [self(), Data]),
    Data.

send([], Secret) ->
    ok;

send([To | Neighbors], Secret) ->
    To ! {message, self(), To, Secret},
    send(Neighbors, Secret).

add_neighbor(New_Neighbor, Neighbors) ->
    [New_Neighbor|Neighbors].

start() -> 
    io:format("Starting~n"),
    First_PID = spawn(node, execute, [[], 3]),
    Second_PID = spawn(node, execute, [[First_PID], 2]).
    %%First_PID ! {add, Second_PID}.
