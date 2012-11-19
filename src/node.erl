-module(node).
-export([start/0, poke/0, execute/2, start_node/3]).
-include("include/message.hrl").

%% Public API
start_node(Min, Max, Average) ->
    spawn(node, execute, [[], #state{min=Min, max=Max, average=Average}]).

%% Main execution loop
execute(Neighbors, State = #state{average=Average, min=Min, max=Max}) ->
    
    receive
        #message{function=min, data=Data} ->
            New_Min = min(Min, Data),
            execute(Neighbors, State#state{min=New_Min});

        #message{function=max, data=Data} ->
            New_Max = max(Max, Data),
            execute(Neighbors, State#state{max=New_Max});

        #message{function=average, data=Data} ->
            New_Average = average([Average, Data]),
            execute(Neighbors, State#state{average=New_Average});

        #message{from=From, data=Data} ->
            io:format("Got message~n"),
            process(From, Data),
            execute(Neighbors, State);

        #add_neighbor{neighbor=Neighbor} ->
            io:format("Adding neighbor~n"),
            New_Neighbors = add_neighbor(Neighbor, Neighbors),
            execute(New_Neighbors, State);

        #request{from=From, field=min} ->
            From ! State#state.min,
            execute(Neighbors, State);

        #request{from=From, field=max} ->
            From ! State#state.max,
            execute(Neighbors, State);

        #request{from=From, field=average} ->
            From ! State#state.average,
            execute(Neighbors, State);

        current_state ->
            io:format("State from ~p~nMin: ~p~nMax: ~p~nAve: ~p~n", 
                [self(), Min, Max, Average]),

            send(Neighbors, State),
            execute(Neighbors, State);

        kill ->
            exit(normal)
    end.

%% Find the average of a list
average(List) -> lists:sum(List) / length(List).

process(From, Data) ->
    io:format("From ~p To ~p :: Secret is: ~p~n", [From, self(), Data]),
    ok.

send([], _) ->
    ok;

send([To | Neighbors], State) ->
    To ! #message{function=average, from=self(), data=State#state.average},
    To ! #message{function=min, from=self(), data=State#state.min},
    To ! #message{function=max, from=self(), data=State#state.max},
    send(Neighbors, State).

add_neighbor(New_Neighbor, Neighbors) ->
    [New_Neighbor|Neighbors].

%% Test code
poke() ->
    second ! current_state,
    first ! current_state.

start() -> 
    io:format("Starting~n"),
    First_PID = spawn(node, execute, [[], #state{min=5,max=5,average=5}]),
    Second_PID = spawn(node, execute, [[First_PID], #state{min=2, max=2, average=2}]),
    register(first, First_PID),
    register(second, Second_PID),

    first ! #add_neighbor{neighbor=Second_PID}.
    %%First_PID ! #message{from=Second_PID, function=min, data=5}.
