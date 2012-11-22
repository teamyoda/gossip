-module(node).
-export([execute/2, start_node/3, send_min/2, send_max/2, send_average/2]).
-include("include/message.hrl").

%% Public API
start_node(Min, Max, Average) ->
    spawn(node, execute, [[], #state{min=Min, max=Max, average=Average}]).

%% Main execution loop
execute(Neighbors, State = #state{average=Average, min=Min, max=Max}) ->
    
    receive
        %% Get a neighbor's min and process it
        #message{function=min, data=Data} ->
            New_Min = min(Min, Data),
            execute(Neighbors, State#state{min=New_Min});

        %% Get a neighbor's max and process it
        #message{function=max, data=Data} ->
            New_Max = max(Max, Data),
            execute(Neighbors, State#state{max=New_Max});

        %% Get a neighbor's average and process it
        #message{function=average, data=Data} ->
            New_Average = average([Average, Data]),
            execute(Neighbors, State#state{average=New_Average});

        %% Get a new neighbor's PID and add it to our list
        #add_neighbor{neighbor=Neighbor} ->
            io:format("Adding neighbor~n"),
            New_Neighbors = add_neighbor(Neighbor, Neighbors),
            execute(New_Neighbors, State);

        %% Get a request for our min and send it to the requestor
        #request{from=From, field=min} ->
            send_min(From, State),
            execute(Neighbors, State);

        %% Get a request for our max and send it to the requestor
        #request{from=From, field=max} ->
            send_max(From, State),
            execute(Neighbors, State);

        %% Get a request for our average and send it to the requestor
        #request{from=From, field=average} ->
            send_average(From, State),
            execute(Neighbors, State);

        step ->
            push(Neighbors, State),
            pull(Neighbors),
            execute(Neighbors, State);

        %% Shut down the node
        kill ->
            exit(normal);

        Data ->
            io:format("Got unknown message (~p):~n~p~n", [self(), Data]),
            execute(Neighbors, State)
    end.


%% Find the average of a list
average(List) -> lists:sum(List) / length(List).

push([], _) ->
    ok;

push([To | Neighbors], State) ->
    %% Send data to neighbors
    send_min(To, State),
    send_max(To, State),
    send_average(To, State),

    push(Neighbors, State).

send_min(PID, State) ->
    PID ! #message{function=min, from=self(), data=State#state.min},
    ok.

send_max(PID, State) ->
    PID ! #message{function=max, from=self(), data=State#state.max},
    ok.

send_average(PID, State) ->
    PID ! #message{function=average, from=self(), data=State#state.average},
    ok.

pull([]) ->
    ok;

pull([From | Neighbors]) ->
    %% Get neighbor's values
    request_min(From),
    request_max(From),
    request_average(From),

    pull(Neighbors).

request_min(PID) ->
    PID ! #request{from=self(), field=min},
    ok.

request_max(PID) ->
    PID ! #request{from=self(), field=max},
    ok.

request_average(PID) ->
    PID ! #request{from=self(), field=average},
    ok.

add_neighbor(New_Neighbor, Neighbors) ->
    [New_Neighbor|Neighbors].
