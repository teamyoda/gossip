-module(node).
-export([execute/2, start_node/3, start_node/1, send_min/2, send_max/2, send_average/2, median/1, forward_fragment/2]).
-include("include/message.hrl").

%% Public API

%% Start a new node with a fragment
start_node(Fragment) when is_list(Fragment) ->
    State = process_fragment(Fragment),
    io:format("starting node with ~p~n", [State]),
    spawn(node, execute, [[], State]).

%% Start a new node with a min, max, and average
start_node(Min, Max, Average) ->
    spawn(node, execute, [[], #state{min=Min, max=Max, average=Average}]).

%% Main execution loop
execute(Neighbors, State) ->
    
    Average = State#state.average,
    Min = State#state.min,
    Max = State#state.max,
    Median = State#state.median,
    Fragment = State#state.fragment,

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

        %% Get a neighbor's median and process it
        #message{function=median, data=Data} ->
            New_Median = median([Median, Data]),
            execute(Neighbors, State#state{median=New_Median});

        %% Update a fragment or pass it on
        #message{function=fragment, from=From, data=Data} ->
            New_State = 
                if Data#fragment.owner == self() ->
                    %% Update node state using the new fragment
                    process_fragment(Fragment);                  
                true ->
                    %% Forward the fragment to all neighbors except the sender
                    forward_fragment(Neighbors -- [From], Fragment),
                    State
                end,
            execute(Neighbors, New_State);

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

        %% Get a request for our median and send it to the requestor
        #request{from=From, field=median} ->
            send_median(From, State),
            execute(Neighbors, State);


        %% Get a request for our fragment and send it to the requestor
        #request{from=From, field=fragment} ->
            send_fragment(From, State),
            execute(Neighbors, State);

        %% Step the node
        step ->
            push(Neighbors, State),
            pull(Neighbors),
            execute(Neighbors, State);

        %% Shut down the node
        kill ->
            exit(normal);

        %% Catchall to just print any wierd messages and continue
        Data ->
            io:format("Got unknown message (~p):~n~p~n", [self(), Data]),
            execute(Neighbors, State)
    end.

%% Process a fragment and return a new state
process_fragment(Fragment) ->
    Average = average(Fragment),
    Min = lists:min(Fragment),
    Max = lists:max(Fragment),
    Median = median(Fragment),
    #state{min=Min, max=Max, average=Average, median=Median, fragment=Fragment}.

%% Find the median of a list
median(List) ->
    Sorted = lists:sort(List),
    if length(Sorted) rem 2 == 1 -> 
        lists:nth((length(Sorted) div 2) + 1, Sorted);
    true ->
        First = lists:nth((length(Sorted) div 2), Sorted),
        Second = lists:nth((length(Sorted) div 2) + 1, Sorted),
        (First + Second)/2
    end.

%% Find the average of a list
average(List) -> lists:sum(List) / length(List).

forward_fragment([], _) -> ok;

forward_fragment([To | Neighbors], Fragment) ->
    To ! #message{function=fragment, from=self(), data=Fragment},
    forward_fragment(Neighbors, Fragment).

push([], _) ->
    ok;

push([To | Neighbors], State) ->
    %% Send data to neighbors
    send_min(To, State),
    send_max(To, State),
    send_average(To, State),
    send_median(To, State),

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

send_median(PID, State) ->
    PID ! #message{function=median, from=self(), data=State#state.median},
    ok.

send_fragment(PID, State) ->
    PID ! #message{function=fragment, from=self(), data=State#state.fragment},
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
