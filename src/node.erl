-module(node).
-export([execute/2, 
        start_node/3, 
        start_remote_node/3, 
        start_node/1, 
        send_min/2, 
        send_max/2, 
        send_average/2, 
        median/1, 
        send_fragment/2, 
        store_fragment/2, 
        process_fragment/2]).

-include("include/message.hrl").

%% Public API

%% Start a new node with a fragment
start_node(Fragment) when is_list(Fragment) ->
    Node = spawn(node, execute, [[], #state{}]),
    Node ! #fragment{owner=Node, data=Fragment, method=store},
    Node.

%% Start a new node with a min, max, and average
start_node(Min, Max, Average) ->
    spawn(node, execute, [[], #state{min=Min, max=Max, average=Average}]).

%% Start a new node on the remote system
start_remote_node(Min, Max, Average) ->
    spawn(slave@vm2, node, execute, [[], #state{min=Min, max=Max, average=Average}]).

%% Main execution loop
execute(Neighbors, State) ->
    
    Average = State#state.average,
    Min = State#state.min,
    Max = State#state.max,
    Median = State#state.median,
    Fragment = State#state.fragment,
    FragmentList = State#state.fragment_list,

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

        #request{from=From, field=fragments} ->
            From ! FragmentList,
            execute(Neighbors, State);

        %% If we have a copy of the fragment, send it back. Otherwise, forward
        %% the request to all neighbors except the sender.
        Frag = #fragment{owner=Owner, sender=Sender, replyto=FinalDest, method=request} ->
            Entry = lists:keyfind(Owner, #fragment.owner, FragmentList),
            if 
                Entry /= false ->
                    send_fragment([Sender], Entry#fragment{replyto=FinalDest, method=reply});
                true ->
                    send_fragment(Neighbors -- [Sender], Frag)
            end,
            execute(Neighbors, State);

        %% If we get a fragment to store, check if it belongs to us and
        %% process it if so. If we hold a copy of it, update our copy and
        %% forward to our neighbors. If it does not belong to us and we
        %% don't hold a copy of it, just forward the fragment to our neighbors.
        Frag = #fragment{owner=Owner, sender=Sender, method=store} ->
            IsNeighbors = lists:member(Owner, Neighbors),
            NewState = 
            if 
                Owner == self() ->
                    %% We own this fragment
                    TempState = process_fragment(Frag, State),
                    replicate_fragment(Neighbors, Frag),
                    store_fragment(Frag, TempState);
                IsNeighbors ->
                    %% We store a copy of this fragment
                    send_fragment(Neighbors -- [Sender], Frag),
                    store_fragment(Frag, State);
                true ->
                    %% We just need to pass the fragment on
                    send_fragment(Neighbors -- [Sender], Frag),
                    State
            end,
            execute(Neighbors, NewState);

        %% If we get a fragment with the method set to reply, then it is in
        %% reply to a request we issued. Store it locally.
        Frag = #fragment{sender=Sender, replyto=FinalDest, method=reply} ->
            NewState = 
            if
                FinalDest == self() ->
                    store_fragment(Frag, State);
                true ->
                    send_fragment(Neighbors -- [Sender], Frag),
                    State
            end,
            execute(Neighbors, NewState);

        %% If we get a fragment marked "replicate", just store it
        Frag = #fragment{method=replicate} ->
            NewState = store_fragment(Frag, State),
            execute(Neighbors, NewState);

        %% Get a new neighbor's PID and add it to our list
        %% Replicate our fragment to the new neighbor
        #add_neighbor{neighbor=NewNeighbor} ->
            NewNeighbors = add_neighbor(NewNeighbor, Neighbors),
            replicate_fragment(NewNeighbors, Fragment),
            execute(NewNeighbors, State);

        
        step ->
            push(Neighbors, State),
            pull(Neighbors),
            execute(Neighbors, State);

        %% Shut down the node
        kill ->
            exit(normal);

        %% Catchall to just print any wierd messages and continue
        Unknown ->
            io:format("Got unknown message (~p):~n~p~n", [self(), Unknown]),
            execute(Neighbors, State)
    end.

%% Process a fragment and return a new state
process_fragment(Fragment, State) when is_record(Fragment, fragment) and is_record(State, state) ->
    Data = Fragment#fragment.data,

    Average = average(Data),
    Min = lists:min(Data),
    Max = lists:max(Data),
    Median = median(Data),

    State#state{min=Min, max=Max, average=Average, median=Median, fragment=Fragment}.

store_fragment(Fragment, State) when is_record(Fragment, fragment) and is_record(State, state) ->
    FragmentList = lists:keystore(Fragment#fragment.owner, 
                                  #fragment.owner, 
                                  State#state.fragment_list, 
                                  Fragment),

    State#state{fragment_list=FragmentList}.

send_fragment(Neighbors, Fragment) ->
    To = pick_dest(Neighbors),
    To ! Fragment#fragment{sender=self()},
    ok.

replicate_fragment([], _) -> ok;
replicate_fragment([To | Rest], Fragment) ->
    To ! Fragment#fragment{owner=self(), sender=self(), method=replicate},
    replicate_fragment(Rest, Fragment).

%% Find the median of a list
median(List) ->
    Sorted = lists:sort(List),
    if 
        length(Sorted) rem 2 == 1 -> 
            lists:nth((length(Sorted) div 2) + 1, Sorted);
        true ->
            First = lists:nth((length(Sorted) div 2), Sorted),
            Second = lists:nth((length(Sorted) div 2) + 1, Sorted),
            (First + Second)/2
    end.

%% Find the average of a list
average(List) -> lists:sum(List) / length(List).

push(Neighbors, State) ->
    %% Send data to neighbors
    To = pick_dest(Neighbors),
    send_min(To, State),
    send_max(To, State),
    send_average(To, State),
    send_median(To, State),
    ok.

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

pull(Neighbors) ->
    %% Get neighbor's values
    From = pick_dest(Neighbors),
    request_min(From),
    request_max(From),
    request_average(From),
    request_median(From),
    ok.

request_min(PID) ->
    PID ! #request{from=self(), field=min},
    ok.

request_max(PID) ->
    PID ! #request{from=self(), field=max},
    ok.

request_average(PID) ->
    PID ! #request{from=self(), field=average},
    ok.

request_median(PID) ->
    PID ! #request{from=self(), field=median},
    ok.

add_neighbor(New_Neighbor, Neighbors) ->
    [New_Neighbor|Neighbors].

%% Gossip to yourself with probability 1/2
%% Gossip to any neighbor with probability 1/(# Neighbors * 2)
pick_dest(Neighbors) ->
    Length = length(Neighbors),
    Choice = 
    if
        Length > 0 ->
            random:uniform(Length * 2);
        true ->
            0
    end,

    Dest = 
    if 
        Choice == 0 ->
            self();
        Choice > Length ->
            self();
        true ->
            lists:nth(Choice, Neighbors)
    end,
    Dest.
