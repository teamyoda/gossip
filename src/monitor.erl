-module(monitor).
-export([start/0, execute/0, get_min/1, get_max/1, get_average/1, 
        make_neighbors/2, step/1, create_nodes/1, get_median/1, get_fragments/1]).
-include("include/message.hrl").

%% Monitor for checking the status of any gossip node

%% Start the monitor
start() ->
    Monitor = spawn(monitor, execute, []),
    register(monitor, Monitor),
    ok.

%% Ask any node what its min is
get_min(To_PID) ->
    To_PID ! #request{from=monitor, field=min},
    ok.

%% Ask any node what its max is
get_max(To_PID) ->
    To_PID ! #request{from=monitor, field=max},
    ok.

get_median(To_PID) ->
    To_PID ! #request{from=monitor, field=median},
    ok.

%% Ask any node what its average is
get_average(To_PID) ->
    To_PID ! #request{from=monitor, field=average},
    ok.

get_fragments(To_PID) ->
    To_PID ! #request{from=monitor, field=fragments},
    ok.

make_neighbors(PID_1, PID_2) ->
    PID_1 ! #add_neighbor{neighbor=PID_2},
    PID_2 ! #add_neighbor{neighbor=PID_1},
    ok.

generate_fragment() ->
    generate_fragment_helper(20, []).
generate_fragment_helper(0, Fragment) ->
    Fragment;
generate_fragment_helper(Number, Fragment) ->
    Entry = random:uniform(100),
    generate_fragment_helper(Number-1, [Entry|Fragment]).

create_nodes(Number) ->
    create_nodes_helper(Number, []).
create_nodes_helper(0, Nodes) ->
    Nodes;
create_nodes_helper(Number, Nodes) ->
    Fragment = generate_fragment(),
    PID = node:start_node(Fragment),
    create_nodes_helper(Number-1, [PID|Nodes]).

step([]) ->
    ok;

step([Head | Rest]) ->
    Head ! step,
    step(Rest).

execute() ->
    receive
        kill ->
            exit(normal);

        Data ->
            io:format("Received: ~p~n", [Data]),
            execute()
    end.

