-module(monitor).
-export([start/0, execute/0, get_min/1, get_max/1, get_average/1, make_neighbors/2, step/1]).
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

%% Ask any node what its average is
get_average(To_PID) ->
    To_PID ! #request{from=monitor, field=average},
    ok.

make_neighbors(PID_1, PID_2) ->
    PID_1 ! #add_neighbor{neighbor=PID_2},
    PID_2 ! #add_neighbor{neighbor=PID_1},
    ok.

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

