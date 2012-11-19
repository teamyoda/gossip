-module(monitor).
-export([start/0, execute/0, get_min/1, get_max/1, get_average/1]).
-include("include/message.hrl").

%% Monitor for checking the status of any gossip node

%% Start the monitor
start() ->
    Monitor = spawn(monitor, execute, []),
    register(monitor, Monitor),
    ok.

%% Ask any node what its min is
get_min(To_PID) ->
    To_PID ! #request{from=monitor, field=min}.

%% Ask any node what its max is
get_max(To_PID) ->
    To_PID ! #request{from=monitor, field=max}.

%% Ask any node what its average is
get_average(To_PID) ->
    To_PID ! #request{from=monitor, field=average}.

execute() ->
    receive
        kill ->
            exit(normal);

        Data ->
            io:format("Received: ~p~n", [Data]),
            execute()
    end.

