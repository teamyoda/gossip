%% This header should contain all message formats used in inter-node 
%% communication

%% Basic message passing format
-record(message, {from, %% PID of the sender
                  function, %% atom representing the function to send to 
                  data}). %% the secret being passed

%% Message to manage neighbors
-record(add_neighbor, {neighbor}). %% PID of the new neighbor

-record(state, {min, %% Node's current minimum secret
                max, %% Node's current maximum secret
                average}). % Node's current avervage secret
