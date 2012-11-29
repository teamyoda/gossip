%% This header should contain all message formats used in inter-node 
%% communication

%% Basic message passing format
-record(message, {from, %% PID of the sender
                  function, %% atom representing the function to send to 
                  data}). %% the secret being passed

%% Message to manage neighbors
-record(add_neighbor, {neighbor}). %% PID of the new neighbor

-record(fragment, {owner, sender, replyto, data, method}).

-record(state, {min=0, %% Node's current minimum secret
                max=0, %% Node's current maximum secret
                average=0, % Node's current average secret
                median=0, % Node's current average secret
                fragment=[], %% Node's current fragment
                fragment_list=[]}). %% List of fragments held by this node

-record(request, {from, field}).

