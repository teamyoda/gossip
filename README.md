# Gossip

Gossip implementation for CMSC 621

### Layout
We follow a style similar to that shown at 
http://learnyousomeerlang.com/designing-a-concurrent-application#lay-them-foundations

### Building

    $ cd gossip
    $ erl -make

This will output the binaries to the ebin directory

### Simple Test

To use after building run from the project directory

    $ erl -pa ebin/
    1> node:start(). # This will bring up two nodes (time-being)
    2> node:poke(). # This is essentially a clock cycle. It prints the current
                    # status of the calculations and sends the updated values
                    # to all neighbors.

### Using the Monitor

We now have a monitor pseudo-node that allows us to see what a node's status is
and perform general debugging. See ``src/monitor.erl`` for all its exports.

    $ erl -pa ebin/

    1> monitor:start().
    ok
    2> Node_1 = node:start_node(1,2,3).
    <0.36.0>
    3> Node_2 = node:start_node(4,5,6).
    <0.38.0>
    4> monitor:make_neighbors(Node_1, Node_2).
    Adding neighbor
    Adding neighbor
    ok
    5> Network = [Node_1, Node_2].
    [<0.36.0>,<0.38.0>]
    6> monitor:step(Network).
    ok
    7> monitor:get_min(Node_1).               
    Received: {message,<0.36.0>,min,1}
    ok
    8> monitor:get_min(Node_2).
    Received: {message,<0.38.0>,min,1}
    ok
    9> monitor:get_average(Node_2).
    Received: {message,<0.38.0>,average,4.5}
    ok
    10> monitor:get_average(Node_1).
    Received: {message,<0.36.0>,average,4.5}
    ok

### Initializing nodes with fragments
The funtion ``node:start_node/1`` allows you to pass in a list of data that
will be used as a fragment to initialize the node.

    1> Node_1 = node:start_node([4,2,5,3,5,3,5,34,7,21,8,9]).

### Quick network creation
The ``monitor:create_nodes/1`` function allows you to quickly create a list of
nodes. All nodes are initialized with a random fragment, so this is more for
ease of testing than anything.

    1> monitor:start().
    ok
    2> Nodes = monitor:create_nodes(5). # Returns a list of nodes
                                        # Nodes are pre-initialized with some
                                        # random fragments

    3> # Can now set up neighbors, step, and query as above

### Spawning Remote Nodes
Additional setup to your environment is required before you can spawn nodes on a remote system. First create 2 VMs as described in project 1. Add these lines to the /etc/hosts file in both VMs:

    192.168.0.101	vm1
    192.168.0.102	vm2

Make sure to download and build this code on both VMs. The rest of the setup is similar to this tutorial: http://learnyousomeerlang.com/distribunomicon

When starting the erlang shell on each vm you need to add additional tags. Start erlang on vm1 like this:

    erl -pa ebin/ -sname master@vm1 -setcookie sync

and on vm2 like this:

    erl -pa ebin/ -sname slave@vm2 -setcookie sync

Finally, before doing anything else, run this command on vm1 in the erlang shell:

    1> net_kernel:connect(slave@vm2).

If it returns true then you are good to go. Run all commands from within the erlang shell on vm1. Now you can run ``node:start_remote_node/3`` to spawn a node that runs on vm2. Notice that it will have a non-zero value for the first part of it's PID, indicating it is running remotely. You can interact with it exactly as you would a local node.
