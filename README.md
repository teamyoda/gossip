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
    
    $ erl -pa ebin

    1> monitor:start(). # Don't need this any more
    <0.36.0>
    ok
    2> Node = node:start_node([1,2,3,4,5,6,7]).
    <0.36.0>
    3> monitor:step(Node).
    ok
    4> monitor:get_min(Node).
    Received: {message,<0.36.0>,min,1}
    ok

### Using the Monitor
We now have a monitor pseudo-node that allows us to see what a node's status is
and perform general debugging. See ``src/monitor.erl`` for all its exports.

**NOTE:** using ``node:start_node/3`` is deprecated. Switch instead to using 
``node:start_node/1``. This takes in a fragment as a list of numbers.

    $ erl -pa ebin/

    1> monitor:start(). # Don't need this any more
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

    1> monitor:start(). # Don't need this any more
    ok
    2> Number = generate_graph:calc_nodes(10). # Get the actual number of
                                               # nodes you will need to create
    14
    3> Nodes = monitor:create_nodes(Number). # Returns a list of nodes
                                             # Nodes are pre-initialized with 
                                             # some random fragments

    # Can now set up graph and neighbors
    4> Graph = generate_graph:build_graph(Nodes). # Returns a digraph of the nodes

    # Assign neighbors to all nodes based on the graph
    5> monitor:make_neighbors_graph(Graph,Nodes). 
    ok
    # Can now step through network and monitor nodes attributes

### Spawning Remote Nodes
Additional setup to your environment is required before you can spawn nodes 
on a remote system. First create 2 VMs as described in project 1. Add these 
lines to the /etc/hosts file in both VMs:

    192.168.0.101	vm1
    192.168.0.102	vm2

Make sure to download and build this code on both VMs. The rest of the setup 
is similar to this tutorial: http://learnyousomeerlang.com/distribunomicon

When starting the erlang shell on each vm you need to add additional tags. 
Start erlang on vm1 like this:

    erl -pa ebin/ -sname master@vm1 -setcookie sync

and on vm2 like this:

    erl -pa ebin/ -sname slave@vm2 -setcookie sync

Finally, before doing anything else, run this command on vm1 in the erlang shell:

    1> net_kernel:connect(slave@vm2).

If it returns true then you are good to go. Run all commands from within the 
erlang shell on vm1. Now you can run ``node:start_remote_node/3`` to spawn a 
node that runs on vm2. Notice that it will have a non-zero value for the first 
part of it's PID, indicating it is running remotely. You can interact with it 
exactly as you would a local node.

### REALLY quick network creation
There now exist two helper functions to really speed up network creation and 
testing. These are ``monitor:create_network/1`` and ``monitor:step/2``. 

    Eshell V5.8.5  (abort with ^G)
    1> Nodes = monitor:create_network(10). # Create a network with ~10 nodes
    [<0.47.0>,<0.46.0>,<0.45.0>,<0.44.0>,<0.43.0>,<0.42.0>,
    <0.41.0>,<0.40.0>,<0.39.0>,<0.38.0>,<0.37.0>,<0.36.0>,
    <0.35.0>,<0.34.0>]
    2> monitor:start(). # Start the monitor, don't need this any more
    ok
    3> monitor:step(Nodes, 100). # Have the nodes do 100 gossip rounds
    ok

### Cool helper functions
* ``monitor:get_min/1`` Pass it a node PID and it will return the node's min
* ``monitor:get_max/1`` Pass it a node PID and it will return the node's max
* ``monitor:get_average/1`` Pass it a node PID and it will return the node's 
    average
* ``monitor:get_median/1`` Pass it a node PID and it will return the node's 
    median
* ``monitor:get_fragments/1`` Pass it a node PID and it will return the all 
    fragments stored by the node
* ``monitor:create_network/1`` Pass it a number and it will create a network
    with at least that many nodes. It will pass back a list of the nodes.
* ``monitor:create_network/2`` Similar to ``create_network/1`` but it allows
    you to also pass the atom ``fragments``. This will pass back the network
    as above, but will also give you the file used to derive the fragments
    for the nodes. Common usage then is like this:
    ```
    1> {Network, File} = monitor:create_network(100, fragments).
    ...
    ```

* ``monitor:get_file_stats/1`` Pass this the ``File`` from above and get back
    the actual min, max, average, and median. Use this for comparing to the 
    gossiped values.

##Demo
After building just run the demo like this

    1> demo:run().

This will create a network of 10,000+ nodes and run 200 gossip rounds. It
then prints the actual values so you can compare them to the gossip values.
