# Gossip

Gossip implementation for CMSC 621

# Building
We follow a style similar to that shown at 
http://learnyousomeerlang.com/designing-a-concurrent-application#lay-them-foundations

### To Build:

    $ cd gossip
    $ erl -make

This will output the binary to the ebin directory

To use the binary after building run from the project directory

    $ erl -pa ebin/
    $ node:start(). # This will bring up two nodes (time-being)
    $ node:poke(). # This is essentially a clock cycle. It prints the current
                   # status of the calculations and sends the updated values
                   # to all neighbors.
