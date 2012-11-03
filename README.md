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
And execute the module as normal.
