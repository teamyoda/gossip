-module(exp5).
-export([run_experiment/0]).

%% This experiment proves the correctness of the request_fragmnet function for assignment task 5. 

run_experiment() ->
    io:format("~n~nThis experiment tests task 5 of the assignment. It will show the fragments stored at node 1 and node 10 to show that node 1 does not initially contain fragment 10. Fragment 10 will be requested and 200 rounds of gossip will run. The fragments now stored at node 1 will be printed to prove it now stores fragment 10.~n~n"),
    io:format("%% Starting monitor~n"),
    monitor:start(),
    io:format("%% Creating netowork with 1000 nodes~n"),
    {Network, File} = monitor:create_network(1000, fragments),
    Fid_10 = lists:nth(10, Network),
    io:format("%% Fragment 10 has ID ~p~n", [Fid_10]),
    io:format("~n//////////////////////~n~n"),
    io:format("%% Showing fragments currently stored at node 1~n"),
    Node1_fragments = monitor:get_fragments(hd(Network)),
    io:format("~p~n~n", [Node1_fragments]),
    io:format("%% Notice node 1 does not have fragment 10 (fragment ID ~p)~n", [Fid_10]),
    io:format("~n//////////////////////~n~n"),
    io:format("%% Showing fragments currently stored at node 10~n"),
    Node10_fragments = monitor:get_fragments(lists:nth(10, Network)),
    io:format("~p~n~n", [Node10_fragments]),
    io:format("%% Notice node 10 does have fragment 10 (fragment ID ~p)~n", [Fid_10]),
    io:format("~n//////////////////////~n~n"),
    io:format("%% Requesting fragment 10 to be stored at node 1~n"),
    monitor:request_fragment(10, Network),
    io:format("%% Stepping through 200 steps of gossip~n"),
    monitor:step(Network, 200),
    io:format("~n//////////////////////~n~n"),
    io:format("%% Showing fragments now stored at node 1~n"),
    Node1_new_fragments = monitor:get_fragments(hd(Network)),
    io:format("~p~n~n", [Node1_new_fragments]),
    io:format("%% Notice node 1 now contains fragment 10 with ID ~p~n", [Fid_10]).



