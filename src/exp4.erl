-module(exp4).
-export([run_experiment/0]).

%% This experiment proves the correctness of the store_fragmnet function for assignment task 4. 

run_experiment() ->
    io:format("~n~nThis experiment tests task 4 of the assignment. It will show the fragments stored at node 10 to show the initial value of fragment 10. The store_fragment function will be run to update the data stored in fragment 10 at node 1, and 200 rounds of gossip will run. The fragments stored at node 10 will be shown again to show that fragment 10 now stores the updated data value.~n~n"),
    io:format("%% Starting monitor~n"),
    monitor:start(),
    io:format("%% Creating netowork with 1000 nodes~n"),
    {Network, File} = monitor:create_network(1000, fragments),
    Fid_10 = lists:nth(10, Network),
    io:format("%% Fragment 10 has ID ~p~n", [Fid_10]),
    io:format("~n//////////////////////~n~n"),
    io:format("%% Showing fragments currently stored at node 10~n"),
    Node10_fragments = monitor:get_fragments(lists:nth(10, Network)),
    io:format("~p~n~n", [Node10_fragments]),
    io:format("%% Notice the value of fragment 10 (fragment ID ~p)~n", [Fid_10]),
    io:format("~n//////////////////////~n~n"),
    io:format("%% Updating contents of fragment 10 as [1,2,3,4,5,6] to be stored at node 1~n"),
    monitor:store_fragment(10, [1,2,3,4,5,6], Network),
    io:format("%% Stepping through 200 steps of gossip~n"),
    monitor:step(Network, 200),
    io:format("~n//////////////////////~n~n"),
    io:format("%% Showing fragments now stored at node 10~n"),
    Node10_new_fragments = monitor:get_fragments(lists:nth(10, Network)),
    io:format("~p~n~n", [Node10_new_fragments]),
    io:format("%% Notice fragment 10 (fragment ID ~p) is updated with the new data~n", [Fid_10]).



