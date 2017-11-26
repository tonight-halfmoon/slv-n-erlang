-module(monitor_resa_nodes).
-export([monitor/1, monitor_node_server/1, monitor_node_client/0]).

%%% In case you want to start in on another node
%%% Then, invoke: net_kernel:start([monitor, shortnames]),
%%% And after that, spawn/4 -> providing the node sname, i.e., monitor@localhost
monitor(Pid) -> 
    process_flag(trap_exit, true),
    spawn(?MODULE, monitor_node_server, [Pid]).

monitor_node_server(Pid) ->
    net_kernel:monitor_nodes(true),
    net_kernel:connect_node(resa@localhost),
    receive
	{'EXIT', Pid, Reason} ->
	    io:format("Trapped an 'EXIT' flag by ~p, for reason '~p'~n", [Pid, Reason]),
	    exit(Reason);
        {nodedown, resa@localhost} ->
	    io:format("Server node is down~n", []),
	    Pid ! server_node_down;
	{nodeup,resa@localhost} ->
	    io:format("Server is running~n", []);
	M ->
	    io:format("Moitor nodes received: ~p~n", [M])
    end,
    monitor_node_server(Pid).

monitor_node_client() ->
    process_flag(trap_exit, true),
    net_kernel:monitor_nodes(true),
    net_kernel:connect(client@localhost),
    receive
	{'EXIT', FromPid, Reason} ->
	    io:format("Trapped an 'EXIT' flag by '~p' for reason '~p'~n", [FromPid, Reason]),
	    exit(Reason);
	M ->
	    io:format("Client node says '~p'~n", [M])
    end,
    monitor_node_client().
