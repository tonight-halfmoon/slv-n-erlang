-module(broadcast).
-compile(export_all).

send(IoList) ->
    case inet:ifget("eth0", [broadaddr]) of
	{ok, [{broadaddr, Ip}]} ->
	    {ok, S} = gen_udp:open(5010, [{broadcast, true}]),
	    gen_udp:send(S, Ip, 6000, IoList),
	    
