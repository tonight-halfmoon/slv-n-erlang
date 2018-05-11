-module(dolphins).
-compile(export_all).

dolphin1() ->
	   receive
		do_a_flip ->
			  io:format("How about no?~n");
		fish	  ->
			  io:format("So long and ...!~n");
		_ ->
			  io:format("Heh, ...")
	end.