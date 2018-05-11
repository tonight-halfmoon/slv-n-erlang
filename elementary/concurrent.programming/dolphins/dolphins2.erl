-module(dolphins2).
-compile(export_all).

dolphin1() ->
    receive 
	do_a_flip ->
	    io:format( "flipping...~n");
	fish ->
	    io:foramt("swimming...~n");
	_ ->
	    io:format("what?~n")
	end.
