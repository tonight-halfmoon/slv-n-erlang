-module(beam_disasm_client).
-export([recover/1]).

-define(outfile, 'recovered.erl').

recover([Beam]) ->
	file:write_file(?outfile, io_lib:fwrite("~p.\n", [beam_disasm:file(Beam)])),
	{ok, done}.


