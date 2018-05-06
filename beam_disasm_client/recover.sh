#!/bin/sh

echo "" >&2

erlc $1 >&2
erl -noshell -pa ./ -s beam_disasm_client recover $2 -s init stop

#file:write_file("./server.erl", io_lib:fwrite("~p.\n", [beam_disasm:file(server)])).

