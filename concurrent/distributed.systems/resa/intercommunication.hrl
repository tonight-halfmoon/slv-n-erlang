-record(connect, {client_pid}).
-record(server_reply,{message}).
-record(cask2alloc, {client_pid}).
-record(cask2free, {client_pid, resource}).
-record(cask4stats, {client_pid}).
-record(abort_client, {message}).
-record(stats, {name, length}).
-record(stats_reply, {stats_free=#stats{}, stats_allocated=#stats{}}).

