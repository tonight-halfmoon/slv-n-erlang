-record(allocate_resource, {server, from_pid}).
-record(res_ds, {hash, value}).
-record(free_resource, {server, from_pid, resource}).
-record(data_structure, {free=#res_ds{}, allocated=#res_ds{}}).
-record(server_request_data, {server}).
-record(handler_reply, {message}).
-record(handler_reply_data, {data=#data_structure{}}).
-record(handler_refused, {reason}).
