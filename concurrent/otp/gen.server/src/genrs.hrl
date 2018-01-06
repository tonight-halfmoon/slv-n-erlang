
%%% Internal GenRS protocols
-record(cask2free, {resource}).
-record(cask2alloc, {}).
-record(cask_dstats, {}).

%%% common
-record(state, {free, allocated}).
-record(ok, {more}).
-record(error, {reason}).
-record(res_ds, {hash, value}).
-record(data_structure, {free=#res_ds{}, allocated=#res_ds{}}).
