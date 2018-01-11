
-include("genrs.hrl").

-define(sm, service_manager).
-define(ssp, service_stats_provider).
-define(rh, res_handler).

-record(cask2free, {resource}).
-record(cask2alloc, {}).
-record(cask_dstats, {}).

-record(ok, {more}).
-record(error, {reason}).
-record(res_ds, {hash, value}).
-record(data_structure, {free=#res_ds{}, allocated=#res_ds{}}).
