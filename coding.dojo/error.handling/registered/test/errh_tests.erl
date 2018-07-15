-module(errh_tests).
-include_lib("eunit/include/eunit.hrl").
-import(errh, [start/0, request/1]).

parent_proc__must_not_restart_when_linked_proc_crash_test() ->
    Pid = self(),

    {ok, _LPid} = start(),

    _Result = request(1),

    _Result2 = request(one),
    receive after 1 -> ok end,

    Self = self(),

    ?assertEqual(pid_to_list(Pid), pid_to_list(Self)).
