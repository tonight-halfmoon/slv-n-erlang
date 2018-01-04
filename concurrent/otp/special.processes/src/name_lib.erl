%%% @author  <rosemary@SCUBA>
%%% @copyright (C) 2017, 
%%% @doc
%%%
%%% @end
%%% On the 16th December 2017

-module(name_lib).
-export([unregister_all/1]).
-include_lib("eunit/include/eunit.hrl").

unregister_all([]) ->
    true;
unregister_all([H|T]) ->
    case whereis(H) of
	undefined ->
	    unregister_all(T);
	_ ->
	    unregister(H),
	    unregister_all(T)
    end.

unregister_all_test_() ->
    register(name_1, spawn(fun() -> [] end)),
    ?assertMatch(Pid when is_pid(Pid), whereis(name_1)),
    unregister_all([name_1]),
    {
      "When a list of registered name processes has been provided, then each registered process must be unregistered",
      ?_assertEqual(undefined, whereis(name_1))
    }.
