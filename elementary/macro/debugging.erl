-module(debugging).

-export([target/1]).
-export([testcall/0]).
-export([testcall/1]).

-record(person, {name, age}).

-define(DBG(Str, Args), ok).
%-define(DBG(Str, Args), io:format(Str, Args)).

-ifdef(debug).
-define(PRINT(Args), io:format("PRINT_~p: ~p~n", [??Args, Args])).
-define(VALUE(Call), io:format("~p = ~p~n", [??Call, Call])).
-define(PRINT_LINE(), ?PRINT(?LINE)).
-define(PRINT_MODULE_STRING(), ?PRINT(?MODULE_STRING)).
-define(PRINT_FILE, ?PRINT(?FILE)).
-else.
-define(PRINT(Args), ok).
-define(VALUE(Call), ok).
-define(PRINT_LINE(), ok).
-define(PRINT_MODULE_STRING(), ok).
-define(PRINT_FILE, ok).

-endif.

target(#person{age=Age} = P) ->
    ?DBG("in records1:target(~p)~n", [P]),
    P#person{age=Age+1}.

testcall() ->
    ?VALUE(length([1,2,3])).

testcall(Args) ->
    ?VALUE(Args),
    ?PRINT_LINE(),
    ?PRINT_FILE.
