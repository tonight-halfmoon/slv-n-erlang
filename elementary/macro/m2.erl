-module(m2).
-author("Ahmad Elghafari").
-date("today").
-what("Revising Macros").
-compile(export_all).

-define(sub(X,Y), X - Y).
-define(always, 5000).
-define(CAPITAL, thisiscapital).

-ifdef(TEST).
m_test() ->
    io:format("~n", []).
-endif.

main() ->
    io:format("'?CAPITAL = ~p; '?always' = ~p; 'sub(50,40)' = ~p; ?TEST = ~p; All of this at line: ~p~n", [?CAPITAL, ?always, ?sub(50,40), m_test(), ?LINE]).
    
