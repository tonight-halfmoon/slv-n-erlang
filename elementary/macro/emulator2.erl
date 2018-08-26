
56> c(debugging).
debugging.erl:34: Warning: variable 'Args' is unused
{ok,debugging}
57> debugging:testcall(asd).
ok
60> c(debugging, [{d, debug}]).
{ok,debugging}
61> debugging:testcall(asd).
"Args" = asd
PRINT_"? LINE": 36
PRINT_"? FILE": "debugging.erl"
ok
62> 
