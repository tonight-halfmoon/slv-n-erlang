-module(perms).
-export([perms/0]).

%%% How to use

%% Eshell V9.3  (abort with ^G)
%% 1> c(perms).
%% {ok,perms}
%% 2> Perms = fun perms:perms/0
%% 2> .
%% #Fun<perms.perms.0>
%% 3> Perms("123").
%% ** exception error: perms:perms/0 called with one argument
%% 4> PermsFun = Perms().
%% #Fun<perms.0.22359056>
%% 5> PermsFun("123").
%% ["123","132","213","231","312","321"]
%% 6> 

perms() -> fun(X) -> G = fun([], _F) -> [[]]; (L, F) -> [[H|T] || H <- L, T <- F(L -- [H], F)] end, G(X, G) end.
