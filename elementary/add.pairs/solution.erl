-module(solution).
-author("Ahmad Elghafari").
-date("today").
-comman("What++++?").
-export([main/0]).

% Write a program that adds two numbers prints the sum to STDOUT.
% Read the input from STDIN. The first line of your input will contain an integer (N) that tells you how many more lines there are in the input. 
% Each of the subsequent N lines contain 2 integers). You need to print the sum of each pair on a separate line of STDOUT.

main() ->
    {ok, [N]} = io:fread("", "~d"),
    test(N).

test(0) -> ok;
test(N) -> 
    {ok, [A,B]} = io:fread("", "~d ~d"),
    io:fwrite("~w~n", [(A+B)]),
    test(N-1).
