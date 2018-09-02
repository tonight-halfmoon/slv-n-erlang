-module(standard_functions).

-export([map/2, filter/2, append/1]).


map(F, Xs) -> [F(X) || X <- Xs].
filter(P, Xs) -> [X || X <- Xs, P(X)].
append(Xss) -> [X || Xs <- Xss, X <- Xs].
