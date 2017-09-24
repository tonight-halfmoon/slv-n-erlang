%%% examples on List Comprehension

[case {X/=Y, Z/=Y,X/=Z} of {true, true, true} -> [Y,X,Z]; _ -> [] end || X <- L, Y <- lists:reverse(L), Z <- [lists:nth(2,L)]].

[LLL| lists:map( fun(L) -> [ case {X/=Y, X/=Z, Y/=Z, T/=X, T/=Y} of {true, true, true, true, true} -> [X,Y,Z,T]; _ -> [] end || X <- L, Y <- lists:reverse(L), Z <- lists:sublist(L, M -I,1), T <- lists:sublist(L,M,1)] end, LLL)]).

%%% map 
[lists:nth(M, X) || X <- L]]
