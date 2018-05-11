-module(pythag).
-export([pythag/1]).
-import(lists,[seq/2]).

pythag(N)->
    [{A,B,C} || 
	A<-lists:seq(1,N), 
	B<-lists:seq(1,N), 
	C<-lists:seq(1,N),
	A*A+B*B=:=C*C, A+B+C=<N].
