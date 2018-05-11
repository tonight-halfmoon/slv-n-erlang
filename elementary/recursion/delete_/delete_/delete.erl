-module(delete).
-export([delete/2, deleteX_using_list_comprehsion/2, deleteX_recursively/2, deleteX_recursively_caseof/2, deleteX/2]).

delete(X, [X|T])->
    delete(X, T);
delete(X, [Y|T]) ->
    [Y| delete(X, T)];
delete(_, []) ->
    [].



deleteX_using_list_comprehsion(X, L) ->
    [Nex || Nex <- L, Nex /= X].

deleteX_recursively(_,[])->
    [];
deleteX_recursively(X, [Hd|Res]) ->
    if X == Hd ->
       deleteX_recursively(X, Res);
       X /= Hd ->
	[Hd] ++ deleteX_recursively(X, Res) 
   end.


deleteX_recursively_caseof(_, []) ->
    [];
deleteX_recursively_caseof(X, [Hd|Res]) ->
    case X of
	 X when X == Hd ->
	    deleteX_recursively_caseof(X, Res);
	 X when X /= Hd ->
	    [Hd] ++ deleteX_recursively_caseof(X,Res)
    end.


deleteX(X, List) ->
    deleteX_tail_recursive(X, List, []).


deleteX_tail_recursive(_, [], AccList_) ->
    AccList_;
deleteX_tail_recursive(X, [Hd|Res], AccList) ->
    if Hd == X ->
	    deleteX_tail_recursive(X, Res, AccList);
       Hd /= X ->
	    deleteX_tail_recursive(X, Res, [Hd]++ AccList)
    end.
