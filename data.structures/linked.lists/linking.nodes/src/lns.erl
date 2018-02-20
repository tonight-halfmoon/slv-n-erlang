-module(lns).

-export([new/0,
	 from_list/1,
	 head/1, tail/1,
	 append/2, prepend/2,
	 visit_all/1,
	 merge_tails/2,
	 extend/2,
	 to_list/1,
	 pop/1,
	 nth/2,
	 count_duplicates/2,
	 show_duplicates/2]).

-record(time_visited, {timestamp = 0}).
-record(node, {value = 'empty', next = nil, time_visited = #time_visited{}}).
-record(lns, {head = nil}).

%%%===================================================================
%%%  API
%%%===================================================================

new() ->
    #lns{}.

head(Lns) ->
    Lns#lns.head.

tail(_ = #lns{head = #node{value = 'empty'}}) ->
    #node{};
tail(Node = #node{value = _, next = nil})->
    Node;
tail(#node{value = _, next = Next}) ->
    tail(Next);
tail(#lns{head = nil}) ->
    nil;
tail(Lns) ->
    tail(Lns#lns.head).

append(#lns{head = nil}, Data) ->
    #lns{head = #node{value = Data}};
append(Lns, Data) ->
    Nhead = append_node(head(Lns), Data),
    #lns{head = Nhead}.

prepend(Lns, Data) ->
    Nhead = #node{value = Data, next = Lns#lns.head},
    #lns{head = Nhead}.

visit_all(#lns{head = nil}) ->
    empty_lns;
visit_all(Lns) ->
    Head = head(Lns),
    Next = Head#node.next,
    Nhead = visit_next(Head, Next),
    #lns{head = Nhead}.

merge_tails(Lns1, Lns2) ->
    Nhead = append_node(head(Lns1), tail(Lns2)),
    #lns{head = Nhead}.

extend(#lns{head = nil}, Lns) ->
    Lns;
extend(Lns1, Lns2) ->
    Nhead = append_node(head(Lns1), head(Lns2)),
    #lns{head = Nhead}.

to_list(Lns) ->
    to_list(Lns, []).

from_list(L) ->
    prepend_all(lists:reverse(L), #lns{}).

pop(#lns{head = nil}) ->
    empty_lns;
pop(_ = #lns{head = #node{value = V, next = Next}}) ->
    {V, #lns{head = Next}}.

nth(N, Lns) ->
    nth(N, 0, Lns#lns.head).

%%%===================================================================
%%% Show duplicates for two Linked Lists. It shows which nodes on the
%%% the first linked lists have duplicates in the second linked list 
%%% and the count as a pair. Duplicates in the first linked list are 
%%% not considered. 
%%% A duplicate instance is a node in a second linked list having the 
%%% same data value of a node in the first linked list. 
%%%
%%% Assumptions:
%%% The two linked lists input are completely independent. Each having
%%% a different head and there is no merge point for the two. Each one
%%% ends with a different tail node.
%%%
%%% Returns:
%%% A List of pairs, each contains the source node and the number of 
%%% duplicate instances found in the second inputlinked list
%%%===================================================================
show_duplicates(Lns1, Lns2) ->
    Dict = traverse(Lns1),
    show_dups(Dict, head(Lns2)).

%%%===================================================================
%%% Count duplicates for two Linked Lists. It shows how many nodes in
%%% the second linked list which are duplicates of nodes from
%%% the first linked list. Duplicates in the same linked list are not
%%% considered. A duplicate instance is a node in a second linked list 
%%% having the same data value of a node in the first linked list. 
%%%
%%% Assumptions:
%%% The two linked lists input are completely independent. Each having
%%% a different head and there is no merge point for the two. Each one
%%% ends with a different tail node.
%%%
%%% Returns:
%%% The total number of duplicates found in the second Linked List
%%% provided as input.
%%%===================================================================
count_duplicates(Lns1, Lns2) ->
    Dict = traverse(Lns1),
    count_dups(Dict, head(Lns2)).

%%%===================================================================
%%% Internal Functions
%%% @private
%%%===================================================================

append_node(Head = #node{next = nil}, Node = #node{}) ->
    Head#node{next = Node};
append_node(Head = #node{next = nil}, Data) ->
    Head#node{next = #node{value = Data}};
append_node(Head, Data) ->
    Head#node{next = append_node(Head#node.next, Data)}.

visit_next(Head, _Next = nil) ->
    Head#node{time_visited = #time_visited{timestamp = os:system_time()}};
visit_next(Head, Next) ->
    Head#node{time_visited = #time_visited{timestamp = os:system_time()},
	      next = visit_next(Head#node.next, Next#node.next)}.

visit_node(#node{value = V, next = Next, time_visited = _}) ->
    #node{value = V, next = Next, time_visited = #time_visited{timestamp = os:system_time()}}.

to_list(#lns{head = nil}, L) ->
   L;
to_list(nil, L) ->
    lists:reverse(L);
to_list(Lns = #lns{}, L) ->
    to_list(head(Lns), L);
to_list(Next = #node{}, L) ->
    Visited = visit_node(Next),
    to_list(Next#node.next, [{Visited#node.value, Visited#node.time_visited}|L]).

prepend_all([], Lns) ->
    Lns;
prepend_all([H|T], Lns) ->
    NewLns =  prepend(Lns, H),
    prepend_all(T, NewLns).

nth(N, I, _) when N < I ->
    n_outside;
nth(N, N, nil) ->
    n_outside;
nth(N, N, #node{value = V}) ->
    V;
nth(N, I, #node{value = _, next = Next}) ->
    nth(N, I + 1, Next).

traverse(#lns{head = nil}) ->
    dict:new();
traverse(Lns) ->
    Dict = dict:new(),
    traverse(head(Lns), Dict).

traverse(nil, Dict) ->
    Dict;
traverse(Next, Dict) ->
    traverse(Next#node.next, dict:store(Next#node.value, 0, Dict)).

show_dups(Dict, nil) ->
    dict:to_list(dict:filter(fun filter_predicate/2, Dict));
show_dups(Dict, Next) ->
    show_dups(dict:update(Next#node.value, fun(V) -> V + 1 end, 0, Dict), Next#node.next).

filter_predicate(_Key, Value) when Value > 0 ->
    true;
filter_predicate(_Key, Value) when Value =:= 0 ->
    false.

count_dups(Dict, nil) ->
    dict:fold(fun(_Key, Value, AccIn) ->
		      case Value > 0 of
			  true ->
			      AccIn + Value;
			  false ->
			      AccIn
		      end
	      end, 0, Dict);
count_dups(Dict, Next) ->
    count_dups(dict:update(Next#node.value, fun(V) -> V + 1 end, 0, Dict), Next#node.next).

update_dict(V, Initial, Dict) ->
    dict:update(V, fun(X) -> X + 1 end, Initial, Dict).
			   
