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

-export_type([linked_list/0]).

-record(time_visited, {timestamp = erlang:timestamp() :: integer()}).
-record(node, {value :: atom(), next :: #node{}, time_visited :: #time_visited{}}).
-record(lns, {head :: #node{}}).

-opaque linked_list() :: #lns{}.

-type node@() :: #node{}.

%%%===================================================================
%%%  API
%%%===================================================================

-spec new() -> linked_list().
 
new() -> #lns{}.

-spec head(linked_list()) -> node@().

head(Lns) -> Lns#lns.head.

tail(_ = #lns{head = #node{value = undefined}}) ->
    #node{};
tail(Node = #node{value = _V, next = undefined})->
    Node;
tail(#node{value = _V, next = Next}) ->
    tail(Next);
tail(#lns{head = undefined}) ->
    undefined;
tail(Lns) ->
    tail(Lns#lns.head).

append(#lns{head = undefined}, Data) ->
    #lns{head = #node{value = Data}};
append(Lns, Data) ->
    Nhead = append_node(head(Lns), Data),
    #lns{head = Nhead}.

prepend(Lns, Data) ->
    Nhead = #node{value = Data, next = Lns#lns.head},
    #lns{head = Nhead}.

visit_all(#lns{head = undefined}) ->
    empty_lns;
visit_all(Lns) ->
    Head = head(Lns),
    Next = Head#node.next,
    Nhead = visit_next(Head, Next),
    #lns{head = Nhead}.

-spec merge_tails(linked_list(), linked_list()) -> linked_list().

merge_tails(Lns1, Lns2) ->
    Nhead = append_node(head(Lns1), tail(Lns2)),
    #lns{head = Nhead}.

-spec extend(linked_list(), linked_list()) -> linked_list().

extend(#lns{head = undefined}, Lns) ->
    Lns;
extend(Lns1, Lns2) ->
    Nhead = append_node(head(Lns1), head(Lns2)),
    #lns{head = Nhead}.

-spec to_list(linked_list()) -> list() | nil().

to_list(Lns) ->
    to_list(Lns, []).

-spec from_list(L :: list()
		   | nil()) -> linked_list().

from_list(X) when not is_list(X) ->
    type_list_expected;
from_list(L) ->
    prepend_all(lists:reverse(L), #lns{}).

-spec pop(linked_list()) -> {atom(), linked_list()}.

pop(#lns{head = undefined}) ->
    empty_lns;
pop(_Lns = #lns{head = #node{value = V, next = Next}}) ->
    {V, #lns{head = Next}}.

-spec nth(N :: integer(), linked_list()) -> node@().

nth(N, Lns) -> nth(N, 0, Lns#lns.head).

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

-spec show_duplicates(linked_list(), linked_list()) -> [{atom(), integer()}].

show_duplicates(Lns1, Lns2) ->
    process_dups(Lns1, Lns2, fun show_dups/2).

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

-spec count_duplicates(linked_list(), linked_list()) -> integer().

count_duplicates(Lns1, Lns2) ->
    process_dups(Lns1, Lns2, fun count_dups/2).

%%%===================================================================
%%% Internal Functions
%%% @private
%%%===================================================================

append_node(Head = #node{next = undefined}, Node = #node{}) ->
    Head#node{next = Node};
append_node(Head = #node{next = undefined}, Data) ->
    Head#node{next = #node{value = Data}};
append_node(Head, Data) ->
    Head#node{next = append_node(Head#node.next, Data)}.

visit_next(Head, _Next = undefined) ->
    Head#node{time_visited = #time_visited{timestamp = os:system_time()}};
visit_next(Head, Next) ->
    Head#node{time_visited = #time_visited{timestamp = os:system_time()},
	      next = visit_next(Head#node.next, Next#node.next)}.

visit_node(#node{value = V, next = Next, time_visited = _Timestamp}) ->
    #node{value = V, next = Next, time_visited = #time_visited{timestamp = os:system_time()}}.

to_list(#lns{head = undefined}, L) ->
   L;
to_list(undefined, L) ->
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
nth(N, N, undefined) ->
    n_outside;
nth(N, N, #node{value = V}) ->
    V;
nth(N, I, #node{value = _V, next = Next}) ->
    nth(N, I + 1, Next).

show_dups(undefined, undefined) ->
    [{undefined, 0}];
show_dups(Dict, undefined) ->
    dict:to_list(dict:filter(fun filter_predicate/2, Dict));
show_dups(Dict, Next) ->
    show_dups(dict:update(Next#node.value, fun(V) -> V + 1 end, 0, Dict), Next#node.next).

filter_predicate(_Key, Value) when Value > 0 ->
    true;
filter_predicate(_Key, Value) when Value =:= 0 ->
    false.

count_dups(undefined, undefined) ->
    0;
count_dups(Dict, undefined) ->
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

process_dups(_Lns1 = #lns{head = undefined}, _Lns2, Fun) ->
    Fun(undefined, undefined);
process_dups(_Lns1, _Lns2 = #lns{head = undefined}, Fun) ->
    Fun(undefined, undefined);
process_dups(Lns1, Lns2, Fun) ->
    Dict = traverse(Lns1),
    Fun(Dict, head(Lns2)).

traverse(#lns{head = undefined}) ->
    dict:new();
traverse(Lns) ->
    Dict = dict:new(),
    traverse(head(Lns), Dict).

traverse(undefined, Dict) ->
    Dict;
traverse(Next, Dict) ->
    traverse(Next#node.next, dict:store(Next#node.value, 0, Dict)).
