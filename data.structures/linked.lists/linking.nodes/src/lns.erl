-module(lns).

-export([new/0,
	 from_list/1,
	 head/1, tail/1,
	 append/2, prepend/2,
	 visit_all/1,
	 merge_tails/2,
	 extend/2,
	 to_list/1]).

-record(time_visited, {timestamp = 0}).
-record(node, {value = 'empty', next = nil, time_visited = #time_visited{}}).
-record(lns, {head = nil}).

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
    Nhead = append_node(?MODULE:head(Lns), Data),
    #lns{head = Nhead}.

append_node(Head = #node{next = nil}, Node = #node{}) ->
    Head#node{next = Node};
append_node(Head = #node{next = nil}, Data) ->
    Head#node{next = #node{value = Data}};
append_node(Head, Data) ->
    Head#node{next = append_node(Head#node.next, Data)}.

prepend(Lns, Data) ->
    Nhead = #node{value = Data, next = Lns#lns.head},
    #lns{head = Nhead}.

visit_all(Lns) ->
    Head = ?MODULE:head(Lns),
    Next = Head#node.next,
    Nhead = visit_next(Head, Next),
    #lns{head = Nhead}.

merge_tails(Lns1, Lns2) ->
    Nhead = append_node(?MODULE:head(Lns1), ?MODULE:tail(Lns2)),
    #lns{head = Nhead}.

extend(Lns1, Lns2) ->
    Nhead = append_node(?MODULE:head(Lns1), ?MODULE:head(Lns2)),
    #lns{head = Nhead}.

to_list(Lns) ->
    to_list(Lns, []).

from_list(L) ->
    prepend_all(lists:reverse(L), #lns{}).

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
    to_list(?MODULE:head(Lns), L);
to_list(Next = #node{}, L) ->
    Visited = visit_node(Next),
    to_list(Next#node.next, [{Visited#node.value, Visited#node.time_visited}|L]).

prepend_all([], Lns) ->
    Lns;
prepend_all([H|T], Lns) ->
    NewLns =  prepend(Lns, H),
    prepend_all(T, NewLns).
