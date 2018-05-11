-module(case_of_guards).
-export([insert/2]).
-export([beach/1]).
-export([t/1]).
-export([beachf/1]).

insert(X,[]) ->
	     [X];
insert(X,Set) ->
	      case lists:member(X,Set) of
	      	   true -> Set;
		   false -> [X|Set]
	      end.

beach(Temp) ->
	    case Temp of 
	    	 {cel, N} when N >= 20, N =< 45 ->
		       'favorable';
		 {kel, N} when N >= 293, N =< 318 ->
		       'scientifically favorable';
		 {feh, N} when N >=68, N =< 113 ->
		       'favorable in the US';
		 _ ->
			'avoid beach'
	    end.

t(T) ->
     true.

beachf({cel, N}) when N >= 20, N=<45 ->
	     'favorable';
beachf({kal, N}) when N >= 293, N =<318 ->
	     'scientifically favourable';
beachf({feh, N}) when N >= 68, N =<113 ->
	     'favourable in the US';

beachf(_) ->
	  'avoid beach'.