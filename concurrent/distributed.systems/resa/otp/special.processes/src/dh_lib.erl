%%% @author  <rosemary@SCUBA>
%%% @copyright (C) 2017, 
%%% @doc
%%% A simple library to help Data Handler
%%% @end
%%% On the 2 Dec 2017

-module(dh_lib).
-export([pairwith_hash/1, keymember/2, keydelete/2, values/1]).
-include_lib("eunit/include/eunit.hrl").
-include("config_internal.hrl").

pairwith_hash_input_term_test_() ->
    R = 'ab.12.0',
    {
      "When a term is provided, then function 'pairwith_hash/1' must return the term paired with its hash defined by 'erlang:phash2/1'",
      ?_assertEqual(#res_ds{hash=erlang:phash2(R), value=R}, pairwith_hash(R))
    }.

pairwith_hash_input_list_test_() ->
    L = [H|_] = ['ab.12.0'],
    {
      "When a list of terms is provided, then function 'pairwith_hash/1' must return a list of terms each paired with its hash defined by 'erlang:phash2/1'",
      ?_assertEqual([#res_ds{hash=erlang:phash2(H), value=H}], pairwith_hash(L))
    }.

pairwith_hash_input_empty_list_test_() ->
    {
      "When an empty list is provided, then an empty list is returned back.",
      ?_assertEqual([], pairwith_hash([]))
    }.

pairwith_hash_input_string_test_() ->
    {
      "When a resource provided as a string, then function 'pairwith_hash/1' must tranform the string to an atom and return its atom paired with the atom's hash defined by 'erlang:phash2/1'",
      ?_assertEqual(#res_ds{hash=erlang:phash2(list_to_atom("ab")), value=ab}, pairwith_hash("ab"))
    }.

pairwith_hash_input_list_with_utf8_encoded_test_() ->
    {
      "When a resources provided as a list with utf8 encoded, then function 'pairwith_hash/1' must transform the utf-8 list to an atom and return its atom paired with the atom's hash defined by 'erlang:phash2/1'",
      ?_assertEqual(#res_ds{hash=erlang:phash2('\f\025\féz'), value='\f\025\féz'}, pairwith_hash([12,21,12,233,122]))
    }.

pairwith_hash_input_binary_with_utf8_encoded_codepoints_test_() ->
    {
      "When a resources provided as a binary with utf8 encoded codepoints, then function 'pairwith_hash/1' must transform the utf-8 binary to an atom and return its atom paired with the atom's hash defined by 'erlang:phash2/1'",
      ?_assertEqual(#res_ds{hash=erlang:phash2('\f\025\féz'), value='\f\025\féz'}, pairwith_hash(<<12,21,12,233,122/utf8>>))
    }.

pairwith_hash(B) when is_binary(B) ->
    pairwith_hash(binary_to_list(B));
pairwith_hash(R) when not is_list(R) ->
    #res_ds{hash=erlang:phash2(R), value=R};
pairwith_hash([]) ->
    [];
pairwith_hash(L) ->
    case is_string(L) of
	true ->
	    pairwith_hash(list_to_atom(L));
	false ->
	    pairwith_hash(L, [])
    end.

pairwith_hash([], Hashp) ->
    Hashp;
pairwith_hash([H|T], Hashp) ->
    pairwith_hash(T, [pairwith_hash(H)|Hashp]).


keymember_test_() ->
    Hash = erlang:phash2(ab),
    {
      "When a list of records and a key provided, and the key exists for one record, then function 'keymember' must return true. ",
      ?_assertEqual(true, keymember(Hash, [{#res_ds{hash=Hash, value=any}, pid}]))
    }.

keymember_key_not_exit_test_() ->
    Hash = erlang:phash2(ab),
    Hash2 = erlang:phash2(as),
    {
      "When a list of records and a key provided, and the key does not exist for one record, then function 'keymember' must return false. ",
      ?_assertEqual(false, keymember(Hash2, [{#res_ds{hash=Hash, value=any}, pid}]))
    }.

keymember(_Hash, []) ->
    false;
keymember(Hash, [{#res_ds{hash=Hash, value=_}, _}|_]) ->
    true;
keymember(Hash, [_H|T]) ->
    keymember(Hash, T).

keydelete_test_() ->
    Hash = erlang:phash2('asv.32'),
    {
      "When a key is provided and a list of records, and the key exists, then function 'keydelete' must return a new list with the record which has the key",
      ?_assertEqual([], keydelete(Hash, [{#res_ds{hash=Hash, value=any}, pid}]))
    }.

keydelete_key_not_exist_test_() ->
    Hash = erlang:phash2('asv.32'),
    Hash2 = erlang:phash2('ab.bb.r'),
    L = [{#res_ds{hash=Hash, value=any}, pid}],
    {
      "When a key is provided and a list of records, and the key exists, then function 'keydelete' must return a new list with the record which has the key",
      ?_assertEqual(L, keydelete(Hash2, L))
    }.

keydelete(Hash, L) ->
    keydelete(Hash, L, []).

keydelete(_Hash, [], RM) ->
    RM;
keydelete(Hash, [{#res_ds{hash=Hash, value=_}, _}|T], RM) ->
    lists:append(RM, T);
keydelete(Hash, [H|T], RM) ->
    keydelete(Hash, T, [H|RM]).

is_string(I) ->
    try list_to_atom(I) of
	Atom when is_atom(Atom) ->
	    true
    catch
	error:badarg ->
	    false
    end.

values_free_list_test_() ->
    L = [#res_ds{hash=any, value=ab}, #res_ds{hash=any, value=rt}, #res_ds{hash=any, value=pl}],
    {
      "When function 'values' is invoked with a list of record #res_ds{hash, value}, then it must return a list of all values without the keys",
      ?_assertEqual([pl, rt, ab], values(L))
    }.

values_allocated_list_test_() ->
    L = [{#res_ds{hash=any, value=ab}, any}, {#res_ds{hash=any, value=rt}, any}, {#res_ds{hash=any, value=pl}, any}],
    {
      "When function 'values' is invoked with a list of {#res_ds{hash, value}, Pid}, then it must return a list of all values without the keys",
      ?_assertEqual([pl, rt, ab], values(L))
    }.

values(L) ->
    values(L, []).

values([], Vs) ->
    Vs;
values([#res_ds{hash=_, value=V}|T], Vs) ->
    values(T, [V|Vs]);
values([{#res_ds{hash=_, value=V}, _}|T], Vs) ->
    values(T, [V|Vs]).
