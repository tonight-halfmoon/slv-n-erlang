%%%-------------------------------------------------------------------
%%% @author  <rosemary@SCUBA>
%%% @copyright (C) 2018, 
%%% @doc
%%%
%%% @end
%%% On the 7th of January 2018
%%%-------------------------------------------------------------------
-module(consumer).

-export([spawn_link/0, sub/2]).

-include("config.hrl").

-include_lib("amqp_client/include/amqp_client.hrl").

spawn_link() ->
    spawn_link(?MODULE, sub, [whereis(?ch), ?queue]).

sub(Channel, Queue_name) ->
    Binding = #'queue.bind'{queue = Queue_name, exchange = ?exch, routing_key = Queue_name},
    #'queue.bind_ok'{} = amqp_channel:call(Channel, Binding),
    Sub = #'basic.consume'{queue = Queue_name},
    #'basic.consume_ok'{consumer_tag = Tag} = amqp_channel:subscribe(Channel, Sub, self()),
    io:format("Tag: ~p~n", [Tag]),
    process_flag(trap_exit, true),
    active(Channel).

active(Channel) ->
    receive
	{'EXIT', _From, _Reason} ->
	    close_conn(),
	    terminated;
	#'basic.consume_ok'{} = Msg ->
	    io:format("~p ~p received ~p~nChannel: ~p~n~n~n", [?MODULE, self(), Msg, Channel]),
	    active(Channel);
	#'basic.cancel_ok'{} = Msg ->
	    io:format("~p ~p received ~p~nChannel: ~p~n~n~n", [?MODULE, self(), Msg, Channel]),
	    ok;
	{#'basic.deliver'{delivery_tag = Tag}, Content} ->
	    io:format("~p ~p received payload ~p~nChannel: ~p~n~n~n", [?MODULE,self(), Content, Channel]),
	    amqp_channel:cast(Channel, #'basic.ack'{delivery_tag = Tag}),
	    io:format("Ack has been sent to the channel~n", []),
	    active(Channel)
    end.
   
close_conn() ->
    amqp_connection:close(whereis(?conn)).
