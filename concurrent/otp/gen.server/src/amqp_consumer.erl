%%%-------------------------------------------------------------------
%%% @author  <rosemary@SCUBA>
%%% @copyright (C) 2018, 
%%% @doc
%%%
%%% @end
%%% On the 7th of January 2018
%%%-------------------------------------------------------------------
-module(amqp_consumer).

-export([spawn_link/0, sub/2, cask_msg/0]).

-include("amqp_config.hrl").

-include_lib("amqp_client/include/amqp_client.hrl").

-record(state, {payload}).

spawn_link() ->
    register(?amqp_consumer, spawn_link(?MODULE, sub, [whereis(?ch), ?queue])).

sub(Channel, Queue_name) ->
    Binding = #'queue.bind'{queue = Queue_name, exchange = ?exch, routing_key = Queue_name},
    #'queue.bind_ok'{} = amqp_channel:call(Channel, Binding),
    Sub = #'basic.consume'{queue = Queue_name},
    #'basic.consume_ok'{consumer_tag = Tag} = amqp_channel:subscribe(Channel, Sub, self()),
    io:format("Tag: ~p~n", [Tag]),
    process_flag(trap_exit, true),
    active(Channel, #state{}).

active(Channel, State) ->
    receive
	{'EXIT', _From, _Reason} ->
	    terminated;
	#'basic.consume_ok'{} = Msg ->
	    io:format("~p ~p received ~p~nChannel: ~p~n~n~n", [?MODULE, self(), Msg, Channel]),
	    active(Channel, State);
	#'basic.cancel_ok'{} = Msg ->
	    io:format("~p ~p received ~p~nChannel: ~p~n~n~n", [?MODULE, self(), Msg, Channel]),
	    ok;
	{#'basic.deliver'{delivery_tag = Tag}, Content} ->
	    io:format("~p ~p received payload ~p~nChannel: ~p~n~n~n", [?MODULE,self(), Content, Channel]),
	    amqp_channel:cast(Channel, #'basic.ack'{delivery_tag = Tag}),
	    io:format("Ack has been sent to the channel~n", []),
	    New_state = #state{payload=Content},
	    active(Channel, New_state);
	#cask_consumer_msg{from=From} ->
	    From ! State,
	    active(Channel, State)
    end.
   
cask_msg() ->
    io:format("function cask_msg ~n", []),
    ?amqp_consumer ! #cask_consumer_msg{from=self()},
    receive
	Payload ->
	    Payload
	end.
