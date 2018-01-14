-module(amqp_sp_sup).

-behaviour(supervisor).

-export([start_link/1]).

-export([init/1]).

start_link(DeclareArgs) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, DeclareArgs).

init({ExchangeDeclareArgs, QueueDeclareArgs}) ->
    SupFlags = #{startegy => one_for_one, intensity => 1, period => 5},
    AMQPExchangeChildSpecs = #{id => amqp_sp_exchange_child,
				      start => {amqp_sp_exchange, start_link, [ExchangeDeclareArgs]}, % mfa
				      restart => permanent,
				      shutdown => brutal_kill,
				      type => worker,
				      modules => []},
    AMQPQueueChildSpecs = #{id => amqp_sp_queue_child,
				   start => {amqp_sp_queue, start_link, [QueueDeclareArgs]},
				   restart => permanent,
				   shutdown => brutal_kill,
				   type => worker,
				   modules => []},
    {ok, {SupFlags, [AMQPExchangeChildSpecs, AMQPQueueChildSpecs]}}.
