-module(genrs_client_sup).

-beahviour(supervisor).

-export([start_link/1]).

-export([init/1]).

start_link(Args) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, Args).

init(Args) ->
    SupFlags = #{strategy => one_for_one, intensity => 1, period => 5},
    GenRSConsumerChildSpecs = #{id => genrs_amqp_consumer_child,
				start => {genrs_amqp_consumer, start_link, [Args]}, % mfa
				restart => permanent,
				shutdown => brutal_kill,
				type => worker,
				modules => []
			       },
    {ok, {SupFlags, [GenRSConsumerChildSpecs]}}.
