{application, 'amqp_service_provider',
	      [{description, "AMQP client defines the necessary Protocols to communicate with comforming messaging middleware broker RabbitMQ"},
	       {vsn, "1"},
	       {registered, [amqp_sp_sup]},
	       {modules, []},
	       {mod, {amqp_sp, {<<"exch6_genrs">>, <<"queue6_genrs">>}}},
	       {applications, [kernel, stdlib]},
	       {env, []}
	       ]}.
