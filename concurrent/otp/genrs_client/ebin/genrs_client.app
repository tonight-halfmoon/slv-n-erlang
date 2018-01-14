{application, 'genrs_client',
	      [{description, "A AMQP client for GenRS"},
	       {vsn, "1"},
	       {registered, [genrs_client_sup]},
	       {modules, []},
	       {mod, {genrs_client, []}},
	       {applications, [kernel, stdlib]},
	       {env, []}
	       ]}.
