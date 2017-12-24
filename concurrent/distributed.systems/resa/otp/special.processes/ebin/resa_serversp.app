{application, resa_serversp,
	      [{description, "Resa Server implements OTP Special Processes"},
	       {vsn, "1"},
	       {registered, [resa_serversp]},
	       {applications, [kernel, stdlib]},
	       {mod, {resa_serversp, []}},
	       {env, []},
	       {modules, [resa_server, handler, stats_provider]}
	      ]}.