{application, rssp,
	      [{description, "Resa Server implements OTP Special Processes"},
	       {vsn, "1"},
	       {registered, [rsspa]},
	       {applications, [kernel, stdlib]},
	       {mod, {resa_server, [['ap.r109']]}},
	       {env, []},
	       {modules, [resa_server, handler, stats_provider]}
	      ]}.