{application, rssp,
	      [{description, "Resa Server implements OTP Special Processes"},
	       {vsn, "1"},
	       {registered, [rsspa]},
	       {applications, [kernel, stdlib]},
	       {mod, {resa_server, [['ap.rapp.109']]}},
	       {env, []},
	       {modules, [resa_server, resa_sup]}
	      ]}.