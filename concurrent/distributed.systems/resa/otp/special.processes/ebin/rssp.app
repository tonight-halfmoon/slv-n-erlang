{application, rssp,
	      [{description, "Resa Server implements OTP Special Processes"},
	       {vsn, "1"},
	       {registered, [rsspa]},
	       {applications, [kernel, stdlib]},
	       {mod, {resas_app, [['ap.rapp.109']]}},
	       {env, []},
	       {modules, [resa_server, resas_app, resa_sup, rh]}
	      ]}.