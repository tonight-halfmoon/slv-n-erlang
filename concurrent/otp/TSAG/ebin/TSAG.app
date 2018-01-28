{application, 'TSAG',
	      [{description, "A Riak Client for GenRS"},
	       {vsn, "1"},
	       {registered, [genrs_riakc_sup]},
	       {modules, []},
	       {mod, {genrs_riakc, {"172.17.0.2", 8087}}},
	       {applications, [kernel, stdlib]},
	       {env, []}
	      ]}.