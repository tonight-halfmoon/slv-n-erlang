{application, res_app,
	      [{description, "Resource Allocator"},
	      	{vsn, "0.1"},
		  {registered, [res_allocator]},
		   {applications, [kernel, stdlib]},
		    {mod, {res_app, []}},
		    {env, []},
		  {modules, [res_app, res_allocator,res_handler, stats_provider]}
		  ]}.