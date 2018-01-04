{application, genrs,
	      [{description, "GenRS Application implements OTP standard behaviour gen_server organises its processes with a Supervision Tree"},
	      {vsn, "1"},
	      {registered, [reg_genrs_app]},
	      {mod, {genrs_app, [['rs.gsrp.999']]}},
	      {modules, [genrs_app, genrs_sup, genrs]},
	      {applications, [kernel, stdlib]},
	      {env, []}	      
	      ]}.
