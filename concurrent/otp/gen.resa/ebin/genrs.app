{application, genrs,
	      [{description, "GenRS Application implements OTP standard behaviour gen_server organises its processes with a Supervision Tree"},
	      {mod, {grs_app, [['gsrp.999']]}},
	      {modules, [grs_app, grs_sup, grs]},
	      {applications, [kernel, stdlib]}
	      ]}.
