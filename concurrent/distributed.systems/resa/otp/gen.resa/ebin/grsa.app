{application, grsa,
	      [{description, "Resa Server implements Generic Server works as an Erlang Application with OTP Supervision Tree"},
	      {mod, {grs_app, [['gsrp.999']]}},
	      {modules, [grs_app, grs_sup, grs]},
	      {applications, [kernel, stdlib]}
	      ]}.
