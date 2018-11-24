-record(usr, {msisdn,			%int()
		id,			%term()
		status = enabled,	%atom()
		plan,			%atom(), prepay | postpay
		services = []}).	%[atom()]. service flag list
