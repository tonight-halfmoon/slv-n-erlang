-define(queue, <<"queue6_genrs">>).
-define(exch, <<"exch6_genrs">>).
-record(amqp_connect_args, {exch, queue}).
-record(cask4_consumer_msg, {from}).
