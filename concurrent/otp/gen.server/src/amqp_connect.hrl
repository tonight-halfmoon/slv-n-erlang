-define(conn, rabbitconn).
-define(ch, ch4_genrs).
-define(queue, <<"queue4_dstats">>).
-define(exch, <<"exch4_genrs">>).
-record(pub, {payload}).
-record(cask_consumer_msg, {from}).
-define(amqp_consumer, amqp_consumer_sp).
-record(amqp_connect, {exch, queue, ch, conn}).
-record(ampq_connect_stopped, {event, reason, from}).
