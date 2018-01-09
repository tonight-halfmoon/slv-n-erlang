-module(amqp_pub).

-export([send/2]).

-include("amqp_connect.hrl").

-include_lib("amqp_client/include/amqp_client.hrl").

send(#amqp_connect{exch=Exch, queue=Q, ch=Ch_proc_name, conn=_Conn_proc_name}, Payload) ->
    Publish = #'basic.publish'{exchange = Exch, routing_key = Q},
    Props = #'P_basic'{delivery_mode = 2},
    amqp_channel:cast(whereis(Ch_proc_name), Publish, #amqp_msg{props = Props, payload = Payload}).
