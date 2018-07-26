## Asynchronous Client API against Math Server - Connected, Linked Clients

### In this example we demonstrate the capability for many clients to connect to the server. We also show how a server can be notified once a client's process exists. 

´´´
For example, suppose clients are comsuming/locking resources in the server. In order to freeup the allocated resource by a client once the client terminates, then a link has been set between the server and each connected client at connect time. For demonstration purposes, we limit the server to have only two resource allocations. Therefore, only two clients can connect to the example server. Absolutely, one could play around wit hthis example and make it capable to handle more clients. Note, this limitation has nothing to do with Erlang Runtime System. This is only for learning purposes.
´´´

$ erl -pa ebin/ Erlang/OTP 20 [erts-9.3.3] [source] [64-bit] [smp:8:8] [ds:8:8:10] [async-threads:10] [hipe] [kernel-poll:false] [dtrace]

Eshell V9.3.3 (abort with ^G)

1> server:start(). {ok,noreply}

2> client:start(cp1). {ok,<0.64.0>}

3> client:connect(cp1). Client connect. Successfully connected. {ok,noreply}

4> client:start(cp2). {ok,<0.67.0>}

5> client:connect(cp2). Client connect. Successfully connected. {ok,noreply}

6> client:start(cp3). {ok,<0.70.0>}

7> client:connect(cp3). Client cannot connect for 'no enough resources'. Client process terminates... {ok,noreply}

8> client:disconnect(cp2). Client is disconnected. {ok,noreply} client's process exiting...

9> server:check_resources(). {reply,math_server_jul18, [{'0x41a',<0.64.0>},{'0xf23',<0.67.0>}]}

10> q(). ok

11>
