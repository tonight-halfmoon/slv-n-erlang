## GenRS - Prototype on Server Sytem implements OTP Generic Server Design Principle

# Description

An Erlang/OTP application implements OTP standard behaviours [`gen_server`](http://erlang.org/doc/man/gen_server.html) and [`supervisor`](http://erlang.org/doc/man/supervisor.html). GenRS implements also behaviour `application`. GenRS application when loaded and started by Erlang application [`kernel`](http://erlang.org/doc/apps/kernel/index.html), then module `application` must have built the supervision tree and all composing pieces are processes ready in runtime.

GenRS completes its non-blocking requests utilising [Erlang RabbitMQ Client library](http://www.rabbitmq.com/erlang-client-user-guide.html). GenRS here shows  a simple example of how to minimally implement a working integration against [RabbitMQ broker](http://www.rabbitmq.com/admin-guide.html) and see a full scenario with a client finally receives a response on a request as a AMQP queued message.

# Experience

A lot of things have changed the way to implement the server, when compared against implemting server-client architecture in plane Erlang or utilising Special Processes of OTP Design Principles. First, no need to explictly register the pid of process `genrs` once started with [`gen_server:start_link`](http://erlang.org/doc/man/gen_server.html#start_link-3). Second, no need to define an internal function to read the mail box. Instead, the implementatoin of `gen_server` callback functions `handle_cast` and `handle_call` will be there to interpret received messages and interact upon.

I did not need to explicitely implement control source code on logging debug messages in runtime, as I had to do that when I utilised Erlang module `proc_lib` in [`RSSP`]( ../special.processes/ebin/rssp.app) when OTP Special Processes applied there. OTP standard behviour `gen_server` makes use of Erlang module `proc_lib` and when option `{debug,Dbgs}` is provided with [`gen_server:start_link`](http://erlang.org/doc/man/gen_server.html#start_link-3) then all the boiler plate is done. Of course, this does not come with a surprise, as described in Erlang [STDLIB documentation](http://erlang.org/doc/apps/stdlib/index.html).

## Communicating through RabbitMQ broker via AMQP Client

To demonstrate concurrently communicating processes, I integrated the [Erlang RabbitMQ Client library](http://www.rabbitmq.com/erlang-client-user-guide.html). The example implemented, say a client requests data statistics. GenRS will process the request and submit the result as a payload with `amqp_msg` on a RabbitMQ broker queue. The client which has subscribed to the same queue as a AMQP client consumer will eventually receives the message sent by [RabbitMQ broker](http://www.rabbitmq.com/admin-guide.html). To immediately see this test of this specific topic, take a look at Test Case `dstats_test_` in module [`./integratoin.terst/test/ait.erl`](./integration.test/ait.erl).

# Testing

## Round-trip Scenario including AMQP Client and RabbitMQ Broker

First, ERL_LIBS set with the path the Erlang AMQP Client and required dependencies. See [IoT](../../iot/readme.md). Then, run RabbitMQ server with `rabbitmq-server`. Then: 

> At ../integration.test/ level

Execute `erl -make`

> At Application root level

1. Execute `ERL_LIBS=$ERL_LIBS:../genrs_client:../amqp_service_provider erl -make`
2. Execute `ERL_LIBS=$ERL_LIBS:../genrs_client:../amqp_service_provider erl -pa ebin`
3. Evaluate `ait:run_suite().`

## Application on Asynchronous Communication

It is shown with the last Test Case of module [`ait.erl`](./integration.test/ait.erl) how a GenRs client could request GenRS server and then receive the response from a RabbitMQ queue.

## Application to Concurrency

Simultaneous multiple client requests: Simulate the case when multiple clients sending GenRS requests at the same time.

```
Module `mcs` spawns many clients to test API function `genrs:cask_dstats` which invokes `gen_server:cast/2`.
```

# Future Work

```
Utilise Erlang Common Test.
```

```
Convert it to a Distributed System: Processes on Nodes
```
