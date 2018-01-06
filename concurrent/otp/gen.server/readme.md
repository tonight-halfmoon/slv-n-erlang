## GenRS - Prototype on Server Sytem implements OTP Generic Server Design Principle

# Description

An Erlang/OTP application implements OTP standard behaviours [`gen_server`](http://erlang.org/doc/man/gen_server.html) and [`supervisor`](http://erlang.org/doc/man/supervisor.html). GenRS implements also behaviour `application`. GenRS application when loaded and started by Erlang application [`kernel`](http://erlang.org/doc/apps/kernel/index.html), then module `application` must have built the supervision tree and all composing pieces are processes ready in runtime.

# Experience

A lot of things have changed the way to implement the server, when compared against implemting server-client architecture in plane Erlang or utilising Special Processes of OTP Design Principles. First, no need to explictly register the pid of process `genrs` once started with [`gen_server:start_link`](http://erlang.org/doc/man/gen_server.html#start_link-3). Second, no need to define an internal function to read the mail box. Instead, the implementatoin of `gen_server` callback functions `handle_cast` and `handle_call` will be there to interpret received messages and interact upon.

I did not need to explicitely implement control source code on logging debug messages in runtime, as I had to do that when I utilised Erlang module `proc_lib` in [`RSSP`]( ../special.processes/ebin/rssp.app) when OTP Special Processes applied there. OTP standard behviour `gen_server` makes use of Erlang module `proc_lib` and when option `{debug,Dbgs}` is provided with [`gen_server:start_link`](http://erlang.org/doc/man/gen_server.html#start_link-3) then all the boiler plate is done. Of course, this does not come with a surprise, as described in Erlang [STDLIB documentation](http://erlang.org/doc/apps/stdlib/index.html).

# Future Work

## Testing

```
Utilise Erlang Common Test.
```

```
Convert it to a Distributed System: Processes on Nodes
```

```
[In Progress] ~~Simultaneous multiple client requests: Simulate the case when multiple clients sending GenRS requests at the same time~~
Currently, API function `genrs:cask_dstats`
```

## Application

0. At the application's root level
1. Execute `erl -make`
2. Execute `erl -pa ebin/`
3. Evaluate `ait:start().`

Note: when `ait:start().` evaluated twice, the second time, you get `{error,{already_started,genrs}}`, which is true.

4. finally, evaluate  `ait:stop().` And that will stop GenRS processes. And the following report is shown:

´´´
=INFO REPORT==== 4-Jan-2018::23:44:12 ===
    application: genrs
    exited: stopped
    type: temporary
    
´´´