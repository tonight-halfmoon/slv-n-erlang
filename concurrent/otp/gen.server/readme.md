## GenRS - Prototype on Server Sytem implements OTP Generic Server Design Principle

# Description

An Erlang/OTP application implements OTP standard behaviours `gen_server` and `supervisor`. GenRS implements also behaviour `application`. GenRS application when loaded and started by Erlang `kernel` module `application` must have built the supervision tree and all composing pieces are processes ready in runtime.

# Experience

A lot of things have changed the way to implement the server. First, no need to explictly register the pid of process `genrs` once started with `gen_server:start_link`. Second, no need to define an internal function to read the mail box. Instead, the implementatoin of `gen_server` callback functions `handle_cast` and `handle_call` will be there to interpret received messages and interact upon.

# Future Work


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