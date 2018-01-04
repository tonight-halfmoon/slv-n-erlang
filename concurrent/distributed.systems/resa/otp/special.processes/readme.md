# RSSP - Prototype on Server System implements OTP Special Processes Design Principle

# Description

An Erlang/OTP application defines a server that manages resources. The server is composed of independent modules each handle piece of work. In runtime they are  concurrent communicating processes which are organised with a Supervision Tree


# Experience

I learned how to define a OTP Supervision Tree. `resa_sup` supervises two children: the application `resa_server` and another supervisor called `sm_sup`. Service Manager supervisor `sm_sup` is responsible of service providers like process `sp`. Once the Erlang kernel `application ` loads and starts up RSSP application the supervision tree is built and all the necessary processes that make RSSP fully functioning in runtime will be ready. I see how `application` cleans everything when `stop` and `unload` are invoked on RSSP. I learned how to use Erlang STDLIB `proc_lib` to handle debugging and statistics about RSSP.

## Development Time

### Make

0. Compose file `Emakefile` and save it at the application's root level
0. Compose file `.app` and save it in `ebin/`

### Generate Boot Script

0. Compose file `.rel` and save it in `ebin/`. Say it is named `rssp-1`
1. Execute `erl -make` at the application's root level
2. change to `ebin/`
3. Start Erlang shell
2. Run Erlang SASL's module `systools`: `systools:make_script("rssp", [local]).`
3. Run command `erl -boot rssp-1`

## Build up

1. Change to the application's root level
2. Execute `erl -make`
2. Execute `erl -pa ebin/`

## Run

0. Start Erlang shell
1. `application:load(rssp).`
2. `application:start(rssp).`
3. `application:stop(rssp).`
4. `application:unload(rssp).`

# Integration Test

## Application

0. At the application's root level
1. Execute `erl -make`
2. Execute `erl -pa ebin/`
3. Evaluate `ait:run_suite().`

## Supervision Tree and Protocols

0. At the application's root level
0. Exeucte `erl -make`
0. Execute `erl -pa ebin/`
1. Evaluate `protocol_tests:run_suite().`

## Simulate Client Interaction

0. At the application's root level
0. Execute `erl -make`
0. Execute `erl -pa ebin/`
1. Evaluaet `e2e_sim:start().`
2. `e2e_sim:stats().`
3. `e2e_sim:alloc().`
4. `e2e_sim:stats().`
5. `e2e_sim:freeup().`
6. `e2e_sim:stats().`
7. `e2e_sim:stop().`
9. `e2e_sim:run_suite().`

10. check surefire report.
