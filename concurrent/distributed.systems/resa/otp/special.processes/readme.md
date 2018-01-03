# RSSP : Server System implements Special Processes OTP Design Principle, defined as a OTP Application and its primary processes are organised with a Supervision Tree

## Build up

### Make
0 - Compose an Emakefile and save it in root level
0 - Compose a .app file and save it in ebin/ dir
1 - erl -make at root level
2 - erl -pa ebin/ at root level
3 - application:load(rssp).
4 - application:start(rssp).

### Generate Boot Script
0 - Compose a .rel file and save it in ebin/ dir
1 - erl -make at root level
2 - systools:make_script("rssp", [local]). at ebin/ dir
3 - erl -boot rssp-1

## Integration Test

### Application

1 - erl -make
2 - erl -pa ebin/
3 - ait:run_suite().

### Supervision Tree and Protocols

0 - erl -make
0 - erl -pa ebin/
1 - protocol_tests:run_suite().

### Simulate Client Interaction

0 - erl -make
0 - erl -pa ebin/
1 - e2e_sim:start().
2 - e2e_sim:stats().
3 - e2e_sim:alloc().
4 - e2e_sim:stats().
5 - e2e_sim:freeup().
6 - e2e_sim:stats().
7 - e2e_sim:stop().
* - e2e_sim:run_suite().

! - check surefire report.
