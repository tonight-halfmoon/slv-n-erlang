Erlang/OTP 19 [erts-8.3.5.5] [source] [64-bit] [smp:8:8] [async-threads:10] [hipe] [kernel-poll:false] [dtrace]

Eshell V8.3.5.5  (abort with ^G)
1> code:is_loaded(fulqualif).
false
2> c(fulqualif).
{ok,fulqualif}
3> code:is_loaded(fulqualif).
{file,"/usr/home/rosemary/erlang/slv-n-erlang/elementary/software-upgrade/fulqualif.beam"}
4> i().
Pid                   Initial Call                          Heap     Reds Msgs
Registered            Current Function                     Stack              
<0.0.0>               otp_ring0:start/2                      233      648    0
init                  init:loop/1                              2              
<0.1.0>               erts_code_purger:start/0               233        8    0
erts_code_purger      erts_code_purger:loop/0                  3              
<0.4.0>               erlang:apply/2                        4185   515351    0
erl_prim_loader       erl_prim_loader:loop/3                   5              
<0.30.0>              gen_event:init_it/6                    610      204    0
error_logger          undefined                                2              
<0.31.0>              erlang:apply/2                        1598      360    0
application_controlle undefined                                0              
<0.33.0>              application_master:init/4              233       66    0
                      undefined                                2              
<0.34.0>              application_master:start_it/4          233       53    0
                      undefined                                0              
<0.35.0>              supervisor:kernel/1                    610     1553    0
kernel_sup            undefined                                2              
<0.36.0>              erlang:apply/2                       46422 12444993    0
code_server           undefined                                0              
<0.38.0>              rpc:init/1                             233       19    0
rex                   gen_server:loop/6                        9              
<0.39.0>              global:init/1                          233       38    0
global_name_server    gen_server:loop/6                        9              
<0.40.0>              erlang:apply/2                         233       15    0
                      undefined                                0              
<0.41.0>              erlang:apply/2                         233        5    0
                      undefined                                0              
<0.42.0>              inet_db:init/1                         233      152    0
inet_db               gen_server:loop/6                        9              
<0.43.0>              global_group:init/1                    233       43    0
global_group          gen_server:loop/6                        9              
<0.44.0>              file_server:init/1                     376      536    0
file_server_2         undefined                                2              
<0.45.0>              supervisor_bridge:standard_error/      233       31    0
standard_error_sup    gen_server:loop/6                        9              
<0.46.0>              erlang:apply/2                         233        8    0
standard_error        undefined                                0              
<0.47.0>              supervisor_bridge:user_sup/1           233       47    0
                      undefined                                2              
<0.48.0>              user_drv:server/2                     2586     3915    0
user_drv              undefined                                0              
<0.49.0>              group:server/3                         233       23    0
user                  undefined                                0              
<0.50.0>              group:server/3                        2586    10314    0
                      undefined                                0              
<0.51.0>              erlang:apply/2                        4185     9492    0
                      undefined                                0              
<0.52.0>              kernel_config:init/1                   233      227    0
                      undefined                                0              
<0.53.0>              supervisor:kernel/1                    233       31    0
kernel_safe_sup       undefined                                0              
<0.57.0>              erlang:apply/2                         233    16839    0
                      erl_eval:do_apply/5                      6              
Total                                                      67119 12500991    0
                                                              71              
ok
5> code:soft_purge(fulqualif).
true
6> fulqualif:start().
true
7> i().
Pid                   Initial Call                          Heap     Reds Msgs
Registered            Current Function                     Stack              
<0.0.0>               otp_ring0:start/2                      233      648    0
init                  init:loop/1                              2              
<0.1.0>               erts_code_purger:start/0               233       12    0
erts_code_purger      erts_code_purger:loop/0                  3              
<0.4.0>               erlang:apply/2                        4185   515351    0
erl_prim_loader       erl_prim_loader:loop/3                   5              
<0.30.0>              gen_event:init_it/6                    610      204    0
error_logger          undefined                                2              
<0.31.0>              erlang:apply/2                        1598      360    0
application_controlle undefined                                0              
<0.33.0>              application_master:init/4              233       66    0
                      undefined                                2              
<0.34.0>              application_master:start_it/4          233       53    0
                      undefined                                0              
<0.35.0>              supervisor:kernel/1                    610     1553    0
kernel_sup            undefined                                2              
<0.36.0>              erlang:apply/2                       46422 12444994    0
code_server           undefined                                0              
<0.38.0>              rpc:init/1                             233       19    0
rex                   gen_server:loop/6                        9              
<0.39.0>              global:init/1                          233       38    0
global_name_server    gen_server:loop/6                        9              
<0.40.0>              erlang:apply/2                         233       15    0
                      undefined                                0              
<0.41.0>              erlang:apply/2                         233        5    0
                      undefined                                0              
<0.42.0>              inet_db:init/1                         233      152    0
inet_db               gen_server:loop/6                        9              
<0.43.0>              global_group:init/1                    233       43    0
global_group          gen_server:loop/6                        9              
<0.44.0>              file_server:init/1                     376      536    0
file_server_2         undefined                                2              
<0.45.0>              supervisor_bridge:standard_error/      233       31    0
standard_error_sup    gen_server:loop/6                        9              
<0.46.0>              erlang:apply/2                         233        8    0
standard_error        undefined                                0              
<0.47.0>              supervisor_bridge:user_sup/1           233       47    0
                      undefined                                2              
<0.48.0>              user_drv:server/2                     1598     7291    0
user_drv              undefined                                0              
<0.49.0>              group:server/3                         233       23    0
user                  undefined                                0              
<0.50.0>              group:server/3                        2586    22697    0
                      undefined                                0              
<0.51.0>              erlang:apply/2                        4185     9684    0
                      undefined                                0              
<0.52.0>              kernel_config:init/1                   233      227    0
                      undefined                                0              
<0.53.0>              supervisor:kernel/1                    233       31    0
kernel_safe_sup       undefined                                0              
<0.57.0>              erlang:apply/2                         987    32697    0
                      erl_eval:do_apply/5                      6              
<0.68.0>              fulqualif:init/0                       233        3    0
'process-and-fully-qu fulqualif:loop/0                         2              
Total                                                      67118 12504173    0
                                                              73              
ok
8> code:is_loaded(fulqualif).
{file,"/usr/home/rosemary/erlang/slv-n-erlang/elementary/software-upgrade/fulqualif.beam"}
9> code:soft_purge(fulqualif).
true
10> i().

Pid                   Initial Call                          Heap     Reds Msgs
Registered            Current Function                     Stack              
<0.0.0>               otp_ring0:start/2                      233      648    0
init                  init:loop/1                              2              
<0.1.0>               erts_code_purger:start/0               233       16    0
erts_code_purger      erts_code_purger:loop/0                  3              
<0.4.0>               erlang:apply/2                        4185   515351    0
erl_prim_loader       erl_prim_loader:loop/3                   5              
<0.30.0>              gen_event:init_it/6                    610      204    0
error_logger          undefined                                2              
<0.31.0>              erlang:apply/2                        1598      360    0
application_controlle undefined                                0              
<0.33.0>              application_master:init/4              233       66    0
                      undefined                                2              
<0.34.0>              application_master:start_it/4          233       53    0
                      undefined                                0              
<0.35.0>              supervisor:kernel/1                    610     1553    0
kernel_sup            undefined                                2              
<0.36.0>              erlang:apply/2                       46422 12444995    0
code_server           undefined                                0              
<0.38.0>              rpc:init/1                             233       19    0
rex                   gen_server:loop/6                        9              
<0.39.0>              global:init/1                          233       38    0
global_name_server    gen_server:loop/6                        9              
<0.40.0>              erlang:apply/2                         233       15    0
                      undefined                                0              
<0.41.0>              erlang:apply/2                         233        5    0
                      undefined                                0              
<0.42.0>              inet_db:init/1                         233      152    0
inet_db               gen_server:loop/6                        9              
<0.43.0>              global_group:init/1                    233       43    0
global_group          gen_server:loop/6                        9              
<0.44.0>              file_server:init/1                     376      536    0
file_server_2         undefined                                2              
<0.45.0>              supervisor_bridge:standard_error/      233       31    0
standard_error_sup    gen_server:loop/6                        9              
<0.46.0>              erlang:apply/2                         233        8    0
standard_error        undefined                                0              
<0.47.0>              supervisor_bridge:user_sup/1           233       47    0
                      undefined                                2              
<0.48.0>              user_drv:server/2                      987    10866    0
user_drv              undefined                                0              
<0.49.0>              group:server/3                         233       23    0
user                  undefined                                0              
<0.50.0>              group:server/3                        2586    35427    0
                      undefined                                0              
<0.51.0>              erlang:apply/2                        4185     9878    0
                      undefined                                0              
<0.52.0>              kernel_config:init/1                   233      227    0
                      undefined                                0              
<0.53.0>              supervisor:kernel/1                    233       31    0
kernel_safe_sup       undefined                                0              
<0.57.0>              erlang:apply/2                         987    49117    0
                      erl_eval:do_apply/5                      6              
<0.68.0>              fulqualif:init/0                       233        3    0
'process-and-fully-qu fulqualif:loop/0                         2              
Total                                                      66507 12507467    0
                                                              73              
ok
11> 
