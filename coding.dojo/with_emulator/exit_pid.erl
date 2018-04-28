22> exit(list_to_pid("<0.109.0>"), normal).

23> i().

24> exit(list_to_pid("<0.109.0>"), kill).
true
25> i().
eunit_server          eunit_server:main/1                      3              
<0.111.0>             math_server:loop/1                     233       10    1
                      math_server:loop/1                       3              
<0.120.0>             math_server:loop/1                     233       10    1
                      math_server:loop/1                       3              
<0.122.0>             math_server:loop/1                     233       10    1
                      math_server:loop/1                       3              
Total                                                      53800  2045929    3
                                                             260              
ok

