Erlang/OTP 19 [erts-8.3.5.5] [source] [64-bit] [smp:8:8] [async-threads:10] [hipe] [kernel-poll:false] [dtrace]

Eshell V8.3.5.5  (abort with ^G)
1> c(record_bif).
record_bif.erl: no such file or directory
error
2> ls().
emulator.erl        emulator.erl~       person.beam         
person.erl          record_bifs.erl     
ok
3> c(record_bifs).
record_bifs.erl:8: function is_record/1 undefined
record_bifs.erl:5: Warning: record person is unused
error
4> c(record_bifs).
{ok,record_bifs}
5> record_bifs:test(asd).
Args is not a record. Args value asd
ok
6> rr(record_bifs).
[person]
7> record_bifs:test(#person{name="Ahmad"}).
Args is a record value {person,"Ahmad",0}
ok
8> 
