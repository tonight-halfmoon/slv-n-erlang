Erlang/OTP 19 [erts-8.3.5.5] [source] [64-bit] [smp:8:8] [async-threads:10] [hipe] [kernel-poll:false] [dtrace]

Eshell V8.3.5.5  (abort with ^G)
1> make:all([load]).
Recompile: db
Recompile: db_server
up_to_date
2> db:module_info().
[{module,db},
 {exports,[{new,0},
           {write,3},
           {read,2},
           {delete,2},
           {destroy,1},
           {module_info,0},
           {module_info,1}]},
 {attributes,[{vsn,[1.1]}]},
 {compile,[{options,[]},
           {version,"7.0.4.1"},
           {source,"/usr/home/rosemary/erlang/slv-n-erlang/elementary/software-upgrade/database/db.erl"}]},
 {native,false},
 {md5,<<247,254,50,145,135,241,141,3,151,42,225,212,229,
        13,207,17>>}]
3> db_server:start().
true
4> db_server:write(francesco, san_francisco).
{write,francesco,san_francisco}
5> db_server:write(alison, london).
{write,alison,london}
6> db_server:read(alison).
{ok,london}
7> db_server:read(martin).
{error,instance}
8> pwd().
/usr/home/rosemary/erlang/slv-n-erlang/elementary/software-upgrade/database
ok
9> c("/usr/home/rosemary/erlang/slv-n-erlang/elementary/software-upgrade/patches/db.erl).
9> c("/usr/home/rosemary/erlang/slv-n-erlang/elementary/software-upgrade/patches/db.erl").
9> c("/usr/home/rosemary/erlang/slv-n-erlang/elementary/software-upgrade/patches/db.erl).
* 2: syntax error before: '.'
9> c("/usr/home/rosemary/erlang/slv-n-erlang/elementary/software-upgrade/patches/db.erl").
{ok,db}
10> code:add_patha("/usr/home/rosemary/erlang/slv-n-erlang/elementary/software-upgrade/patches").
true
11> code:load_file(db).
{error,not_purged}
12> 
=ERROR REPORT==== 1-Sep-2018::15:09:14 ===
Loading of /usr/home/rosemary/erlang/slv-n-erlang/elementary/software-upgrade/database/db.beam failed: not_purged

12> code:soft_purge(db).
true
13> code:load_file(db).
{module,db}
14> db:module_info().
[{module,db},
 {exports,[{convert,2},
           {new,0},
           {write,3},
           {read,2},
           {delete,2},
           {destroy,1},
           {module_info,0},
           {module_info,1}]},
 {attributes,[{vsn,[1.2]}]},
 {compile,[{options,[]},
           {version,"7.0.4.1"},
           {source,"/usr/home/rosemary/erlang/slv-n-erlang/elementary/software-upgrade/patches/db.erl"}]},
 {native,false},
 {md5,<<175,196,13,22,240,18,28,147,168,163,93,238,135,
        246,18,148>>}]
15> db_server:upgrade(dict).
{upgrade,dict}
16> db_server:write(martin, cairo).
{write,martin,cairo}
17> db_server:read(francesco).
{ok,san_francisco}
18> db_server:read(martin).
{ok,cairo}
19> 