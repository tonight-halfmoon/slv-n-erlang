rosemary@SCUBA:[227]~/..ring/naiive$ ls
ebin  Emakefile  history.erl  src  test
rosemary@SCUBA:[227]~/..ring/naiive$ vim history2.erl 
rosemary@SCUBA:[226]~/..ring/naiive$ git status
On branch master
Your branch is up to date with 'origin/master'.

Changes not staged for commit:
  (use "git add <file>..." to update what will be committed)
  (use "git checkout -- <file>..." to discard changes in working directory)

	modified:   src/ring.erl
	modified:   test/ring_tests.erl

Untracked files:
  (use "git add <file>..." to include in what will be committed)

	../../../coding.dojo/server_with_state.erl
	history2.erl

no changes added to commit (use "git add" and/or "git commit -a")
rosemary@SCUBA:[226]~/..ring/naiive$ erl -make
rosemary@SCUBA:[226]~/..ring/naiive$ erl -pa ebin/
Erlang/OTP 20 [erts-9.3] [source] [64-bit] [smp:8:8] [ds:8:8:10] [async-threads:10] [hipe] [kernel-poll:false]

Eshell V9.3  (abort with ^G)
1> eunit:test(ring).
  All 6 tests passed.
ok
2> q().
ok
3> rosemary@SCUBA:[226]~/..ring/naiive$ cd ..
rosemary@SCUBA:[226]~/..urrent/ring$ cd ..
rosemary@SCUBA:[226]~/../concurrent$ got status
bash: got: command not found
rosemary@SCUBA:[226]~/../concurrent$ git status
On branch master
Your branch is up to date with 'origin/master'.

Changes not staged for commit:
  (use "git add <file>..." to update what will be committed)
  (use "git checkout -- <file>..." to discard changes in working directory)

	modified:   ring/naiive/src/ring.erl
	modified:   ring/naiive/test/ring_tests.erl

Untracked files:
  (use "git add <file>..." to include in what will be committed)

	../coding.dojo/server_with_state.erl
	ring/naiive/history2.erl

no changes added to commit (use "git add" and/or "git commit -a")
rosemary@SCUBA:[226]~/../concurrent$ git add ring
rosemary@SCUBA:[226]~/../concurrent$ git status
On branch master
Your branch is up to date with 'origin/master'.

Changes to be committed:
  (use "git reset HEAD <file>..." to unstage)

	new file:   ring/naiive/history2.erl
	modified:   ring/naiive/src/ring.erl
	modified:   ring/naiive/test/ring_tests.erl

Untracked files:
  (use "git add <file>..." to include in what will be committed)

	../coding.dojo/server_with_state.erl

rosemary@SCUBA:[226]~/../concurrent$ git commit -m "Use Case : terminate_gracefully_when_ring_nodes_receive_a_quit_message_test"
[master 2f3bef3] Use Case : terminate_gracefully_when_ring_nodes_receive_a_quit_message_test
 3 files changed, 441 insertions(+), 22 deletions(-)
 create mode 100644 concurrent/ring/naiive/history2.erl
rosemary@SCUBA:[227]~/../concurrent$ git status
On branch master
Your branch is ahead of 'origin/master' by 1 commit.
  (use "git push" to publish your local commits)

Untracked files:
  (use "git add <file>..." to include in what will be committed)

	../coding.dojo/server_with_state.erl

nothing added to commit but untracked files present (use "git add" to track)
rosemary@SCUBA:[227]~/../concurrent$ git status
On branch master
Your branch is ahead of 'origin/master' by 1 commit.
  (use "git push" to publish your local commits)

Untracked files:
  (use "git add <file>..." to include in what will be committed)

	../coding.dojo/server_with_state.erl

nothing added to commit but untracked files present (use "git add" to track)
rosemary@SCUBA:[227]~/../concurrent$ git status
On branch master
Your branch is ahead of 'origin/master' by 1 commit.
  (use "git push" to publish your local commits)

Untracked files:
  (use "git add <file>..." to include in what will be committed)

	../coding.dojo/server_with_state.erl

nothing added to commit but untracked files present (use "git add" to track)
rosemary@SCUBA:[224]~/../concurrent$ cd ring/
rosemary@SCUBA:[224]~/..urrent/ring$ erl -make
rosemary@SCUBA:[224]~/..urrent/ring$ cd naiive/
rosemary@SCUBA:[224]~/..ring/naiive$ erl -make
Recompile: src/ring
rosemary@SCUBA:[224]~/..ring/naiive$ erl -pa ebin/
Erlang/OTP 20 [erts-9.3] [source] [64-bit] [smp:8:8] [ds:8:8:10] [async-threads:10] [hipe] [kernel-poll:false]

Eshell V9.3  (abort with ^G)
1> eunit:test(ring).
  All 6 tests passed.
ok
2> q().
ok
3> rosemary@SCUBA:[224]~/..ring/naiive$ git status
On branch master
Your branch is ahead of 'origin/master' by 1 commit.
  (use "git push" to publish your local commits)

Untracked files:
  (use "git add <file>..." to include in what will be committed)

	../../../coding.dojo/server_with_state.erl

nothing added to commit but untracked files present (use "git add" to track)
rosemary@SCUBA:[224]~/..ring/naiive$ git pull
Already up to date.
rosemary@SCUBA:[229]~/..ring/naiive$ git push
Username for 'https://github.com': tonight-halfmoon
Password for 'https://tonight-halfmoon@github.com': 
Counting objects: 10, done.
Delta compression using up to 8 threads.
Compressing objects: 100% (7/7), done.
Writing objects: 100% (10/10), 2.88 KiB | 1.44 MiB/s, done.
Total 10 (delta 4), reused 0 (delta 0)
remote: Resolving deltas: 100% (4/4), completed with 4 local objects.
To https://github.com/tonight-halfmoon/slv-n-erlang.git
   8645647..2f3bef3  master -> master
rosemary@SCUBA:[230]~/..ring/naiive$ git status
On branch master
Your branch is up to date with 'origin/master'.

Untracked files:
  (use "git add <file>..." to include in what will be committed)

	../../../coding.dojo/server_with_state.erl

nothing added to commit but untracked files present (use "git add" to track)
rosemary@SCUBA:[230]~/..ring/naiive$ ls
ebin  Emakefile  history2.erl  history.erl  src  test
rosemary@SCUBA:[349]~/..ring/naiive$ cd ..
rosemary@SCUBA:[349]~/..urrent/ring$ ls
naiive
rosemary@SCUBA:[349]~/..urrent/ring$ mv naiive/ central-process
rosemary@SCUBA:[349]~/..urrent/ring$ ls
central-process
rosemary@SCUBA:[349]~/..urrent/ring$ mkdir parent-of-next
rosemary@SCUBA:[349]~/..urrent/ring$ ls
central-process  parent-of-next
rosemary@SCUBA:[349]~/..urrent/ring$ rm -r parent-of-next/
rosemary@SCUBA:[349]~/..urrent/ring$ cp -r central-process/ chain-hook
rosemary@SCUBA:[349]~/..urrent/ring$ cd chain-hook/
rosemary@SCUBA:[349]~/../chain-hook$ ls
ebin  Emakefile  history2.erl  history.erl  src  test
rosemary@SCUBA:[349]~/../chain-hook$ rm history*.erl
rosemary@SCUBA:[349]~/../chain-hook$ ls
ebin  Emakefile  src  test
rosemary@SCUBA:[349]~/../chain-hook$ rm src/*
rosemary@SCUBA:[349]~/../chain-hook$ ls
ebin  Emakefile  src  test
rosemary@SCUBA:[349]~/../chain-hook$ cd src/
rosemary@SCUBA:[349]~/..in-hook/src$ ls
rosemary@SCUBA:[349]~/..in-hook/src$ cd ,,
bash: cd: ,,: No such file or directory
rosemary@SCUBA:[349]~/..in-hook/src$ cd ..
rosemary@SCUBA:[349]~/../chain-hook$ rm ebin/*
rosemary@SCUBA:[349]~/../chain-hook$ rm test/*
rosemary@SCUBA:[349]~/../chain-hook$ ls
ebin  Emakefile  src  test
rosemary@SCUBA:[349]~/../chain-hook$ erl -make
rosemary@SCUBA:[349]~/../chain-hook$ erl -pa ebin/
Erlang/OTP 20 [erts-9.3] [source] [64-bit] [smp:8:8] [ds:8:8:10] [async-threads:10] [hipe] [kernel-poll:false]

Eshell V9.3  (abort with ^G)
1> eunit:test(ring).
undefined
*** test module not found ***
**ring

=======================================================
  Failed: 0.  Skipped: 0.  Passed: 0.
One or more tests were cancelled.
error
2> q().
ok
3> rosemary@SCUBA:[349]~/../chain-hook$ ls
ebin  Emakefile  src  test
rosemary@SCUBA:[349]~/../chain-hook$ emacs test/ring_tests.erl
rosemary@SCUBA:[349]~/../chain-hook$ cd ..
rosemary@SCUBA:[349]~/..urrent/ring$ git status
On branch master
Your branch is up to date with 'origin/master'.

Changes not staged for commit:
  (use "git add/rm <file>..." to update what will be committed)
  (use "git checkout -- <file>..." to discard changes in working directory)

	deleted:    naiive/Emakefile
	deleted:    naiive/history.erl
	deleted:    naiive/history2.erl
	deleted:    naiive/src/ring.erl
	deleted:    naiive/test/ring_tests.erl

Untracked files:
  (use "git add <file>..." to include in what will be committed)

	../../coding.dojo/server_with_state.erl
	central-process/
	chain-hook/

no changes added to commit (use "git add" and/or "git commit -a")
rosemary@SCUBA:[349]~/..urrent/ring$ git add central-process/
rosemary@SCUBA:[349]~/..urrent/ring$ git status
On branch master
Your branch is up to date with 'origin/master'.

Changes to be committed:
  (use "git reset HEAD <file>..." to unstage)

	new file:   central-process/Emakefile
	new file:   central-process/history.erl
	new file:   central-process/history2.erl
	new file:   central-process/src/ring.erl
	new file:   central-process/test/ring_tests.erl

Changes not staged for commit:
  (use "git add/rm <file>..." to update what will be committed)
  (use "git checkout -- <file>..." to discard changes in working directory)

	deleted:    naiive/Emakefile
	deleted:    naiive/history.erl
	deleted:    naiive/history2.erl
	deleted:    naiive/src/ring.erl
	deleted:    naiive/test/ring_tests.erl

Untracked files:
  (use "git add <file>..." to include in what will be committed)

	../../coding.dojo/server_with_state.erl
	chain-hook/

rosemary@SCUBA:[349]~/..urrent/ring$ git add naiive/
rosemary@SCUBA:[349]~/..urrent/ring$ git status
On branch master
Your branch is up to date with 'origin/master'.

Changes to be committed:
  (use "git reset HEAD <file>..." to unstage)

	renamed:    naiive/Emakefile -> central-process/Emakefile
	renamed:    naiive/history.erl -> central-process/history.erl
	renamed:    naiive/history2.erl -> central-process/history2.erl
	renamed:    naiive/src/ring.erl -> central-process/src/ring.erl
	renamed:    naiive/test/ring_tests.erl -> central-process/test/ring_tests.erl

Untracked files:
  (use "git add <file>..." to include in what will be committed)

	../../coding.dojo/server_with_state.erl
	chain-hook/

rosemary@SCUBA:[349]~/..urrent/ring$ git commit -m "Ring - Central Process Strategy"
[master e5a2550] Ring - Central Process Strategy
 5 files changed, 0 insertions(+), 0 deletions(-)
 rename concurrent/ring/{naiive => central-process}/Emakefile (100%)
 rename concurrent/ring/{naiive => central-process}/history.erl (100%)
 rename concurrent/ring/{naiive => central-process}/history2.erl (100%)
 rename concurrent/ring/{naiive => central-process}/src/ring.erl (100%)
 rename concurrent/ring/{naiive => central-process}/test/ring_tests.erl (100%)
rosemary@SCUBA:[348]~/..urrent/ring$ git status
On branch master
Your branch is ahead of 'origin/master' by 1 commit.
  (use "git push" to publish your local commits)

Untracked files:
  (use "git add <file>..." to include in what will be committed)

	../../coding.dojo/server_with_state.erl
	chain-hook/

nothing added to commit but untracked files present (use "git add" to track)
rosemary@SCUBA:[348]~/..urrent/ring$ cd chain-hook/
rosemary@SCUBA:[348]~/../chain-hook$ ls
ebin  Emakefile  src  test
rosemary@SCUBA:[348]~/../chain-hook$ emacs test/ring_tests.erl 
^Z
[1]+  Stopped                 emacs test/ring_tests.erl
rosemary@SCUBA:[223]~/../chain-hook$ bg
[1]+ emacs test/ring_tests.erl &
rosemary@SCUBA:[223]~/../chain-hook$ erl -make
Recompile: test/ring_tests
test/ring_tests.erl:5: function spawn_node/0 undefined
test/ring_tests.erl:6: function spawn_next/1 undefined
test/ring_tests.erl:8: function parent_of/1 undefined
rosemary@SCUBA:[223]~/../chain-hook$ erl -make
Recompile: test/ring_tests
rosemary@SCUBA:[224]~/../chain-hook$ erl -pa ebin/
Erlang/OTP 20 [erts-9.3] [source] [64-bit] [smp:8:8] [ds:8:8:10] [async-threads:10] [hipe] [kernel-poll:false]

Eshell V9.3  (abort with ^G)
1> eunit:test(ring).
undefined
*** test module not found ***
**ring

=======================================================
  Failed: 0.  Skipped: 0.  Passed: 0.
One or more tests were cancelled.
error
2> eunit:test(ring_tests).
ring_tests: new_node_and_spawn_next_node_in_the_ring_test (module 'ring_tests')...*failed*
in function ring:spawn_node/0
  called as spawn_node()
in call from ring_tests:new_node_and_spawn_next_node_in_the_ring_test/0 (test/ring_tests.erl, line 6)
in call from ring_tests:new_node_and_spawn_next_node_in_the_ring_test/0
**error:undef
  output:<<"">>

=======================================================
  Failed: 1.  Skipped: 0.  Passed: 0.
error
3> c(ring).
{error,non_existing}
4> q().
ok
5> rosemary@SCUBA:[230]~/../chain-hook$ erl -make
Recompile: src/ring
src/ring.erl:2: function spawn_node/0 undefined
src/ring.erl:4: Warning: function spaw_node/0 is unused
rosemary@SCUBA:[230]~/../chain-hook$ ls src/
ring.erl
rosemary@SCUBA:[230]~/../chain-hook$ ls src/
ring.erl  ring.erl~
rosemary@SCUBA:[230]~/../chain-hook$ erl -make
Recompile: src/ring
Recompile: test/ring_tests
rosemary@SCUBA:[230]~/../chain-hook$ erl -pa ebin/
Erlang/OTP 20 [erts-9.3] [source] [64-bit] [smp:8:8] [ds:8:8:10] [async-threads:10] [hipe] [kernel-poll:false]

Eshell V9.3  (abort with ^G)
1> eunit:test(ring).
  Test passed.
ok
2> c(ring_tests).
Recompiling /home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/test/ring_tests.erl
{ok,ring_tests}
3> eunit:test(ring).
ring_tests: new_node_and_spawn_next_node_in_the_ring_test (module 'ring_tests')...*failed*
in function erlang:is_process_alive/1
  called as is_process_alive(ok)
in call from ring_tests:'-new_node_and_spawn_next_node_in_the_ring_test/0-fun-0-'/1 (/home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/test/ring_tests.erl, line 10)
**error:badarg 
  output:<<"">>

=======================================================
  Failed: 1.  Skipped: 0.  Passed: 0.
error
4> c(ring).
Recompiling /home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/src/ring.erl
{ok,ring}
5> c(ring_tests).
Recompiling /home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/test/ring_tests.erl
{ok,ring_tests}
6> eunit:test(ring).
ring_tests: new_node_and_spawn_next_node_in_the_ring_test (module 'ring_tests')...*failed*
in function ring_tests:new_node_and_spawn_next_node_in_the_ring_test/0 (/home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/test/ring_tests.erl, line 9)
**error:{badmatch,ok}
  output:<<"">>

=======================================================
  Failed: 1.  Skipped: 0.  Passed: 0.
error
7> c(ring).
Recompiling /home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/src/ring.erl
/home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/src/ring.erl:13: Warning: function spawn_node/1 is unused
/home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/src/ring.erl:16: Warning: function init/1 is unused
/home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/src/ring.erl:19: Warning: function loop/1 is unused
/home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/src/ring.erl:21: Warning: variable 'M' is unused
{ok,ring}
8> c(ring).
Recompiling /home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/src/ring.erl
/home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/src/ring.erl:16: Warning: function init/1 is unused
/home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/src/ring.erl:19: Warning: function loop/1 is unused
/home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/src/ring.erl:21: Warning: variable 'M' is unused
{ok,ring}
9> c(ring).
Recompiling /home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/src/ring.erl
/home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/src/ring.erl:16: Warning: function init/1 is unused
/home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/src/ring.erl:19: Warning: function loop/1 is unused
{ok,ring}
10> c(ring).
Recompiling /home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/src/ring.erl
{ok,ring}
11> eunit:test(ring).
ring_tests: new_node_and_spawn_next_node_in_the_ring_test (module 'ring_tests')...*failed*
in function ring_tests:new_node_and_spawn_next_node_in_the_ring_test/0 (/home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/test/ring_tests.erl, line 9)
**error:{badmatch,ok}
  output:<<"">>

=======================================================
  Failed: 1.  Skipped: 0.  Passed: 0.
error
12> c(ring).         
Recompiling /home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/src/ring.erl
{ok,ring}
13> eunit:test(ring).

=ERROR REPORT==== 2-May-2018::20:16:45 ===
Error in process <0.161.0> with exit value:
{badarg,[{ring,loop,1,
               [{file,"/home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/src/ring.erl"},
                {line,27}]}]}
ring_tests: new_node_and_spawn_next_node_in_the_ring_test (module 'ring_tests')...*timed out* 
undefined
=======================================================
  Failed: 0.  Skipped: 0.  Passed: 0.
One or more tests were cancelled.
error
14> c(ring).         
Recompiling /home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/src/ring.erl
/home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/src/ring.erl:15: variable 'ParentPid' is unbound
/home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/src/ring.erl:14: Warning: variable 'Parent' is unused
error
15> c(ring).         
Recompiling /home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/src/ring.erl
{ok,ring}
16> eunit:test(ring).
ring_tests: new_node_and_spawn_next_node_in_the_ring_test (module 'ring_tests')...*failed*
in function ring_tests:'-new_node_and_spawn_next_node_in_the_ring_test/0-fun-0-'/1 (/home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/test/ring_tests.erl, line 10)
**error:{assert,[{module,ring_tests},
         {line,10},
         {expression,"is_process_alive ( NewNodePid )"},
         {expected,true},
         {value,false}]}
  output:<<"">>

=======================================================
  Failed: 1.  Skipped: 0.  Passed: 0.
error
17> c(ring).
Recompiling /home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/src/ring.erl
/home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/src/ring.erl:18: Warning: variable 'Pid' is unused
{ok,ring}
18> c(ring).
Recompiling /home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/src/ring.erl
{ok,ring}
19> eunit:test(ring).
  Test passed.
ok
20> c(ring).         
Recompiling /home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/src/ring.erl
{ok,ring}
21> eunit:test(ring).
  Test passed.
ok
22> c(ring_tests).   
Recompiling /home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/test/ring_tests.erl
{ok,ring_tests}
23> eunit:test(ring).
  Test passed.
ok
24> c(ring_tests).   
Recompiling /home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/test/ring_tests.erl
{ok,ring_tests}
25> eunit:test(ring).
  Test passed.
ok
26> c(ring_tests).   
Recompiling /home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/test/ring_tests.erl
{ok,ring_tests}
27> eunit:test(ring).
ring_tests: new_node_and_spawn_next_node_in_the_ring_test (module 'ring_tests')...*failed*
in function ring_tests:'-new_node_and_spawn_next_node_in_the_ring_test/0-fun-2-'/1 (/home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/test/ring_tests.erl, line 13)
**error:{assert,[{module,ring_tests},
         {line,13},
         {comment,<0.283.0>},
         {expression,"NewNodePid"},
         {expected,true},
         {not_boolean,<0.283.0>}]}
  output:<<"">>

=======================================================
  Failed: 1.  Skipped: 0.  Passed: 0.
error
28> c(ring_tests).   
Recompiling /home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/test/ring_tests.erl
{ok,ring_tests}
29> eunit:test(ring).
  Test passed.
ok
30> c(ring_tests).
Recompiling /home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/test/ring_tests.erl
/home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/test/ring_tests.erl:19: function send_message/3 undefined
/home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/test/ring_tests.erl:21: function fetch_message/1 undefined
error
31> c(ring_tests).   
Recompiling /home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/test/ring_tests.erl
{ok,ring_tests}
32> eunit:test(ring).
ring_tests: node_send_message_to_the_next_test...*failed*
in function ring:send_message/3
  called as send_message(<0.349.0>,<0.350.0>,'how are you?')
in call from ring_tests:node_send_message_to_the_next_test/0 (/home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/test/ring_tests.erl, line 20)
in call from ring_tests:node_send_message_to_the_next_test/0
**error:undef
  output:<<"">>

=======================================================
  Failed: 1.  Skipped: 0.  Passed: 1.
error
33> c(ring).
Recompiling /home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/src/ring.erl
/home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/src/ring.erl:34: syntax error before: ')'
/home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/src/ring.erl:42: syntax error before: 'end'
/home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/src/ring.erl:2: function fetch_message/1 undefined
/home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/src/ring.erl:30: function loop/1 undefined
/home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/src/ring.erl:32: function loop/1 undefined
error
34> c(ring).
Recompiling /home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/src/ring.erl
/home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/src/ring.erl:41: syntax error before: '.'
/home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/src/ring.erl:42: syntax error before: 'end'
/home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/src/ring.erl:2: function fetch_message/1 undefined
/home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/src/ring.erl:30: function loop/1 undefined
/home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/src/ring.erl:32: function loop/1 undefined
error
35> c(ring).
Recompiling /home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/src/ring.erl
/home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/src/ring.erl:41: syntax error before: '.'
/home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/src/ring.erl:42: syntax error before: 'end'
/home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/src/ring.erl:2: function fetch_message/1 undefined
/home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/src/ring.erl:30: function loop/1 undefined
/home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/src/ring.erl:32: function loop/1 undefined
error
36> c(ring).
Recompiling /home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/src/ring.erl
/home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/src/ring.erl:44: syntax error before: '.'
/home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/src/ring.erl:45: syntax error before: 'end'
/home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/src/ring.erl:33: function loop/1 undefined
/home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/src/ring.erl:35: function loop/1 undefined
/home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/src/ring.erl:21: Warning: variable 'NodePid' is unused
error
37> c(ring).
Recompiling /home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/src/ring.erl
/home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/src/ring.erl:44: syntax error before: '.'
/home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/src/ring.erl:45: syntax error before: 'end'
/home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/src/ring.erl:33: function loop/1 undefined
/home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/src/ring.erl:35: function loop/1 undefined
error
38> c(ring).
Recompiling /home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/src/ring.erl
/home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/src/ring.erl:37: Warning: variable 'LastMessage' is unused
{ok,ring}
39> c(ring).    
Recompiling /home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/src/ring.erl
{ok,ring}
40> eunit:test(ring).
ring_tests: node_send_message_to_the_next_test...*failed*
in function ring_tests:'-node_send_message_to_the_next_test/0-fun-0-'/1 (/home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/test/ring_tests.erl, line 24)
**error:{assertEqual,[{module,ring_tests},
              {line,24},
              {expression,"Reply"},
              {expected,'how are you?'},
              {value,ok}]}
  output:<<"">>

=======================================================
  Failed: 1.  Skipped: 0.  Passed: 1.
error
41> c(ring).
Recompiling /home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/src/ring.erl
{ok,ring}
42> c(ring).         
Recompiling /home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/src/ring.erl
{ok,ring}
43> eunit:test(ring).
ring_tests: node_send_message_to_the_next_test...*timed out*
undefined
=======================================================
  Failed: 0.  Skipped: 0.  Passed: 0.
One or more tests were cancelled.
error
44> c(ring).         
Recompiling /home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/src/ring.erl
{ok,ring}
45> eunit:test(ring).
  2 tests passed.
ok
46> c(ring).         
Recompiling /home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/src/ring.erl
{ok,ring}
47> c(ring_test).
{error,non_existing}
48> c(ring_tests).
Recompiling /home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/test/ring_tests.erl
{ok,ring_tests}
49> eunit:test(ring).
ring_tests: when_node_receive_quit_message_then_terminate_gracefully_test...*failed*
in function ring_tests:'-when_node_receive_quit_message_then_terminate_gracefully_test/0-fun-0-'/1 (/home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/test/ring_tests.erl, line 32)
**error:{assert,[{module,ring_tests},
         {line,32},
         {expression,"is_process_alive ( NextNode )"},
         {expected,false},
         {value,true}]}
  output:<<"">>

=======================================================
  Failed: 1.  Skipped: 0.  Passed: 2.
error
50> c(ring).         
Recompiling /home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/src/ring.erl
{ok,ring}
51> eunit:test(ring).
ring_tests: when_node_receive_quit_message_then_terminate_gracefully_test...*failed*
in function ring_tests:'-when_node_receive_quit_message_then_terminate_gracefully_test/0-fun-0-'/1 (/home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/test/ring_tests.erl, line 32)
**error:{assert,[{module,ring_tests},
         {line,32},
         {expression,"is_process_alive ( NextNode )"},
         {expected,false},
         {value,true}]}
  output:<<"">>

=======================================================
  Failed: 1.  Skipped: 0.  Passed: 2.
error
52> c(ring).         
Recompiling /home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/src/ring.erl
/home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/src/ring.erl:28: Warning: variable 'From' is unused
{ok,ring}
53> c(ring).         
Recompiling /home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/src/ring.erl
{ok,ring}
54> eunit:test(ring).
ring_tests: when_node_receive_quit_message_then_terminate_gracefully_test...*failed*
in function ring_tests:'-when_node_receive_quit_message_then_terminate_gracefully_test/0-fun-0-'/1 (/home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/test/ring_tests.erl, line 32)
**error:{assert,[{module,ring_tests},
         {line,32},
         {expression,"is_process_alive ( NextNode )"},
         {expected,false},
         {value,true}]}
  output:<<"">>

=======================================================
  Failed: 1.  Skipped: 0.  Passed: 2.
error
55> c(ring).         
Recompiling /home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/src/ring.erl
{ok,ring}
56> eunit:test(ring).
ring_tests: when_node_receive_quit_message_then_terminate_gracefully_test...*failed*
in function ring_tests:'-when_node_receive_quit_message_then_terminate_gracefully_test/0-fun-0-'/1 (/home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/test/ring_tests.erl, line 32)
**error:{assert,[{module,ring_tests},
         {line,32},
         {expression,"is_process_alive ( NextNode )"},
         {expected,false},
         {value,true}]}
  output:<<"">>

=======================================================
  Failed: 1.  Skipped: 0.  Passed: 2.
error
57> c(ring_tests).   
Recompiling /home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/test/ring_tests.erl
{ok,ring_tests}
58> eunit:test(ring).
ring_tests: when_node_receive_quit_message_then_terminate_gracefully_test...*failed*
in function ring_tests:'-when_node_receive_quit_message_then_terminate_gracefully_test/0-fun-0-'/1 (/home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/test/ring_tests.erl, line 32)
**error:{assert,[{module,ring_tests},
         {line,32},
         {expression,"is_process_alive ( NextNode )"},
         {expected,false},
         {value,true}]}
  output:<<"">>

=======================================================
  Failed: 1.  Skipped: 0.  Passed: 2.
error
59> c(ring_tests).   
Recompiling /home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/test/ring_tests.erl
{ok,ring_tests}
60> eunit:test(ring).
  All 3 tests passed.
ok
61> c(ring_tests).   
Recompiling /home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/test/ring_tests.erl
{ok,ring_tests}
62> eunit:test(ring).
ring_tests: when_node_receive_quit_message_then_then_node_and_the_parent_terminate_gracefully_test...*failed*
in function ring_tests:'-when_node_receive_quit_message_then_then_node_and_the_parent_terminate_gracefully_test/0-fun-1-'/1 (/home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/test/ring_tests.erl, line 34)
**error:{assert,[{module,ring_tests},
         {line,34},
         {expression,"is_process_alive ( FirstNode )"},
         {expected,false},
         {value,true}]}
  output:<<"">>

=======================================================
  Failed: 1.  Skipped: 0.  Passed: 2.
error
63> c(ring_tests).   
Recompiling /home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/test/ring_tests.erl
{ok,ring_tests}
64> eunit:test(ring).
ring_tests: when_node_receive_quit_message_then_then_node_and_the_parent_terminate_gracefully_test...*failed*
in function ring_tests:'-when_node_receive_quit_message_then_then_node_and_the_parent_terminate_gracefully_test/0-fun-1-'/1 (/home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/test/ring_tests.erl, line 34)
**error:{assert,[{module,ring_tests},
         {line,34},
         {expression,"is_process_alive ( FirstNode )"},
         {expected,false},
         {value,true}]}
  output:<<"">>

=======================================================
  Failed: 1.  Skipped: 0.  Passed: 2.
error
65> c(ring).         
Recompiling /home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/src/ring.erl
{ok,ring}
66> eunit:test(ring).
  All 3 tests passed.
ok
67> c(ring).         
Recompiling /home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/src/ring.erl
{ok,ring}
68> eunit:test(ring).
  All 3 tests passed.
ok
69> c(ring). 
Recompiling /home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/src/ring.erl
/home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/src/ring.erl:47: illegal pattern
error
70> c(ring).
Recompiling /home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/src/ring.erl
{ok,ring}
71> eunit:test(ring).

=ERROR REPORT==== 2-May-2018::21:01:14 ===
Error in process <0.705.0> with exit value:
{badarg,[{ring,loop,1,
               [{file,"/home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/src/ring.erl"},
                {line,59}]}]}
ring_tests: new_node_and_spawn_next_node_in_the_ring_test...*failed*
in function ring_tests:new_node_and_spawn_next_node_in_the_ring_test/0 (/home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/test/ring_tests.erl, line 13)
**error:{badmatch,undefined}
  output:<<"">>

=======================================================
  Failed: 1.  Skipped: 0.  Passed: 2.
error
72> c(ring).         
Recompiling /home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/src/ring.erl
/home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/src/ring.erl:44: Warning: variable 'Child' is unused
{ok,ring}
73> c(ring).         
Recompiling /home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/src/ring.erl
{ok,ring}
74> eunit:test(ring).

=ERROR REPORT==== 2-May-2018::21:02:05 ===
Error in process <0.729.0> with exit value:
{badarg,[{ring,loop,1,
               [{file,"/home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/src/ring.erl"},
                {line,59}]}]}
ring_tests: new_node_and_spawn_next_node_in_the_ring_test...*failed*
in function ring_tests:new_node_and_spawn_next_node_in_the_ring_test/0 (/home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/test/ring_tests.erl, line 13)
**error:{badmatch,undefined}
  output:<<"">>

=======================================================
  Failed: 1.  Skipped: 0.  Passed: 2.
error
75> c(ring).         
Recompiling /home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/src/ring.erl
{ok,ring}
76> eunit:test(ring).

=ERROR REPORT==== 2-May-2018::21:02:43 ===
Error in process <0.749.0> with exit value:
{badarg,[{ring,loop,1,
               [{file,"/home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/src/ring.erl"},
                {line,59}]}]}
ring_tests: when_node_receive_quit_message_then_then_node_and_the_parent_terminate_gracefully_test...*failed*
in function ring_tests:'-when_node_receive_quit_message_then_then_node_and_the_parent_terminate_gracefully_test/0-fun-1-'/1 (/home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/test/ring_tests.erl, line 34)
**error:{assert,[{module,ring_tests},
         {line,34},
         {expression,"is_process_alive ( FirstNode )"},
         {expected,false},
         {value,true}]}
  output:<<"">>

ring_tests: new_node_and_spawn_next_node_in_the_ring_test...*failed*
in function ring_tests:new_node_and_spawn_next_node_in_the_ring_test/0 (/home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/test/ring_tests.erl, line 13)
**error:{badmatch,undefined}
  output:<<"">>

=======================================================
  Failed: 2.  Skipped: 0.  Passed: 1.
error
77> c(ring).         
Recompiling /home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/src/ring.erl
{ok,ring}
78> eunit:test(ring).

=ERROR REPORT==== 2-May-2018::21:04:51 ===
Error in process <0.768.0> with exit value:
{{case_clause,false},
 [{ring,loop,1,
        [{file,"/home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/src/ring.erl"},
         {line,59}]}]}
ring_tests: when_node_receive_quit_message_then_then_node_and_the_parent_terminate_gracefully_test...*failed*
in function ring_tests:'-when_node_receive_quit_message_then_then_node_and_the_parent_terminate_gracefully_test/0-fun-1-'/1 (/home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/test/ring_tests.erl, line 34)
**error:{assert,[{module,ring_tests},
         {line,34},
         {expression,"is_process_alive ( FirstNode )"},
         {expected,false},
         {value,true}]}
  output:<<"">>

ring_tests: new_node_and_spawn_next_node_in_the_ring_test...*failed*
in function ring_tests:new_node_and_spawn_next_node_in_the_ring_test/0 (/home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/test/ring_tests.erl, line 13)
**error:{badmatch,undefined}
  output:<<"">>

=======================================================
  Failed: 2.  Skipped: 0.  Passed: 1.
error
79> c(ring_tests).   
Recompiling /home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/test/ring_tests.erl
{ok,ring_tests}
80> c(ring_).        
{error,non_existing}
81> c(ring). 
Recompiling /home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/src/ring.erl
{ok,ring}
82> c(ring_tests).
Recompiling /home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/test/ring_tests.erl
{ok,ring_tests}
83> eunit:test(ring).

=ERROR REPORT==== 2-May-2018::21:05:13 ===
Error in process <0.822.0> with exit value:
{{case_clause,false},
 [{ring,loop,1,
        [{file,"/home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/src/ring.erl"},
         {line,59}]}]}
ring_tests: when_node_receive_quit_message_then_then_node_and_the_parent_terminate_gracefully_test...*failed*
in function ring_tests:'-when_node_receive_quit_message_then_then_node_and_the_parent_terminate_gracefully_test/0-fun-1-'/1 (/home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/test/ring_tests.erl, line 34)
**error:{assert,[{module,ring_tests},
         {line,34},
         {expression,"is_process_alive ( FirstNode )"},
         {expected,false},
         {value,true}]}
  output:<<"">>

ring_tests: new_node_and_spawn_next_node_in_the_ring_test...*failed*
in function ring_tests:new_node_and_spawn_next_node_in_the_ring_test/0 (/home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/test/ring_tests.erl, line 13)
**error:{badmatch,undefined}
  output:<<"">>

=======================================================
  Failed: 2.  Skipped: 0.  Passed: 1.
error
84> c(ring_tests).
Recompiling /home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/test/ring_tests.erl
/home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/test/ring_tests.erl:30: syntax error before: quit
error
85> c(ring_tests).   
Recompiling /home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/test/ring_tests.erl
{ok,ring_tests}
86> eunit:test(ring).
ring_tests: when_node_receive_quit_message_then_then_node_and_the_parent_terminate_gracefully_test...*failed*
in function ring:send_quit_message/1
  called as send_quit_message(<0.868.0>)
in call from ring_tests:when_node_receive_quit_message_then_then_node_and_the_parent_terminate_gracefully_test/0 (/home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/test/ring_tests.erl, line 30)
in call from ring_tests:when_node_receive_quit_message_then_then_node_and_the_parent_terminate_gracefully_test/0
**error:undef
  output:<<"">>

ring_tests: new_node_and_spawn_next_node_in_the_ring_test...*failed*
in function ring_tests:new_node_and_spawn_next_node_in_the_ring_test/0 (/home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/test/ring_tests.erl, line 13)
**error:{badmatch,undefined}
  output:<<"">>

=======================================================
  Failed: 2.  Skipped: 0.  Passed: 1.
error
87> c(ring).
Recompiling /home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/src/ring.erl
{ok,ring}
88> eunit:test(ring).
ring_tests: when_node_receive_quit_message_then_then_node_and_the_parent_terminate_gracefully_test...*failed*
in function ring:send_quit_message/1
  called as send_quit_message(<0.887.0>)
in call from ring_tests:when_node_receive_quit_message_then_then_node_and_the_parent_terminate_gracefully_test/0 (/home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/test/ring_tests.erl, line 30)
in call from ring_tests:when_node_receive_quit_message_then_then_node_and_the_parent_terminate_gracefully_test/0
**error:undef
  output:<<"">>

ring_tests: new_node_and_spawn_next_node_in_the_ring_test...*failed*
in function ring_tests:new_node_and_spawn_next_node_in_the_ring_test/0 (/home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/test/ring_tests.erl, line 13)
**error:{badmatch,undefined}
  output:<<"">>

=======================================================
  Failed: 2.  Skipped: 0.  Passed: 1.
error
89> c(ring).         
Recompiling /home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/src/ring.erl
{ok,ring}
90> eunit:test(ring).

=ERROR REPORT==== 2-May-2018::21:07:33 ===
Error in process <0.906.0> with exit value:
{{case_clause,false},
 [{ring,loop,1,
        [{file,"/home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/src/ring.erl"},
         {line,60}]}]}
ring_tests: when_node_receive_quit_message_then_then_node_and_the_parent_terminate_gracefully_test...*failed*
in function ring_tests:'-when_node_receive_quit_message_then_then_node_and_the_parent_terminate_gracefully_test/0-fun-0-'/1 (/home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/test/ring_tests.erl, line 33)
in call from ring_tests:when_node_receive_quit_message_then_then_node_and_the_parent_terminate_gracefully_test/0 (/home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/test/ring_tests.erl, line 33)
**error:{assert,[{module,ring_tests},
         {line,33},
         {expression,"is_process_alive ( NextNode )"},
         {expected,false},
         {value,true}]}
  output:<<"">>

ring_tests: new_node_and_spawn_next_node_in_the_ring_test...*failed*
in function ring_tests:new_node_and_spawn_next_node_in_the_ring_test/0 (/home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/test/ring_tests.erl, line 13)
**error:{badmatch,undefined}
  output:<<"">>

=======================================================
  Failed: 2.  Skipped: 0.  Passed: 1.
error
91> c(ring).         
Recompiling /home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/src/ring.erl
{ok,ring}
92> eunit:test(ring).

=ERROR REPORT==== 2-May-2018::21:08:04 ===
Error in process <0.925.0> with exit value:
{{case_clause,false},
 [{ring,loop,1,
        [{file,"/home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/src/ring.erl"},
         {line,60}]}]}
ring_tests: when_node_receive_quit_message_then_then_node_and_the_parent_terminate_gracefully_test...*failed*
in function ring_tests:'-when_node_receive_quit_message_then_then_node_and_the_parent_terminate_gracefully_test/0-fun-0-'/1 (/home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/test/ring_tests.erl, line 33)
in call from ring_tests:when_node_receive_quit_message_then_then_node_and_the_parent_terminate_gracefully_test/0 (/home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/test/ring_tests.erl, line 33)
**error:{assert,[{module,ring_tests},
         {line,33},
         {expression,"is_process_alive ( NextNode )"},
         {expected,false},
         {value,true}]}
  output:<<"">>

ring_tests: new_node_and_spawn_next_node_in_the_ring_test...*failed*
in function ring_tests:new_node_and_spawn_next_node_in_the_ring_test/0 (/home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/test/ring_tests.erl, line 13)
**error:{badmatch,undefined}
  output:<<"">>

=======================================================
  Failed: 2.  Skipped: 0.  Passed: 1.
error
93> c(ring).
Recompiling /home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/src/ring.erl
{ok,ring}
94> c(ring_tests).
Recompiling /home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/test/ring_tests.erl
{ok,ring_tests}
95> eunit:test(ring).

=ERROR REPORT==== 2-May-2018::21:09:41 ===
Error in process <0.961.0> with exit value:
{{case_clause,false},
 [{ring,loop,1,
        [{file,"/home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/src/ring.erl"},
         {line,60}]}]}
ring_tests: when_node_receive_quit_message_then_then_node_and_the_parent_terminate_gracefully_test...*failed*
in function ring_tests:'-when_node_receive_quit_message_then_then_node_and_the_parent_terminate_gracefully_test/0-fun-0-'/1 (/home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/test/ring_tests.erl, line 33)
in call from ring_tests:when_node_receive_quit_message_then_then_node_and_the_parent_terminate_gracefully_test/0 (/home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/test/ring_tests.erl, line 33)
**error:{assert,[{module,ring_tests},
         {line,33},
         {expression,"is_process_alive ( NextNode )"},
         {expected,false},
         {value,true}]}
  output:<<"">>

ring_tests: new_node_and_spawn_next_node_in_the_ring_test...*failed*
in function ring_tests:new_node_and_spawn_next_node_in_the_ring_test/0 (/home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/test/ring_tests.erl, line 13)
**error:{badmatch,undefined}
  output:<<"">>

=======================================================
  Failed: 2.  Skipped: 0.  Passed: 1.
error
96> c(ring_tests).   
Recompiling /home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/test/ring_tests.erl
{ok,ring_tests}
97> c(ring).         
Recompiling /home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/src/ring.erl
{ok,ring}
98> eunit:test(ring).

=ERROR REPORT==== 2-May-2018::21:11:28 ===
Error in process <0.997.0> with exit value:
{{case_clause,false},
 [{ring,loop,1,
        [{file,"/home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/src/ring.erl"},
         {line,60}]}]}
ring_tests: when_node_receive_quit_message_then_then_node_and_the_parent_terminate_gracefully_test...
=ERROR REPORT==== 2-May-2018::21:11:28 ===
Error in process <0.1000.0> with exit value:
{badarg,[{ring,loop,1,
               [{file,"/home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/src/ring.erl"},
                {line,45}]}]}

=ERROR REPORT==== 2-May-2018::21:11:28 ===
Error in process <0.1001.0> with exit value:
{badarg,[{ring,loop,1,
               [{file,"/home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/src/ring.erl"},
                {line,45}]}]}
*failed*
in function ring_tests:'-when_node_receive_quit_message_then_then_node_and_the_parent_terminate_gracefully_test/0-fun-0-'/1 (/home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/test/ring_tests.erl, line 33)
in call from ring_tests:when_node_receive_quit_message_then_then_node_and_the_parent_terminate_gracefully_test/0 (/home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/test/ring_tests.erl, line 33)
**error:{assert,[{module,ring_tests},
         {line,33},
         {expression,"is_process_alive ( NextNode )"},
         {expected,false},
         {value,true}]}
  output:<<"">>

ring_tests: node_send_message_to_the_next_test...*timed out*
undefined
=======================================================
  Failed: 1.  Skipped: 0.  Passed: 0.
One or more tests were cancelled.
error
99> c(ring).         
Recompiling /home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/src/ring.erl
{ok,ring}
100> eunit:test(ring).

=ERROR REPORT==== 2-May-2018::21:13:35 ===
Error in process <0.1013.0> with exit value:
{{case_clause,false},
 [{ring,loop,1,
        [{file,"/home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/src/ring.erl"},
         {line,60}]}]}
ring_tests: when_node_receive_quit_message_then_then_node_and_the_parent_terminate_gracefully_test...*failed*
in function ring_tests:'-when_node_receive_quit_message_then_then_node_and_the_parent_terminate_gracefully_test/0-fun-0-'/1 (/home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/test/ring_tests.erl, line 33)
in call from ring_tests:when_node_receive_quit_message_then_then_node_and_the_parent_terminate_gracefully_test/0 (/home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/test/ring_tests.erl, line 33)
**error:{assert,[{module,ring_tests},
         {line,33},
         {expression,"is_process_alive ( NextNode )"},
         {expected,false},
         {value,true}]}
  output:<<"">>

=======================================================
  Failed: 1.  Skipped: 0.  Passed: 2.
error
101> c(ring).         
Recompiling /home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/src/ring.erl
{ok,ring}
102> eunit:test(ring).
ring_tests: when_node_receive_quit_message_then_then_node_and_the_parent_terminate_gracefully_test...*failed*
in function ring_tests:'-when_node_receive_quit_message_then_then_node_and_the_parent_terminate_gracefully_test/0-fun-0-'/1 (/home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/test/ring_tests.erl, line 33)
in call from ring_tests:when_node_receive_quit_message_then_then_node_and_the_parent_terminate_gracefully_test/0 (/home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/test/ring_tests.erl, line 33)
**error:{assert,[{module,ring_tests},
         {line,33},
         {expression,"is_process_alive ( NextNode )"},
         {expected,false},
         {value,true}]}
  output:<<"">>

=======================================================
  Failed: 1.  Skipped: 0.  Passed: 2.
error
103> c(ring).         
Recompiling /home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/src/ring.erl
{ok,ring}
104> eunit:test(ring).
ring_tests: when_node_receive_quit_message_then_then_node_and_the_parent_terminate_gracefully_test...*failed*
in function ring_tests:'-when_node_receive_quit_message_then_then_node_and_the_parent_terminate_gracefully_test/0-fun-0-'/1 (/home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/test/ring_tests.erl, line 33)
in call from ring_tests:when_node_receive_quit_message_then_then_node_and_the_parent_terminate_gracefully_test/0 (/home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/test/ring_tests.erl, line 33)
**error:{assert,[{module,ring_tests},
         {line,33},
         {expression,"is_process_alive ( NextNode )"},
         {expected,false},
         {value,true}]}
  output:<<"">>

=======================================================
  Failed: 1.  Skipped: 0.  Passed: 2.
error
105> c(ring_tests).   
Recompiling /home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/test/ring_tests.erl
{ok,ring_tests}
106> eunit:test(ring).
ring_tests: when_node_receive_quit_message_then_then_node_and_the_parent_terminate_gracefully_test...*failed*
in function ring_tests:'-when_node_receive_quit_message_then_then_node_and_the_parent_terminate_gracefully_test/0-fun-0-'/1 (/home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/test/ring_tests.erl, line 34)
in call from ring_tests:when_node_receive_quit_message_then_then_node_and_the_parent_terminate_gracefully_test/0 (/home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/test/ring_tests.erl, line 34)
**error:{assert,[{module,ring_tests},
         {line,34},
         {expression,"is_process_alive ( NextNode )"},
         {expected,false},
         {value,true}]}
  output:<<"">>

=======================================================
  Failed: 1.  Skipped: 0.  Passed: 2.
error
107> c(ring_tests).   
Recompiling /home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/test/ring_tests.erl
{ok,ring_tests}
108> eunit:test(ring).
ring_tests: when_node_receive_quit_message_then_then_node_and_the_parent_terminate_gracefully_test...*failed*
in function ring_tests:'-when_node_receive_quit_message_then_then_node_and_the_parent_terminate_gracefully_test/0-fun-1-'/1 (/home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/test/ring_tests.erl, line 35)
**error:{assert,[{module,ring_tests},
         {line,35},
         {expression,"is_process_alive ( FirstNode )"},
         {expected,false},
         {value,true}]}
  output:<<"">>

=======================================================
  Failed: 1.  Skipped: 0.  Passed: 2.
error
109> c(ring_tests).   
Recompiling /home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/test/ring_tests.erl
{ok,ring_tests}
110> eunit:test(ring).
ring_tests: when_node_receive_quit_message_then_then_node_and_the_parent_terminate_gracefully_test...*failed*
in function ring_tests:'-when_node_receive_quit_message_then_then_node_and_the_parent_terminate_gracefully_test/0-fun-1-'/1 (/home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/test/ring_tests.erl, line 35)
**error:{assert,[{module,ring_tests},
         {line,35},
         {expression,"is_process_alive ( FirstNode )"},
         {expected,false},
         {value,true}]}
  output:<<"">>

=======================================================
  Failed: 1.  Skipped: 0.  Passed: 2.
error
111> c(ring).         
Recompiling /home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/src/ring.erl
{ok,ring}
112> eunit:test(ring).
ring_tests: when_node_receive_quit_message_then_then_node_and_the_parent_terminate_gracefully_test...*failed*
in function ring_tests:'-when_node_receive_quit_message_then_then_node_and_the_parent_terminate_gracefully_test/0-fun-1-'/1 (/home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/test/ring_tests.erl, line 35)
**error:{assert,[{module,ring_tests},
         {line,35},
         {expression,"is_process_alive ( FirstNode )"},
         {expected,false},
         {value,true}]}
  output:<<"">>

=======================================================
  Failed: 1.  Skipped: 0.  Passed: 2.
error
113> c(ring_tests).   
Recompiling /home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/test/ring_tests.erl
{ok,ring_tests}
114> c(ring).         
Recompiling /home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/src/ring.erl
{ok,ring}
115> eunit:test(ring).
ring_tests: when_node_receive_quit_message_then_then_node_and_the_parent_terminate_gracefully_test...*failed*
in function ring_tests:'-when_node_receive_quit_message_then_then_node_and_the_parent_terminate_gracefully_test/0-fun-1-'/1 (/home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/test/ring_tests.erl, line 35)
**error:{assert,[{module,ring_tests},
         {line,35},
         {expression,"is_process_alive ( FirstNode )"},
         {expected,false},
         {value,true}]}
  output:<<"">>

=======================================================
  Failed: 1.  Skipped: 0.  Passed: 2.
error
116> c(ring).         
Recompiling /home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/src/ring.erl
{ok,ring}
117> c(ring_tests).   
Recompiling /home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/test/ring_tests.erl
{ok,ring_tests}
118> eunit:test(ring).
ring_tests: when_node_receive_quit_message_then_then_node_and_the_parent_terminate_gracefully_test...*failed*
in function ring_tests:'-when_node_receive_quit_message_then_then_node_and_the_parent_terminate_gracefully_test/0-fun-0-'/1 (/home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/test/ring_tests.erl, line 34)
in call from ring_tests:when_node_receive_quit_message_then_then_node_and_the_parent_terminate_gracefully_test/0 (/home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/test/ring_tests.erl, line 34)
**error:{assert,[{module,ring_tests},
         {line,34},
         {expression,"is_process_alive ( NextNode )"},
         {expected,false},
         {value,true}]}
  output:<<"">>

=======================================================
  Failed: 1.  Skipped: 0.  Passed: 2.
error
119> c(ring_tests).   
Recompiling /home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/test/ring_tests.erl
{ok,ring_tests}
120> eunit:test(ring).
  All 3 tests passed.
ok
121> c(ring).         
Recompiling /home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/src/ring.erl
{ok,ring}
122> eunit:test(ring).
  All 3 tests passed.
ok
123> c(ring_tests).   
Recompiling /home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/test/ring_tests.erl
{ok,ring_tests}
124> eunit:test(ring).
ring_tests: when_node_receive_quit_message_then_then_node_and_the_parent_terminate_gracefully_test...*failed*
in function ring_tests:'-when_node_receive_quit_message_then_then_node_and_the_parent_terminate_gracefully_test/0-fun-0-'/1 (/home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/test/ring_tests.erl, line 34)
in call from ring_tests:when_node_receive_quit_message_then_then_node_and_the_parent_terminate_gracefully_test/0 (/home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/test/ring_tests.erl, line 34)
**error:{assert,[{module,ring_tests},
         {line,34},
         {expression,"is_process_alive ( NextNode )"},
         {expected,false},
         {value,true}]}
  output:<<"">>

=======================================================
  Failed: 1.  Skipped: 0.  Passed: 2.
error
125> c(ring).         
Recompiling /home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/src/ring.erl
{ok,ring}
126> c(ring_tests).
Recompiling /home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/test/ring_tests.erl
{ok,ring_tests}
127> eunit:test(ring).
ring_tests: when_node_receive_quit_message_then_then_node_and_the_parent_terminate_gracefully_test...*failed*
in function ring_tests:'-when_node_receive_quit_message_then_then_node_and_the_parent_terminate_gracefully_test/0-fun-0-'/1 (/home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/test/ring_tests.erl, line 34)
in call from ring_tests:when_node_receive_quit_message_then_then_node_and_the_parent_terminate_gracefully_test/0 (/home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/test/ring_tests.erl, line 34)
**error:{assert,[{module,ring_tests},
         {line,34},
         {expression,"is_process_alive ( NextNode )"},
         {expected,false},
         {value,true}]}
  output:<<"">>

=======================================================
  Failed: 1.  Skipped: 0.  Passed: 2.
error
128> c(ring).         
Recompiling /home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/src/ring.erl
{ok,ring}
129> eunit:test(ring).
ring_tests: when_node_receive_quit_message_then_then_node_and_the_parent_terminate_gracefully_test...*failed*
in function ring_tests:'-when_node_receive_quit_message_then_then_node_and_the_parent_terminate_gracefully_test/0-fun-0-'/1 (/home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/test/ring_tests.erl, line 34)
in call from ring_tests:when_node_receive_quit_message_then_then_node_and_the_parent_terminate_gracefully_test/0 (/home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/test/ring_tests.erl, line 34)
**error:{assert,[{module,ring_tests},
         {line,34},
         {expression,"is_process_alive ( NextNode )"},
         {expected,false},
         {value,true}]}
  output:<<"">>

=======================================================
  Failed: 1.  Skipped: 0.  Passed: 2.
error
130> c(ring_tests).   
Recompiling /home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/test/ring_tests.erl
{ok,ring_tests}
131> eunit:test(ring).
ring_tests: when_node_receive_quit_message_then_then_node_and_the_parent_terminate_gracefully_test...*failed*
in function ring_tests:'-when_node_receive_quit_message_then_then_node_and_the_parent_terminate_gracefully_test/0-fun-0-'/1 (/home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/test/ring_tests.erl, line 34)
in call from ring_tests:when_node_receive_quit_message_then_then_node_and_the_parent_terminate_gracefully_test/0 (/home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/test/ring_tests.erl, line 34)
**error:{assert,[{module,ring_tests},
         {line,34},
         {expression,"is_process_alive ( NextNode )"},
         {expected,false},
         {value,true}]}
  output:<<"">>

=======================================================
  Failed: 1.  Skipped: 0.  Passed: 2.
error
132> c(ring_tests).   
Recompiling /home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/test/ring_tests.erl
{ok,ring_tests}
133> c(ring).         
Recompiling /home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/src/ring.erl
{ok,ring}
134> eunit:test(ring).
ring_tests: when_node_receive_quit_message_then_then_node_and_the_parent_terminate_gracefully_test...*failed*
in function ring_tests:'-when_node_receive_quit_message_then_then_node_and_the_parent_terminate_gracefully_test/0-fun-0-'/1 (/home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/test/ring_tests.erl, line 34)
in call from ring_tests:when_node_receive_quit_message_then_then_node_and_the_parent_terminate_gracefully_test/0 (/home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/test/ring_tests.erl, line 34)
**error:{assert,[{module,ring_tests},
         {line,34},
         {expression,"is_process_alive ( NextNode )"},
         {expected,false},
         {value,true}]}
  output:<<"">>

=======================================================
  Failed: 1.  Skipped: 0.  Passed: 2.
error
135> c(ring).         
Recompiling /home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/src/ring.erl
{ok,ring}
136> eunit:test(ring).
ring_tests: when_node_receive_quit_message_then_then_node_and_the_parent_terminate_gracefully_test...*failed*
in function ring_tests:'-when_node_receive_quit_message_then_then_node_and_the_parent_terminate_gracefully_test/0-fun-0-'/1 (/home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/test/ring_tests.erl, line 34)
in call from ring_tests:when_node_receive_quit_message_then_then_node_and_the_parent_terminate_gracefully_test/0 (/home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/test/ring_tests.erl, line 34)
**error:{assert,[{module,ring_tests},
         {line,34},
         {expression,"is_process_alive ( NextNode )"},
         {expected,false},
         {value,true}]}
  output:<<"">>

=======================================================
  Failed: 1.  Skipped: 0.  Passed: 2.
error
137> c(ring).         
Recompiling /home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/src/ring.erl
/home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/src/ring.erl:44: Warning: variable 'LastMessage' is unused
{ok,ring}
138> eunit:test(ring).
ring_tests: when_node_receive_quit_message_then_then_node_and_the_parent_terminate_gracefully_test...*failed*
in function ring_tests:'-when_node_receive_quit_message_then_then_node_and_the_parent_terminate_gracefully_test/0-fun-0-'/1 (/home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/test/ring_tests.erl, line 34)
in call from ring_tests:when_node_receive_quit_message_then_then_node_and_the_parent_terminate_gracefully_test/0 (/home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/test/ring_tests.erl, line 34)
**error:{assert,[{module,ring_tests},
         {line,34},
         {expression,"is_process_alive ( NextNode )"},
         {expected,false},
         {value,true}]}
  output:<<"">>

=======================================================
  Failed: 1.  Skipped: 0.  Passed: 2.
error
139> c(ring_tests).
Recompiling /home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/test/ring_tests.erl
{ok,ring_tests}
140> c(ring).
Recompiling /home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/src/ring.erl
/home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/src/ring.erl:44: Warning: variable 'LastMessage' is unused
/home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/src/ring.erl:52: Warning: variable 'From' is unused
{ok,ring}
141> c(ring).      
Recompiling /home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/src/ring.erl
/home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/src/ring.erl:52: Warning: variable 'From' is unused
{ok,ring}
142> c(ring).         
Recompiling /home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/src/ring.erl
{ok,ring}
143> c(ring_tests).
Recompiling /home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/test/ring_tests.erl
{ok,ring_tests}
144> eunit:test(ring).
ring_tests: when_node_receive_quit_message_then_then_node_and_the_parent_terminate_gracefully_test...*failed*
in function ring:set_child/2
  called as set_child(<0.1539.0>,<0.1540.0>)
in call from ring_tests:when_node_receive_quit_message_then_then_node_and_the_parent_terminate_gracefully_test/0 (/home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/test/ring_tests.erl, line 31)
in call from ring_tests:when_node_receive_quit_message_then_then_node_and_the_parent_terminate_gracefully_test/0
**error:undef
  output:<<"">>

=======================================================
  Failed: 1.  Skipped: 0.  Passed: 2.
error
145> c(ring_tests).   
Recompiling /home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/test/ring_tests.erl
{ok,ring_tests}
146> c(ring).      
Recompiling /home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/src/ring.erl
{ok,ring}
147> eunit:test(ring).
  All 3 tests passed.
ok
148> c(ring).         
Recompiling /home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/src/ring.erl
{ok,ring}
149> eunit:test(ring).

=ERROR REPORT==== 2-May-2018::21:30:01 ===
Error in process <0.1595.0> with exit value:
{badarg,[{ring,loop,1,
               [{file,"/home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/src/ring.erl"},
                {line,69}]}]}
  All 3 tests passed.
ok
150> c(ring).         
Recompiling /home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/src/ring.erl
{ok,ring}
151> eunit:test(ring).
  All 3 tests passed.
ok
152> c(ring_tests).
Recompiling /home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/test/ring_tests.erl
/home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/test/ring_tests.erl:39: variable 'N' is unbound
/home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/test/ring_tests.erl:39: function start/2 undefined
error
153> c(ring_tests).
Recompiling /home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/test/ring_tests.erl
/home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/test/ring_tests.erl:39: function start/2 undefined
error
154> c(ring_tests).   
Recompiling /home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/test/ring_tests.erl
{ok,ring_tests}
155> eunit:test(ring).
ring_tests: start_ring_test...*failed*
in function ring:start/2
  called as start(5,"hello")
in call from ring_tests:start_ring_test/0 (/home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/test/ring_tests.erl, line 40)
in call from ring_tests:start_ring_test/0
**error:undef
  output:<<"">>

=======================================================
  Failed: 1.  Skipped: 0.  Passed: 3.
error
156> c(ring_tests).   
Recompiling /home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/test/ring_tests.erl
{ok,ring_tests}
157> c(ring).      
Recompiling /home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/src/ring.erl
/home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/src/ring.erl:47: function start/3 undefined
/home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/src/ring.erl:46: Warning: variable 'Message' is unused
error
158> c(ring).
Recompiling /home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/src/ring.erl
{ok,ring}
159> c(ring_tests).
Recompiling /home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/test/ring_tests.erl
{ok,ring_tests}
160> eunit:test(ring).
ring_tests: start_ring_test...*failed*
in function ring_tests:'-start_ring_test/0-fun-0-'/1 (/home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/test/ring_tests.erl, line 46)
**error:{assert,[{module,ring_tests},
         {line,46},
         {expression,"is_process_alive ( lists : last ( Nodes ) )"},
         {expected,false},
         {value,true}]}
  output:<<"">>


=ERROR REPORT==== 2-May-2018::21:47:02 ===
Error in process <0.1744.0> with exit value:
{badarg,[{erlang,'++',['how are you?',"<0.1744.0>"],[]},
         {lists,append,2,[{file,"lists.erl"},{line,116}]},
         {ring,loop,1,
               [{file,"/home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/src/ring.erl"},
                {line,72}]}]}
ring_tests: node_send_message_to_the_next_test...*timed out*
undefined
=======================================================
  Failed: 1.  Skipped: 0.  Passed: 1.
One or more tests were cancelled.
error
161> atom_to_list('').
[]
162> atom_to_list('asd').
"asd"
163> c(ring).            
Recompiling /home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/src/ring.erl
{ok,ring}
164> eunit:test(ring).   

=ERROR REPORT==== 2-May-2018::21:48:39 ===
Error in process <0.1758.0> with exit value:
{badarg,[{erlang,atom_to_list,["hello"],[]},
         {ring,loop,1,
               [{file,"/home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/src/ring.erl"},
                {line,72}]}]}
ring_tests: start_ring_test...*failed*
in function ring_tests:'-start_ring_test/0-fun-0-'/1 (/home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/test/ring_tests.erl, line 46)
**error:{assert,[{module,ring_tests},
         {line,46},
         {expression,"is_process_alive ( lists : last ( Nodes ) )"},
         {expected,false},
         {value,true}]}
  output:<<"">>

ring_tests: node_send_message_to_the_next_test...*failed*
in function ring_tests:'-node_send_message_to_the_next_test/0-fun-0-'/1 (/home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/test/ring_tests.erl, line 26)
**error:{assertEqual,[{module,ring_tests},
              {line,26},
              {expression,"Reply"},
              {expected,'how are you?'},
              {value,"how are you?<0.1768.0>"}]}
  output:<<"">>

=======================================================
  Failed: 2.  Skipped: 0.  Passed: 2.
error
165> c(ring).         
Recompiling /home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/src/ring.erl
{ok,ring}
166> eunit:test(ring).
ring_tests: start_ring_test...*failed*
in function ring_tests:'-start_ring_test/0-fun-0-'/1 (/home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/test/ring_tests.erl, line 46)
**error:{assert,[{module,ring_tests},
         {line,46},
         {expression,"is_process_alive ( lists : last ( Nodes ) )"},
         {expected,false},
         {value,true}]}
  output:<<"">>

=======================================================
  Failed: 1.  Skipped: 0.  Passed: 3.
error
167> c(ring_tests).   
Recompiling /home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/test/ring_tests.erl
{ok,ring_tests}
168> eunit:test(ring).
ring_tests: start_ring_test...*failed*
in function ring_tests:'-start_ring_test/0-fun-0-'/1 (/home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/test/ring_tests.erl, line 46)
in call from ring_tests:start_ring_test/0 (/home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/test/ring_tests.erl, line 46)
**error:{assertEqual,[{module,ring_tests},
              {line,46},
              {expression,"Nodes"},
              {expected,[asd,as,d,asd]},
              {value,[<0.1824.0>,<0.1823.0>,<0.1822.0>,<0.1821.0>,
                      <0.1820.0>]}]}
  output:<<"">>

=======================================================
  Failed: 1.  Skipped: 0.  Passed: 3.
error
169> c(ring_tests).   
Recompiling /home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/test/ring_tests.erl
{ok,ring_tests}
170> eunit:test(ring).
ring_tests: start_ring_test...*failed*
in function ring_tests:'-start_ring_test/0-fun-0-'/1 (/home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/test/ring_tests.erl, line 45)
**error:{assert,[{module,ring_tests},
         {line,45},
         {expression,"is_process_alive ( lists : last ( Nodes ) )"},
         {expected,false},
         {value,true}]}
  output:<<"">>

=======================================================
  Failed: 1.  Skipped: 0.  Passed: 3.
error
171> c(ring).          
Recompiling /home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/src/ring.erl
{ok,ring}
172> eunit:test(ring).

=ERROR REPORT==== 2-May-2018::21:55:38 ===
Error in process <0.1898.0> with exit value:
{badarg,[{erlang,is_process_alive,[undefined],[]},
         {ring,'-loop/1-fun-0-',1,
               [{file,"/home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/src/ring.erl"},
                {line,81}]},
         {ring,loop,1,
               [{file,"/home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/src/ring.erl"},
                {line,81}]}]}
ring_tests: start_ring_test...
=ERROR REPORT==== 2-May-2018::21:55:38 ===
Error in process <0.1901.0> with exit value:
{badarg,[{erlang,is_process_alive,[undefined],[]},
         {ring,'-loop/1-fun-0-',1,
               [{file,"/home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/src/ring.erl"},
                {line,81}]},
         {ring,loop,1,
               [{file,"/home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/src/ring.erl"},
                {line,81}]}]}
*failed*
in function ring_tests:'-start_ring_test/0-fun-0-'/1 (/home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/test/ring_tests.erl, line 45)
**error:{assert,[{module,ring_tests},
         {line,45},
         {expression,"is_process_alive ( lists : last ( Nodes ) )"},
         {expected,false},
         {value,true}]}
  output:<<"">>

=======================================================
  Failed: 1.  Skipped: 0.  Passed: 3.
error
173> 
173> 
173> 
173> 
173> 
173> 
173> 
173> 
173> 
173> 
173> 
173> 
173> 
173> 
173> 
173> 
173> 
173> 
173> 
173> 
173> 
173> 
173> eunit:test(ring).

=ERROR REPORT==== 2-May-2018::21:55:47 ===
Error in process <0.1918.0> with exit value:
{badarg,[{erlang,is_process_alive,[undefined],[]},
         {ring,'-loop/1-fun-0-',1,
               [{file,"/home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/src/ring.erl"},
                {line,81}]},
         {ring,loop,1,
               [{file,"/home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/src/ring.erl"},
                {line,81}]}]}
ring_tests: start_ring_test...
=ERROR REPORT==== 2-May-2018::21:55:47 ===
Error in process <0.1921.0> with exit value:
{badarg,[{erlang,is_process_alive,[undefined],[]},
         {ring,'-loop/1-fun-0-',1,
               [{file,"/home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/src/ring.erl"},
                {line,81}]},
         {ring,loop,1,
               [{file,"/home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/src/ring.erl"},
                {line,81}]}]}
*failed*
in function ring_tests:'-start_ring_test/0-fun-0-'/1 (/home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/test/ring_tests.erl, line 45)
**error:{assert,[{module,ring_tests},
         {line,45},
         {expression,"is_process_alive ( lists : last ( Nodes ) )"},
         {expected,false},
         {value,true}]}
  output:<<"">>

=======================================================
  Failed: 1.  Skipped: 0.  Passed: 3.
error
174> c(ring).         
Recompiling /home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/src/ring.erl
{ok,ring}
175> eunit:test(ring).

=ERROR REPORT==== 2-May-2018::21:56:33 ===
Error in process <0.1952.0> with exit value:
{{assert,[{module,ring},
          {line,70},
          {expression,"is_process_alive ( Parent )"},
          {expected,true},
          {value,false}]},
 [{ring,'-loop/1-fun-0-',1,
        [{file,"/home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/src/ring.erl"},
         {line,70}]},
  {ring,loop,1,
        [{file,"/home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/src/ring.erl"},
         {line,70}]}]}
ring_tests: start_ring_test...
=ERROR REPORT==== 2-May-2018::21:56:33 ===
Error in process <0.1958.0> with exit value:
{badarg,[{erlang,is_process_alive,[undefined],[]},
         {ring,'-loop/1-fun-1-',1,
               [{file,"/home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/src/ring.erl"},
                {line,82}]},
         {ring,loop,1,
               [{file,"/home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/src/ring.erl"},
                {line,82}]}]}
*failed*
in function ring_tests:'-start_ring_test/0-fun-0-'/1 (/home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/test/ring_tests.erl, line 45)
**error:{assert,[{module,ring_tests},
         {line,45},
         {expression,"is_process_alive ( lists : last ( Nodes ) )"},
         {expected,false},
         {value,true}]}
  output:<<"">>

=======================================================
  Failed: 1.  Skipped: 0.  Passed: 3.
error
176> c(ring).         
Recompiling /home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/src/ring.erl
{ok,ring}
177> eunit:test(ring).

=ERROR REPORT==== 2-May-2018::21:56:44 ===
Error in process <0.1989.0> with exit value:
{{assert,[{module,ring},
          {line,70},
          {expression,"is_process_alive ( Parent )"},
          {expected,true},
          {value,false}]},
 [{ring,'-loop/1-fun-0-',1,
        [{file,"/home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/src/ring.erl"},
         {line,70}]},
  {ring,loop,1,
        [{file,"/home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/src/ring.erl"},
         {line,70}]}]}
ring_tests: start_ring_test...*failed*
in function ring_tests:'-start_ring_test/0-fun-0-'/1 (/home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/test/ring_tests.erl, line 45)
**error:{assert,[{module,ring_tests},
         {line,45},
         {expression,"is_process_alive ( lists : last ( Nodes ) )"},
         {expected,false},
         {value,true}]}
  output:<<"">>

=======================================================
  Failed: 1.  Skipped: 0.  Passed: 3.
error
178> 
178> 
178> 
178> 
178> 
178> 
178> 
178> 
178> 
178> 
178> 
178> 
178> 
178> 
178> 
178> 
178> 
178> eunit:test(ring).

=ERROR REPORT==== 2-May-2018::21:56:48 ===
Error in process <0.2009.0> with exit value:
{{assert,[{module,ring},
          {line,70},
          {expression,"is_process_alive ( Parent )"},
          {expected,true},
          {value,false}]},
 [{ring,'-loop/1-fun-0-',1,
        [{file,"/home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/src/ring.erl"},
         {line,70}]},
  {ring,loop,1,
        [{file,"/home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/src/ring.erl"},
         {line,70}]}]}

=ERROR REPORT==== 2-May-2018::21:56:48 ===
Error in process <0.2010.0> with exit value:
{{assert,[{module,ring},
          {line,70},
          {expression,"is_process_alive ( Parent )"},
          {expected,true},
          {value,false}]},
 [{ring,'-loop/1-fun-0-',1,
        [{file,"/home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/src/ring.erl"},
         {line,70}]},
  {ring,loop,1,
        [{file,"/home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/src/ring.erl"},
         {line,70}]}]}

=ERROR REPORT==== 2-May-2018::21:56:48 ===
Error in process <0.2011.0> with exit value:
{{assert,[{module,ring},
          {line,70},
          {expression,"is_process_alive ( Parent )"},
          {expected,true},
          {value,false}]},
 [{ring,'-loop/1-fun-0-',1,
        [{file,"/home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/src/ring.erl"},
         {line,70}]},
  {ring,loop,1,
        [{file,"/home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/src/ring.erl"},
         {line,70}]}]}
ring_tests: start_ring_test...*failed*
in function ring_tests:'-start_ring_test/0-fun-0-'/1 (/home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/test/ring_tests.erl, line 45)
**error:{assert,[{module,ring_tests},
         {line,45},
         {expression,"is_process_alive ( lists : last ( Nodes ) )"},
         {expected,false},
         {value,true}]}
  output:<<"">>

=======================================================
  Failed: 1.  Skipped: 0.  Passed: 3.
error
179> c(ring).         
Recompiling /home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/src/ring.erl
{ok,ring}
180> eunit:test(ring).
ring_tests: start_ring_test...*failed*
in function ring_tests:'-start_ring_test/0-fun-0-'/1 (/home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/test/ring_tests.erl, line 45)
**error:{assert,[{module,ring_tests},
         {line,45},
         {expression,"is_process_alive ( lists : last ( Nodes ) )"},
         {expected,false},
         {value,true}]}
  output:<<"">>

=======================================================
  Failed: 1.  Skipped: 0.  Passed: 3.
error
181> c(ring).         
Recompiling /home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/src/ring.erl
/home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/src/ring.erl:71: syntax error before: Parent
/home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/src/ring.erl:62: function loop/1 undefined
/home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/src/ring.erl:64: function loop/1 undefined
error
182> c(ring).         
Recompiling /home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/src/ring.erl
{ok,ring}
183> eunit:test(ring).
ring_tests: start_ring_test...*failed*
in function ring_tests:'-start_ring_test/0-fun-0-'/1 (/home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/test/ring_tests.erl, line 45)
**error:{assert,[{module,ring_tests},
         {line,45},
         {expression,"is_process_alive ( lists : last ( Nodes ) )"},
         {expected,false},
         {value,true}]}
  output:<<"">>

=======================================================
  Failed: 1.  Skipped: 0.  Passed: 3.
error
184> c(ring).         
Recompiling /home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/src/ring.erl
{ok,ring}
185> eunit:test(ring).
ring_tests: start_ring_test...*failed*
in function ring_tests:'-start_ring_test/0-fun-0-'/1 (/home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/test/ring_tests.erl, line 45)
**error:{assert,[{module,ring_tests},
         {line,45},
         {expression,"is_process_alive ( lists : last ( Nodes ) )"},
         {expected,false},
         {value,true}]}
  output:<<"">>

=======================================================
  Failed: 1.  Skipped: 0.  Passed: 3.
error
186> c(ring).         
Recompiling /home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/src/ring.erl
{ok,ring}
187> eunit:test(ring).
ring_tests: start_ring_test...*failed*
in function ring_tests:'-start_ring_test/0-fun-0-'/1 (/home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/test/ring_tests.erl, line 45)
**error:{assert,[{module,ring_tests},
         {line,45},
         {expression,"is_process_alive ( lists : last ( Nodes ) )"},
         {expected,false},
         {value,true}]}
  output:<<"">>

=======================================================
  Failed: 1.  Skipped: 0.  Passed: 3.
error
188> c(ring).         
Recompiling /home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/src/ring.erl
/home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/src/ring.erl:68: Warning: variable 'Child' is unused
{ok,ring}
189> eunit:test(ring).
ring_tests: start_ring_test...*failed*
in function ring_tests:'-start_ring_test/0-fun-0-'/1 (/home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/test/ring_tests.erl, line 45)
**error:{assert,[{module,ring_tests},
         {line,45},
         {expression,"is_process_alive ( lists : last ( Nodes ) )"},
         {expected,false},
         {value,true}]}
  output:<<"">>

ring_tests: when_node_receive_quit_message_then_then_node_and_the_parent_terminate_gracefully_test...*failed*
in function ring_tests:'-when_node_receive_quit_message_then_then_node_and_the_parent_terminate_gracefully_test/0-fun-0-'/1 (/home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/test/ring_tests.erl, line 36)
in call from ring_tests:when_node_receive_quit_message_then_then_node_and_the_parent_terminate_gracefully_test/0 (/home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/test/ring_tests.erl, line 36)
**error:{assert,[{module,ring_tests},
         {line,36},
         {expression,"is_process_alive ( NextNode )"},
         {expected,false},
         {value,true}]}
  output:<<"">>

=======================================================
  Failed: 2.  Skipped: 0.  Passed: 2.
error
190> c(ring).         
Recompiling /home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/src/ring.erl
/home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/src/ring.erl:45: Warning: variable 'Message' is unused
/home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/src/ring.erl:68: Warning: variable 'Child' is unused
{ok,ring}
191> eunit:test(ring).
ring_tests: start_ring_test...*failed*
in function ring_tests:'-start_ring_test/0-fun-0-'/1 (/home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/test/ring_tests.erl, line 45)
**error:{assert,[{module,ring_tests},
         {line,45},
         {expression,"is_process_alive ( lists : last ( Nodes ) )"},
         {expected,false},
         {value,true}]}
  output:<<"">>

ring_tests: when_node_receive_quit_message_then_then_node_and_the_parent_terminate_gracefully_test...*failed*
in function ring_tests:'-when_node_receive_quit_message_then_then_node_and_the_parent_terminate_gracefully_test/0-fun-0-'/1 (/home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/test/ring_tests.erl, line 36)
in call from ring_tests:when_node_receive_quit_message_then_then_node_and_the_parent_terminate_gracefully_test/0 (/home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/test/ring_tests.erl, line 36)
**error:{assert,[{module,ring_tests},
         {line,36},
         {expression,"is_process_alive ( NextNode )"},
         {expected,false},
         {value,true}]}
  output:<<"">>

=======================================================
  Failed: 2.  Skipped: 0.  Passed: 2.
error
192> c(ring).         
Recompiling /home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/src/ring.erl
/home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/src/ring.erl:45: Warning: variable 'Message' is unused
/home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/src/ring.erl:68: Warning: variable 'Child' is unused
{ok,ring}
193> eunit:test(ring).
ring_tests: start_ring_test...*failed*
in function ring_tests:'-start_ring_test/0-fun-0-'/1 (/home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/test/ring_tests.erl, line 45)
**error:{assert,[{module,ring_tests},
         {line,45},
         {expression,"is_process_alive ( lists : last ( Nodes ) )"},
         {expected,false},
         {value,true}]}
  output:<<"">>

ring_tests: when_node_receive_quit_message_then_then_node_and_the_parent_terminate_gracefully_test...*failed*
in function ring_tests:'-when_node_receive_quit_message_then_then_node_and_the_parent_terminate_gracefully_test/0-fun-0-'/1 (/home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/test/ring_tests.erl, line 36)
in call from ring_tests:when_node_receive_quit_message_then_then_node_and_the_parent_terminate_gracefully_test/0 (/home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/test/ring_tests.erl, line 36)
**error:{assert,[{module,ring_tests},
         {line,36},
         {expression,"is_process_alive ( NextNode )"},
         {expected,false},
         {value,true}]}
  output:<<"">>

=======================================================
  Failed: 2.  Skipped: 0.  Passed: 2.
error
194> c(ring).         
Recompiling /home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/src/ring.erl
/home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/src/ring.erl:45: Warning: variable 'Message' is unused
/home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/src/ring.erl:68: Warning: variable 'Child' is unused
{ok,ring}
195> eunit:test(ring).
ring_tests: start_ring_test...*failed*
in function ring_tests:'-start_ring_test/0-fun-0-'/1 (/home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/test/ring_tests.erl, line 45)
**error:{assert,[{module,ring_tests},
         {line,45},
         {expression,"is_process_alive ( lists : last ( Nodes ) )"},
         {expected,false},
         {value,true}]}
  output:<<"">>

ring_tests: when_node_receive_quit_message_then_then_node_and_the_parent_terminate_gracefully_test...*failed*
in function ring_tests:'-when_node_receive_quit_message_then_then_node_and_the_parent_terminate_gracefully_test/0-fun-0-'/1 (/home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/test/ring_tests.erl, line 36)
in call from ring_tests:when_node_receive_quit_message_then_then_node_and_the_parent_terminate_gracefully_test/0 (/home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/test/ring_tests.erl, line 36)
**error:{assert,[{module,ring_tests},
         {line,36},
         {expression,"is_process_alive ( NextNode )"},
         {expected,false},
         {value,true}]}
  output:<<"">>

=======================================================
  Failed: 2.  Skipped: 0.  Passed: 2.
error
196> c(ring).         
Recompiling /home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/src/ring.erl
/home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/src/ring.erl:45: Warning: variable 'Message' is unused
{ok,ring}
197> c(ring).         
Recompiling /home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/src/ring.erl
{ok,ring}
198> eunit:test(ring).
ring_tests: start_ring_test...*failed*
in function ring_tests:'-start_ring_test/0-fun-0-'/1 (/home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/test/ring_tests.erl, line 45)
**error:{assert,[{module,ring_tests},
         {line,45},
         {expression,"is_process_alive ( lists : last ( Nodes ) )"},
         {expected,false},
         {value,true}]}
  output:<<"">>

ring_tests: when_node_receive_quit_message_then_then_node_and_the_parent_terminate_gracefully_test...*failed*
in function ring_tests:'-when_node_receive_quit_message_then_then_node_and_the_parent_terminate_gracefully_test/0-fun-0-'/1 (/home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/test/ring_tests.erl, line 36)
in call from ring_tests:when_node_receive_quit_message_then_then_node_and_the_parent_terminate_gracefully_test/0 (/home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/test/ring_tests.erl, line 36)
**error:{assert,[{module,ring_tests},
         {line,36},
         {expression,"is_process_alive ( NextNode )"},
         {expected,false},
         {value,true}]}
  output:<<"">>

=======================================================
  Failed: 2.  Skipped: 0.  Passed: 2.
error
199> c(ring).         
Recompiling /home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/src/ring.erl
{ok,ring}
200> c(ring_tests).
Recompiling /home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/test/ring_tests.erl
{ok,ring_tests}
201> eunit:test(ring).
ring_tests: start_ring_test...*failed*
in function ring_tests:'-start_ring_test/0-fun-0-'/1 (/home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/test/ring_tests.erl, line 45)
**error:{assert,[{module,ring_tests},
         {line,45},
         {expression,"is_process_alive ( lists : nth ( 2 , Nodes ) )"},
         {expected,false},
         {value,true}]}
  output:<<"">>

ring_tests: when_node_receive_quit_message_then_then_node_and_the_parent_terminate_gracefully_test...*failed*
in function ring_tests:'-when_node_receive_quit_message_then_then_node_and_the_parent_terminate_gracefully_test/0-fun-0-'/1 (/home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/test/ring_tests.erl, line 36)
in call from ring_tests:when_node_receive_quit_message_then_then_node_and_the_parent_terminate_gracefully_test/0 (/home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/test/ring_tests.erl, line 36)
**error:{assert,[{module,ring_tests},
         {line,36},
         {expression,"is_process_alive ( NextNode )"},
         {expected,false},
         {value,true}]}
  output:<<"">>

=======================================================
  Failed: 2.  Skipped: 0.  Passed: 2.
error
202> c(ring_tests).   
Recompiling /home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/test/ring_tests.erl
/home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/test/ring_tests.erl:42: Warning: the result of the expression is ignored (suppress the warning by assigning the expression to the _ variable)
/home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/test/ring_tests.erl:42: Warning: variable 'Last' is unused
{ok,ring_tests}
203> eunit:test(ring).
ring_tests: start_ring_test...*failed*
in function ring_tests:'-start_ring_test/0-fun-0-'/1 (/home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/test/ring_tests.erl, line 46)
**error:{assert,[{module,ring_tests},
         {line,46},
         {expression,"is_process_alive ( lists : nth ( 2 , Nodes ) )"},
         {expected,false},
         {value,true}]}
  output:<<"">>

ring_tests: when_node_receive_quit_message_then_then_node_and_the_parent_terminate_gracefully_test...*failed*
in function ring_tests:'-when_node_receive_quit_message_then_then_node_and_the_parent_terminate_gracefully_test/0-fun-0-'/1 (/home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/test/ring_tests.erl, line 36)
in call from ring_tests:when_node_receive_quit_message_then_then_node_and_the_parent_terminate_gracefully_test/0 (/home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/test/ring_tests.erl, line 36)
**error:{assert,[{module,ring_tests},
         {line,36},
         {expression,"is_process_alive ( NextNode )"},
         {expected,false},
         {value,true}]}
  output:<<"">>

=======================================================
  Failed: 2.  Skipped: 0.  Passed: 2.
error
204> c(ring_tests).   
Recompiling /home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/test/ring_tests.erl
/home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/test/ring_tests.erl:42: Warning: the result of the expression is ignored (suppress the warning by assigning the expression to the _ variable)
/home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/test/ring_tests.erl:42: Warning: variable 'Last' is unused
{ok,ring_tests}
205> eunit:test(ring).
ring_tests: start_ring_test...*failed*
in function ring_tests:'-start_ring_test/0-fun-0-'/1 (/home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/test/ring_tests.erl, line 46)
**error:{assert,[{module,ring_tests},
         {line,46},
         {expression,"is_process_alive ( First )"},
         {expected,false},
         {value,true}]}
  output:<<"">>

ring_tests: when_node_receive_quit_message_then_then_node_and_the_parent_terminate_gracefully_test...*failed*
in function ring_tests:'-when_node_receive_quit_message_then_then_node_and_the_parent_terminate_gracefully_test/0-fun-0-'/1 (/home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/test/ring_tests.erl, line 36)
in call from ring_tests:when_node_receive_quit_message_then_then_node_and_the_parent_terminate_gracefully_test/0 (/home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/test/ring_tests.erl, line 36)
**error:{assert,[{module,ring_tests},
         {line,36},
         {expression,"is_process_alive ( NextNode )"},
         {expected,false},
         {value,true}]}
  output:<<"">>

=======================================================
  Failed: 2.  Skipped: 0.  Passed: 2.
error
206> c(ring_tests).   
Recompiling /home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/test/ring_tests.erl
/home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/test/ring_tests.erl:42: Warning: the result of the expression is ignored (suppress the warning by assigning the expression to the _ variable)
/home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/test/ring_tests.erl:42: Warning: variable 'Last' is unused
{ok,ring_tests}
207> eunit:test(ring).
ring_tests: when_node_receive_quit_message_then_then_node_and_the_parent_terminate_gracefully_test...*failed*
in function ring_tests:'-when_node_receive_quit_message_then_then_node_and_the_parent_terminate_gracefully_test/0-fun-0-'/1 (/home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/test/ring_tests.erl, line 36)
in call from ring_tests:when_node_receive_quit_message_then_then_node_and_the_parent_terminate_gracefully_test/0 (/home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/test/ring_tests.erl, line 36)
**error:{assert,[{module,ring_tests},
         {line,36},
         {expression,"is_process_alive ( NextNode )"},
         {expected,false},
         {value,true}]}
  output:<<"">>

=======================================================
  Failed: 1.  Skipped: 0.  Passed: 3.
error
208> c(ring).
Recompiling /home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/src/ring.erl
/home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/src/ring.erl:80: variable 'Child' is unbound
error
209> c(ring).
Recompiling /home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/src/ring.erl
{ok,ring}
210> c(ring_tests).,
210> c(ring_tests). 
* 1: syntax error before: '.'
210> c(ring_tests).
Recompiling /home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/test/ring_tests.erl
{ok,ring_tests}
211> eunit:test(ring).
  All 4 tests passed.
ok
212> c(ring).         
Recompiling /home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/src/ring.erl
/home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/src/ring.erl:46: function send_message/1 undefined
error
213> c(ring).
Recompiling /home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/src/ring.erl
{ok,ring}
214> eunit:test(ring).
  All 4 tests passed.
ok
215> c(ring_tests).   
Recompiling /home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/test/ring_tests.erl
{ok,ring_tests}
216> c(ring).         
Recompiling /home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/src/ring.erl
{ok,ring}
217> eunit:test(ring).
ring_tests: start_ring_test...*failed*
in function ring_tests:'-start_ring_test/0-fun-1-'/1 (/home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/test/ring_tests.erl, line 51)
**error:{assertEqual,[{module,ring_tests},
              {line,51},
              {expression,"Reply"},
              {expected,"hello"},
              {value,undefined}]}
  output:<<"">>

=======================================================
  Failed: 1.  Skipped: 0.  Passed: 3.
error
218> c(ring).         
Recompiling /home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/src/ring.erl
/home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/src/ring.erl:75: variable 'Message' is unbound
error
219> c(ring).         
Recompiling /home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/src/ring.erl
{ok,ring}
220> eunit:test(ring).
ring_tests: start_ring_test...*failed*
in function ring_tests:'-start_ring_test/0-fun-1-'/1 (/home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/test/ring_tests.erl, line 51)
**error:{assertEqual,[{module,ring_tests},
              {line,51},
              {expression,"Reply"},
              {expected,"hello"},
              {value,undefined}]}
  output:<<"">>

=======================================================
  Failed: 1.  Skipped: 0.  Passed: 3.
error
221> c(ring).         
Recompiling /home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/src/ring.erl
/home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/src/ring.erl:74: function sent_message/3 undefined
error
222> c(ring).         
Recompiling /home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/src/ring.erl
{ok,ring}
223> eunit:test(ring).
ring_tests: start_ring_test...*failed*
in function ring_tests:'-start_ring_test/0-fun-1-'/1 (/home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/test/ring_tests.erl, line 51)
**error:{assertEqual,[{module,ring_tests},
              {line,51},
              {expression,"Reply"},
              {expected,"hello"},
              {value,undefined}]}
  output:<<"">>

=======================================================
  Failed: 1.  Skipped: 0.  Passed: 3.
error
224> c(ring).         
Recompiling /home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/src/ring.erl
{ok,ring}
225> eunit:test(ring).
ring_tests: start_ring_test...*failed*
in function ring_tests:'-start_ring_test/0-fun-1-'/1 (/home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/test/ring_tests.erl, line 51)
**error:{assertEqual,[{module,ring_tests},
              {line,51},
              {expression,"Reply"},
              {expected,"hello"},
              {value,undefined}]}
  output:<<"">>

=======================================================
  Failed: 1.  Skipped: 0.  Passed: 3.
error
226> c(ring_tests).   
Recompiling /home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/test/ring_tests.erl
{ok,ring_tests}
227> eunit:test(ring).
ring_tests: start_ring_test...*failed*
in function ring_tests:'-start_ring_test/0-fun-1-'/1 (/home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/test/ring_tests.erl, line 51)
**error:{assertEqual,[{module,ring_tests},
              {line,51},
              {expression,"Reply"},
              {expected,"hello"},
              {value,undefined}]}
  output:<<"">>

=======================================================
  Failed: 1.  Skipped: 0.  Passed: 3.
error
228> c(ring_tests).   
Recompiling /home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/test/ring_tests.erl
{ok,ring_tests}
229> eunit:test(ring).
ring_tests: start_ring_test...*failed*
in function ring_tests:'-start_ring_test/0-fun-1-'/1 (/home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/test/ring_tests.erl, line 51)
**error:{assertEqual,[{module,ring_tests},
              {line,51},
              {expression,"Reply"},
              {expected,"hello"},
              {value,undefined}]}
  output:<<"">>

=======================================================
  Failed: 1.  Skipped: 0.  Passed: 3.
error
230> c(ring).         
Recompiling /home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/src/ring.erl
{ok,ring}
231> eunit:test(ring).
ring_tests: start_ring_test...*failed*
in function ring_tests:'-start_ring_test/0-fun-1-'/1 (/home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/test/ring_tests.erl, line 51)
**error:{assertEqual,[{module,ring_tests},
              {line,51},
              {expression,"Reply"},
              {expected,"hello"},
              {value,undefined}]}
  output:<<"">>

=======================================================
  Failed: 1.  Skipped: 0.  Passed: 3.
error
232> c(ring).         
Recompiling /home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/src/ring.erl
{ok,ring}
233> eunit:test(ring).

=ERROR REPORT==== 2-May-2018::22:18:06 ===
Error in process <0.3012.0> with exit value:
{badarg,[{erlang,is_process_alive,[undefined],[]},
         {ring,'-loop/1-fun-0-',1,
               [{file,"/home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/src/ring.erl"},
                {line,81}]},
         {ring,loop,1,
               [{file,"/home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/src/ring.erl"},
                {line,81}]}]}
ring_tests: start_ring_test...
=ERROR REPORT==== 2-May-2018::22:18:06 ===
Error in process <0.3015.0> with exit value:
{badarg,[{erlang,is_process_alive,[undefined],[]},
         {ring,'-loop/1-fun-0-',1,
               [{file,"/home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/src/ring.erl"},
                {line,81}]},
         {ring,loop,1,
               [{file,"/home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/src/ring.erl"},
                {line,81}]}]}
*failed*
in function ring_tests:'-start_ring_test/0-fun-1-'/1 (/home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/test/ring_tests.erl, line 51)
**error:{assertEqual,[{module,ring_tests},
              {line,51},
              {expression,"Reply"},
              {expected,"hello"},
              {value,undefined}]}
  output:<<"">>

=======================================================
  Failed: 1.  Skipped: 0.  Passed: 3.
error
234> i().
Pid                   Initial Call                          Heap     Reds Msgs
Registered            Current Function                     Stack              
<0.0.0>               otp_ring0:start/2                      376     1723    0
init                  init:loop/1                              2              
<0.1.0>               erts_code_purger:start/0               610  1297613    0
erts_code_purger      erts_code_purger:wait_for_request        0              
<0.2.0>               erts_literal_area_collector:start      233  1252258    0
                      erts_literal_area_collector:msg_l        5              
<0.3.0>               erts_dirty_process_code_checker:s      233      822    0
                      erts_dirty_process_code_checker:m        1              
<0.6.0>               erlang:apply/2                         987  1139142    0
erl_prim_loader       erl_prim_loader:loop/3                   5              
<0.32.0>              gen_event:init_it/6                   4185    69628    0
error_logger          gen_event:fetch_msg/6                   10              
<0.33.0>              erlang:apply/2                        1598     1343    0
application_controlle gen_server:loop/7                        7              
<0.35.0>              application_master:init/4              233      888    0
                      application_master:main_loop/2           7              
<0.36.0>              application_master:start_it/4          233      909    0
                      application_master:loop_it/4             5              
<0.37.0>              supervisor:kernel/1                    610     3123    0
kernel_sup            gen_server:loop/7                       10              
<0.38.0>              erlang:apply/2                        4185   259236    0
code_server           code_server:loop/1                       3              
<0.40.0>              rpc:init/1                             233      851    0
rex                   gen_server:loop/7                       10              
<0.41.0>              global:init/1                          233      882    0
global_name_server    gen_server:loop/7                       10              
<0.42.0>              erlang:apply/2                         233      844    0
                      global:loop_the_locker/1                 5              
<0.43.0>              erlang:apply/2                         233      822    0
                      global:loop_the_registrar/0              2              
<0.44.0>              inet_db:init/1                         376     1234    0
inet_db               gen_server:loop/7                       10              
<0.46.0>              global_group:init/1                    233      893    0
global_group          gen_server:loop/7                       10              
<0.47.0>              file_server:init/1                    1598   232528    0
file_server_2         gen_server:loop/7                       10              
<0.48.0>              gen_event:init_it/6                    233      870    0
erl_signal_server     gen_event:fetch_msg/6                   10              
<0.49.0>              supervisor_bridge:standard_error/      233      869    0
standard_error_sup    gen_server:loop/7                       10              
<0.50.0>              erlang:apply/2                         233      830    0
standard_error        standard_error:server_loop/1             2              
<0.51.0>              supervisor_bridge:user_sup/1           233      893    0
                      gen_server:loop/7                       10              
<0.52.0>              user_drv:server/2                     2586   300251    0
user_drv              user_drv:server_loop/6                   9              
<0.53.0>              group:server/3                         376     1293    0
user                  group:server_loop/3                      4              
<0.54.0>              group:server/3                        4185   492875    0
                      group:server_loop/3                      4              
<0.55.0>              kernel_config:init/1                   233      866    0
                      gen_server:loop/7                       10              
<0.56.0>              supervisor:kernel/1                    233      902    0
kernel_safe_sup       gen_server:loop/7                       10              
<0.58.0>              erlang:apply/2                        6772    71051    0
                      shell:shell_rep/4                       17              
<0.61.0>              erlang:apply/2                       10958   622297    0
                      c:pinfo/1                               50              
<0.65.0>              erlang:apply/2                        2586     8273    0
eunit_server          eunit_server:main/1                      3              
<0.2980.0>            ring:init/1                            233 31329002    1
                      erlang:setelement/3                      1              
<0.2981.0>            ring:init/1                            233       16    0
                      ring:loop/1                              5              
<0.2983.0>            ring:init/1                            233       11    0
                      ring:loop/1                              5              
<0.2984.0>            ring:init/1                            233       11    0
                      ring:loop/1                              5              
<0.3017.0>            ring:init/1                            233 84116147    1
                      erlang:setelement/3                      1              
<0.3018.0>            ring:init/1                            233        9    0
                      ring:loop/1                              5              
<0.3020.0>            ring:init/1                            233        4    0
                      ring:loop/1                              5              
<0.3021.0>            ring:init/1                            233        4    0
                      ring:loop/1                              5              
Total                                                      47347 39798277    2
                                                             283              
ok
235> c(ring).         
Recompiling /home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/src/ring.erl
{ok,ring}
236> eunit:test(ring).

=ERROR REPORT==== 2-May-2018::22:19:50 ===
Error in process <0.3050.0> with exit value:
{badarg,[{erlang,is_process_alive,[undefined],[]},
         {ring,'-loop/1-fun-0-',1,
               [{file,"/home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/src/ring.erl"},
                {line,81}]},
         {ring,loop,1,
               [{file,"/home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/src/ring.erl"},
                {line,81}]}]}
ring_tests: start_ring_test...
=ERROR REPORT==== 2-May-2018::22:19:50 ===
Error in process <0.3053.0> with exit value:
{badarg,[{erlang,is_process_alive,[undefined],[]},
         {ring,'-loop/1-fun-0-',1,
               [{file,"/home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/src/ring.erl"},
                {line,81}]},
         {ring,loop,1,
               [{file,"/home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/src/ring.erl"},
                {line,81}]}]}
*failed*
in function ring_tests:'-start_ring_test/0-fun-1-'/1 (/home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/test/ring_tests.erl, line 51)
**error:{assertEqual,[{module,ring_tests},
              {line,51},
              {expression,"Reply"},
              {expected,"hello"},
              {value,undefined}]}
  output:<<"">>

=======================================================
  Failed: 1.  Skipped: 0.  Passed: 3.
error
237> c(ring).         
Recompiling /home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/src/ring.erl
{ok,ring}
238> eunit:test(ring).

=ERROR REPORT==== 2-May-2018::22:21:06 ===
Error in process <0.3087.0> with exit value:
{badarg,[{erlang,is_process_alive,[undefined],[]},
         {ring,'-loop/1-fun-0-',1,
               [{file,"/home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/src/ring.erl"},
                {line,81}]},
         {ring,loop,1,
               [{file,"/home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/src/ring.erl"},
                {line,81}]}]}
ring_tests: start_ring_test...
=ERROR REPORT==== 2-May-2018::22:21:06 ===
Error in process <0.3090.0> with exit value:
{badarg,[{erlang,is_process_alive,[undefined],[]},
         {ring,'-loop/1-fun-0-',1,
               [{file,"/home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/src/ring.erl"},
                {line,81}]},
         {ring,loop,1,
               [{file,"/home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/src/ring.erl"},
                {line,81}]}]}
*failed*
in function ring_tests:'-start_ring_test/0-fun-1-'/1 (/home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/test/ring_tests.erl, line 51)
**error:{assertEqual,[{module,ring_tests},
              {line,51},
              {expression,"Reply"},
              {expected,"hello"},
              {value,undefined}]}
  output:<<"">>

=======================================================
  Failed: 1.  Skipped: 0.  Passed: 3.
error
239> c(ring).         
Recompiling /home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/src/ring.erl
/home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/src/ring.erl:66: Warning: variable 'LastMessage' is unused
{ok,ring}
240> eunit:test(ring).

=ERROR REPORT==== 2-May-2018::22:22:00 ===
Error in process <0.3124.0> with exit value:
{badarg,[{erlang,is_process_alive,[undefined],[]},
         {ring,'-loop/1-fun-0-',1,
               [{file,"/home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/src/ring.erl"},
                {line,82}]},
         {ring,loop,1,
               [{file,"/home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/src/ring.erl"},
                {line,82}]}]}

=ERROR REPORT==== 2-May-2018::22:22:00 ===
Error in process <0.3127.0> with exit value:
{badarg,[{erlang,is_process_alive,[undefined],[]},
         {ring,'-loop/1-fun-0-',1,
               [{file,"/home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/src/ring.erl"},
                {line,82}]},
         {ring,loop,1,
               [{file,"/home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/src/ring.erl"},
                {line,82}]}]}
ring_tests: start_ring_test...*failed*
in function ring_tests:'-start_ring_test/0-fun-1-'/1 (/home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/test/ring_tests.erl, line 51)
**error:{assertEqual,[{module,ring_tests},
              {line,51},
              {expression,"Reply"},
              {expected,"hello"},
              {value,undefined}]}
  output:<<"">>

=======================================================
  Failed: 1.  Skipped: 0.  Passed: 3.
error
241> c(ring).         
Recompiling /home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/src/ring.erl
/home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/src/ring.erl:67: Warning: variable 'LastMessage' is unused
{ok,ring}
242> eunit:test(ring).

=ERROR REPORT==== 2-May-2018::22:22:17 ===
Error in process <0.3161.0> with exit value:
{badarg,[{erlang,is_process_alive,[undefined],[]},
         {ring,'-loop/1-fun-0-',1,
               [{file,"/home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/src/ring.erl"},
                {line,83}]},
         {ring,loop,1,
               [{file,"/home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/src/ring.erl"},
                {line,83}]}]}
ring_tests: start_ring_test...
=ERROR REPORT==== 2-May-2018::22:22:17 ===
Error in process <0.3164.0> with exit value:
{badarg,[{erlang,is_process_alive,[undefined],[]},
         {ring,'-loop/1-fun-0-',1,
               [{file,"/home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/src/ring.erl"},
                {line,83}]},
         {ring,loop,1,
               [{file,"/home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/src/ring.erl"},
                {line,83}]}]}
*failed*
in function ring_tests:'-start_ring_test/0-fun-1-'/1 (/home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/test/ring_tests.erl, line 51)
**error:{assertEqual,[{module,ring_tests},
              {line,51},
              {expression,"Reply"},
              {expected,"hello"},
              {value,undefined}]}
  output:<<"">>

=======================================================
  Failed: 1.  Skipped: 0.  Passed: 3.
error
243> c(ring).         
Recompiling /home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/src/ring.erl
/home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/src/ring.erl:68: Warning: variable 'LastMessage' is unused
{ok,ring}
244> eunit:test(ring).

=ERROR REPORT==== 2-May-2018::22:23:10 ===
Error in process <0.3198.0> with exit value:
{badarg,[{erlang,is_process_alive,[undefined],[]},
         {ring,'-loop/1-fun-0-',1,
               [{file,"/home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/src/ring.erl"},
                {line,84}]},
         {ring,loop,1,
               [{file,"/home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/src/ring.erl"},
                {line,84}]}]}
ring_tests: start_ring_test...
=ERROR REPORT==== 2-May-2018::22:23:10 ===
Error in process <0.3201.0> with exit value:
{badarg,[{erlang,is_process_alive,[undefined],[]},
         {ring,'-loop/1-fun-0-',1,
               [{file,"/home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/src/ring.erl"},
                {line,84}]},
         {ring,loop,1,
               [{file,"/home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/src/ring.erl"},
                {line,84}]}]}
*failed*
in function ring_tests:'-start_ring_test/0-fun-1-'/1 (/home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/test/ring_tests.erl, line 51)
**error:{assertEqual,[{module,ring_tests},
              {line,51},
              {expression,"Reply"},
              {expected,"hello"},
              {value,undefined}]}
  output:<<"">>

=======================================================
  Failed: 1.  Skipped: 0.  Passed: 3.
error
245> c(ring).         
Recompiling /home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/src/ring.erl
/home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/src/ring.erl:70: Warning: variable 'LastMessage' is unused
{ok,ring}
246> eunit:test(ring).

=ERROR REPORT==== 2-May-2018::22:23:22 ===
Error in process <0.3235.0> with exit value:
{badarg,[{erlang,is_process_alive,[undefined],[]},
         {ring,'-loop/1-fun-0-',1,
               [{file,"/home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/src/ring.erl"},
                {line,86}]},
         {ring,loop,1,
               [{file,"/home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/src/ring.erl"},
                {line,86}]}]}
ring_tests: start_ring_test...
=ERROR REPORT==== 2-May-2018::22:23:22 ===
Error in process <0.3238.0> with exit value:
{badarg,[{erlang,is_process_alive,[undefined],[]},
         {ring,'-loop/1-fun-0-',1,
               [{file,"/home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/src/ring.erl"},
                {line,86}]},
         {ring,loop,1,
               [{file,"/home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/src/ring.erl"},
                {line,86}]}]}
*failed*
in function ring_tests:'-start_ring_test/0-fun-1-'/1 (/home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/test/ring_tests.erl, line 51)
**error:{assertEqual,[{module,ring_tests},
              {line,51},
              {expression,"Reply"},
              {expected,"hello"},
              {value,undefined}]}
  output:<<"">>

=======================================================
  Failed: 1.  Skipped: 0.  Passed: 3.
error
247> c(ring).         
Recompiling /home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/src/ring.erl
/home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/src/ring.erl:64: Warning: variable 'Child' is unused
/home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/src/ring.erl:64: Warning: variable 'LastMessage' is unused
{ok,ring}
248> eunit:test(ring).
ring_tests: start_ring_test...*failed*
in function ring_tests:'-start_ring_test/0-fun-0-'/1 (/home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/test/ring_tests.erl, line 50)
in call from ring_tests:start_ring_test/0 (/home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/test/ring_tests.erl, line 50)
**error:{assert,[{module,ring_tests},
         {line,50},
         {expression,"is_process_alive ( Last )"},
         {expected,false},
         {value,true}]}
  output:<<"">>

ring_tests: when_node_receive_quit_message_then_then_node_and_the_parent_terminate_gracefully_test...*failed*
in function ring_tests:'-when_node_receive_quit_message_then_then_node_and_the_parent_terminate_gracefully_test/0-fun-0-'/1 (/home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/test/ring_tests.erl, line 36)
in call from ring_tests:when_node_receive_quit_message_then_then_node_and_the_parent_terminate_gracefully_test/0 (/home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/test/ring_tests.erl, line 36)
**error:{assert,[{module,ring_tests},
         {line,36},
         {expression,"is_process_alive ( NextNode )"},
         {expected,false},
         {value,true}]}
  output:<<"">>

=======================================================
  Failed: 2.  Skipped: 0.  Passed: 2.
error
249> c(ring).         
Recompiling /home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/src/ring.erl
/home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/src/ring.erl:46: Warning: variable 'Last' is unused
/home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/src/ring.erl:64: Warning: variable 'Child' is unused
/home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/src/ring.erl:64: Warning: variable 'LastMessage' is unused
{ok,ring}
250> c(ring).         
Recompiling /home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/src/ring.erl
/home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/src/ring.erl:64: Warning: variable 'Child' is unused
{ok,ring}
251> c(ring).
Recompiling /home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/src/ring.erl
{ok,ring}
252> eunit:test(ring).
ring_tests: start_ring_test...*failed*
in function ring_tests:'-start_ring_test/0-fun-0-'/1 (/home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/test/ring_tests.erl, line 50)
in call from ring_tests:start_ring_test/0 (/home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/test/ring_tests.erl, line 50)
**error:{assert,[{module,ring_tests},
         {line,50},
         {expression,"is_process_alive ( Last )"},
         {expected,false},
         {value,true}]}
  output:<<"">>

ring_tests: when_node_receive_quit_message_then_then_node_and_the_parent_terminate_gracefully_test...*failed*
in function ring_tests:'-when_node_receive_quit_message_then_then_node_and_the_parent_terminate_gracefully_test/0-fun-0-'/1 (/home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/test/ring_tests.erl, line 36)
in call from ring_tests:when_node_receive_quit_message_then_then_node_and_the_parent_terminate_gracefully_test/0 (/home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/test/ring_tests.erl, line 36)
**error:{assert,[{module,ring_tests},
         {line,36},
         {expression,"is_process_alive ( NextNode )"},
         {expected,false},
         {value,true}]}
  output:<<"">>

=======================================================
  Failed: 2.  Skipped: 0.  Passed: 2.
error
253> c(ring_test).    
{error,non_existing}
254> c(ring_tests).
Recompiling /home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/test/ring_tests.erl
{ok,ring_tests}
255> eunit:test(ring).
ring_tests: start_ring_test...*failed*
in function ring_tests:'-start_ring_test/0-fun-1-'/1 (/home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/test/ring_tests.erl, line 51)
**error:{assertEqual,[{module,ring_tests},
              {line,51},
              {expression,"Reply"},
              {expected,"hello"},
              {value,undefined}]}
  output:<<"">>

ring_tests: when_node_receive_quit_message_then_then_node_and_the_parent_terminate_gracefully_test...*failed*
in function ring_tests:'-when_node_receive_quit_message_then_then_node_and_the_parent_terminate_gracefully_test/0-fun-0-'/1 (/home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/test/ring_tests.erl, line 36)
in call from ring_tests:when_node_receive_quit_message_then_then_node_and_the_parent_terminate_gracefully_test/0 (/home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/test/ring_tests.erl, line 36)
**error:{assert,[{module,ring_tests},
         {line,36},
         {expression,"is_process_alive ( NextNode )"},
         {expected,false},
         {value,true}]}
  output:<<"">>

=======================================================
  Failed: 2.  Skipped: 0.  Passed: 2.
error
256> c(ring_tests).   
Recompiling /home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/test/ring_tests.erl
{ok,ring_tests}
257> c(ring_tests).   
Recompiling /home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/test/ring_tests.erl
{ok,ring_tests}
258> c(ring).      
Recompiling /home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/src/ring.erl
{ok,ring}
259> eunit:test(ring).
ring_tests: start_ring_test...*failed*
in function ring_tests:'-start_ring_test/0-fun-1-'/1 (/home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/test/ring_tests.erl, line 51)
**error:{assertEqual,[{module,ring_tests},
              {line,51},
              {expression,"Reply"},
              {expected,"hello"},
              {value,undefined}]}
  output:<<"">>

ring_tests: when_node_receive_quit_message_then_then_node_and_the_parent_terminate_gracefully_test...*failed*
in function ring_tests:'-when_node_receive_quit_message_then_then_node_and_the_parent_terminate_gracefully_test/0-fun-0-'/1 (/home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/test/ring_tests.erl, line 36)
in call from ring_tests:when_node_receive_quit_message_then_then_node_and_the_parent_terminate_gracefully_test/0 (/home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/test/ring_tests.erl, line 36)
**error:{assert,[{module,ring_tests},
         {line,36},
         {expression,"is_process_alive ( NextNode )"},
         {expected,false},
         {value,true}]}
  output:<<"">>

=======================================================
  Failed: 2.  Skipped: 0.  Passed: 2.
error
260> c(ring).         
Recompiling /home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/src/ring.erl
{ok,ring}
261> eunit:test(ring).
ring_tests: start_ring_test...*failed*
in function ring_tests:'-start_ring_test/0-fun-1-'/1 (/home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/test/ring_tests.erl, line 51)
**error:{assertEqual,[{module,ring_tests},
              {line,51},
              {expression,"Reply"},
              {expected,"hello"},
              {value,undefined}]}
  output:<<"">>

ring_tests: when_node_receive_quit_message_then_then_node_and_the_parent_terminate_gracefully_test...*failed*
in function ring_tests:'-when_node_receive_quit_message_then_then_node_and_the_parent_terminate_gracefully_test/0-fun-0-'/1 (/home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/test/ring_tests.erl, line 36)
in call from ring_tests:when_node_receive_quit_message_then_then_node_and_the_parent_terminate_gracefully_test/0 (/home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/test/ring_tests.erl, line 36)
**error:{assert,[{module,ring_tests},
         {line,36},
         {expression,"is_process_alive ( NextNode )"},
         {expected,false},
         {value,true}]}
  output:<<"">>

=======================================================
  Failed: 2.  Skipped: 0.  Passed: 2.
error
262> c(ring).         
Recompiling /home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/src/ring.erl
{ok,ring}
263> eunit:test(ring).
ring_tests: start_ring_test...*failed*
in function ring_tests:'-start_ring_test/0-fun-1-'/1 (/home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/test/ring_tests.erl, line 51)
**error:{assertEqual,[{module,ring_tests},
              {line,51},
              {expression,"Reply"},
              {expected,"hello"},
              {value,undefined}]}
  output:<<"">>

ring_tests: when_node_receive_quit_message_then_then_node_and_the_parent_terminate_gracefully_test...*failed*
in function ring_tests:'-when_node_receive_quit_message_then_then_node_and_the_parent_terminate_gracefully_test/0-fun-0-'/1 (/home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/test/ring_tests.erl, line 36)
in call from ring_tests:when_node_receive_quit_message_then_then_node_and_the_parent_terminate_gracefully_test/0 (/home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/test/ring_tests.erl, line 36)
**error:{assert,[{module,ring_tests},
         {line,36},
         {expression,"is_process_alive ( NextNode )"},
         {expected,false},
         {value,true}]}
  output:<<"">>

=======================================================
  Failed: 2.  Skipped: 0.  Passed: 2.
error
264> c(ring).         
Recompiling /home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/src/ring.erl
{ok,ring}
265> eunit:test(ring).
ring_tests: start_ring_test...*failed*
in function ring_tests:'-start_ring_test/0-fun-1-'/1 (/home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/test/ring_tests.erl, line 51)
**error:{assertEqual,[{module,ring_tests},
              {line,51},
              {expression,"Reply"},
              {expected,"hello"},
              {value,undefined}]}
  output:<<"">>

ring_tests: when_node_receive_quit_message_then_then_node_and_the_parent_terminate_gracefully_test...*failed*
in function ring_tests:'-when_node_receive_quit_message_then_then_node_and_the_parent_terminate_gracefully_test/0-fun-0-'/1 (/home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/test/ring_tests.erl, line 36)
in call from ring_tests:when_node_receive_quit_message_then_then_node_and_the_parent_terminate_gracefully_test/0 (/home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/test/ring_tests.erl, line 36)
**error:{assert,[{module,ring_tests},
         {line,36},
         {expression,"is_process_alive ( NextNode )"},
         {expected,false},
         {value,true}]}
  output:<<"">>

=======================================================
  Failed: 2.  Skipped: 0.  Passed: 2.
error
266> c(ring).         
Recompiling /home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/src/ring.erl
{ok,ring}
267> eunit:test(ring).
ring_tests: start_ring_test...*failed*
in function ring_tests:'-start_ring_test/0-fun-1-'/1 (/home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/test/ring_tests.erl, line 51)
**error:{assertEqual,[{module,ring_tests},
              {line,51},
              {expression,"Reply"},
              {expected,"hello"},
              {value,undefined}]}
  output:<<"">>

ring_tests: when_node_receive_quit_message_then_then_node_and_the_parent_terminate_gracefully_test...*failed*
in function ring_tests:'-when_node_receive_quit_message_then_then_node_and_the_parent_terminate_gracefully_test/0-fun-0-'/1 (/home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/test/ring_tests.erl, line 36)
in call from ring_tests:when_node_receive_quit_message_then_then_node_and_the_parent_terminate_gracefully_test/0 (/home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/test/ring_tests.erl, line 36)
**error:{assert,[{module,ring_tests},
         {line,36},
         {expression,"is_process_alive ( NextNode )"},
         {expected,false},
         {value,true}]}
  output:<<"">>

=======================================================
  Failed: 2.  Skipped: 0.  Passed: 2.
error
268> c(ring).         
Recompiling /home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/src/ring.erl
{ok,ring}
269> eunit:test(ring).
ring_tests: start_ring_test...*failed*
in function ring_tests:'-start_ring_test/0-fun-1-'/1 (/home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/test/ring_tests.erl, line 51)
**error:{assertEqual,[{module,ring_tests},
              {line,51},
              {expression,"Reply"},
              {expected,"hello"},
              {value,undefined}]}
  output:<<"">>

ring_tests: when_node_receive_quit_message_then_then_node_and_the_parent_terminate_gracefully_test...*failed*
in function ring_tests:'-when_node_receive_quit_message_then_then_node_and_the_parent_terminate_gracefully_test/0-fun-0-'/1 (/home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/test/ring_tests.erl, line 36)
in call from ring_tests:when_node_receive_quit_message_then_then_node_and_the_parent_terminate_gracefully_test/0 (/home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/test/ring_tests.erl, line 36)
**error:{assert,[{module,ring_tests},
         {line,36},
         {expression,"is_process_alive ( NextNode )"},
         {expected,false},
         {value,true}]}
  output:<<"">>

=======================================================
  Failed: 2.  Skipped: 0.  Passed: 2.
error
270> c(ring).         
Recompiling /home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/src/ring.erl
{ok,ring}
271> eunit:test(ring).
ring_tests: start_ring_test...*failed*
in function ring_tests:'-start_ring_test/0-fun-1-'/1 (/home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/test/ring_tests.erl, line 51)
**error:{assertEqual,[{module,ring_tests},
              {line,51},
              {expression,"Reply"},
              {expected,"hello"},
              {value,undefined}]}
  output:<<"">>

ring_tests: when_node_receive_quit_message_then_then_node_and_the_parent_terminate_gracefully_test...*failed*
in function ring_tests:'-when_node_receive_quit_message_then_then_node_and_the_parent_terminate_gracefully_test/0-fun-0-'/1 (/home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/test/ring_tests.erl, line 36)
in call from ring_tests:when_node_receive_quit_message_then_then_node_and_the_parent_terminate_gracefully_test/0 (/home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/test/ring_tests.erl, line 36)
**error:{assert,[{module,ring_tests},
         {line,36},
         {expression,"is_process_alive ( NextNode )"},
         {expected,false},
         {value,true}]}
  output:<<"">>

=======================================================
  Failed: 2.  Skipped: 0.  Passed: 2.
error
272> c(ring_tests).   
Recompiling /home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/test/ring_tests.erl
{ok,ring_tests}
273> eunit:test(ring).
ring_tests: when_node_receive_quit_message_then_then_node_and_the_parent_terminate_gracefully_test...*failed*
in function ring_tests:'-when_node_receive_quit_message_then_then_node_and_the_parent_terminate_gracefully_test/0-fun-0-'/1 (/home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/test/ring_tests.erl, line 36)
in call from ring_tests:when_node_receive_quit_message_then_then_node_and_the_parent_terminate_gracefully_test/0 (/home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/test/ring_tests.erl, line 36)
**error:{assert,[{module,ring_tests},
         {line,36},
         {expression,"is_process_alive ( NextNode )"},
         {expected,false},
         {value,true}]}
  output:<<"">>

=======================================================
  Failed: 1.  Skipped: 0.  Passed: 3.
error
274> c(ring_tests).   
Recompiling /home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/chain-hook/test/ring_tests.erl
{ok,ring_tests}
275> eunit:test(ring).
  All 4 tests passed.
ok
276>  q().
ok
277> rosemary@SCUBA:[231]~/../chain-hook$ 


