
% The first node
 
~/..synchronous$ erl -name t -pa ebin/

t@localhost.jpat.org)23> Pid = spawn('t@localhost.jpat.org', math_server, loop, [fun geomrtry:areas/1]).

% The second node
%
$ erl -name r

 (r@localhost.jpat.org)21> net_kernel:connect_node('t@localhost.jpat.org').

true

% the first node

(t@localhost.jpat.org)23> global:register_name(math_server1, Pid, fun global:notify_all_name/3). 

yes

% the second node
(r@localhost.jpat.org)19> global:registered_names().
[math_server1]


(r@localhost.jpat.org)20> global:send(math_server1, {print, [{circle, 3}]}).
<8088.104.0>


% or Use Module 'rpc' . read kernel.pdf
