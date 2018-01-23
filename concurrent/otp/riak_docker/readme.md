
The Learning is directed by [basho Blog](http://basho.com/posts/technical/running-riak-in-docker/).

> Make a Riak-TS Docker

```
Option 0:

The most flexible and most configurable method is to [BYOC](https://github.com/basho-labs/riak-docker).
```

However, for learning purposes, this be going step by step and learn the building units, let's start with the most elementary options.

```
Step 0: Install Docker

> Archlinux
Execute the following commands:

1.  sudo pacman -S docker
2.  system enable docker.service
3.  system start docker.service
4.  systemctl status docker.service
```

```
Option 1:
sudo docker run --name=riak -d -p 8087:8087 -p 8098:8098 basho/riak-ts
```

```
Option 2:
sudo docker run --name=riak -d -P -v $(pwd)/schemas:/etc/riak/schemas -p 8087:8087 -p 8098:8098 basho/riak-ts

 -  Check the absolute path of $(pwd)/schemas is the one intended. Replace $(pwd) with the absolute value.

In directory $(pwd)/schemas, the definition of table(s) and buckets. In this directory, we could define table [GeoCheckin.sql](./example/schemas/GeoCheckin.sql).
```

> Check IP
sudo docker inspect -f '{{.NetworkSettings.IPAddress}}:8087' riak | tr '\\n' , | sed 's/,$//'

> Enter Docker Container System
sudo docker exec -it riak bash

> Interact with riak-shell
Inside docker container riak: riak-shell describe GeoCheckin;
Since we haven't passed riak_shell.conf at docker container initialise time. The default, value is riak@127.0.0.1 is set. And for that when we enter the riak-shell the first time in the docker container riak-shell it gives us message 'disconnected'. To configure that either pass a related volume likewise we passed the schemas definition or us ad-hoc method. Launch the bash shell of docker container riak and set the value to 172.17.0.2 or the IP you got for the docker outbound in /etc/riak/riak_shell.conf.

> Riak Explorer

On host machine:

   http://172.17.0.2:8098/admin/