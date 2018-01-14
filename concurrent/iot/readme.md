

## Development Time

### Step-0 

1. RabbitMQ Server is installed
1. amqp_client application and its dependencies [release artifacts](http://www.rabbitmq.com/releases/rabbitmq-erlang-client/) are downloaded or its source code [git repository](https://github.com/rabbitmq/rabbitmq-erlang-client.git) is cloned and built 

Once release artifacts of `amqp_client` including: [`rabbitmq_client`](http://www.rabbitmq.com/releases/rabbitmq-erlang-client/v3.6.14/amqp_client-3.6.14.ez), [`rabbit_common`](http://www.rabbitmq.com/releases/rabbitmq-erlang-client/v3.6.14/rabbit_common-3.6.14.ez) and [`recon`](http://www.rabbitmq.com/releases/rabbitmq-erlang-client/v3.6.14/recon-2.3.2.ez) are downloaded, let's open up our packages:

1. unzip the `.ez` archives

```
ln -s rabbitmq_client-<version> rabbitmq_client
ln -s rabbit_common-<version> rabbit_common
ln -s recon-<version> recon
```

0. Environment Variables
> Archlinux

0. add a `.sh` bash script to /etc/profile.d with the following contents
ERL_LIBS=/path/to/amqp_client/release-artifacts/root

1. Start RabbitMQ Server

> ArchLinux

$ sudo rabbitmq-server

> include_lib

Same like, when we provide `-include_lib("eunit/include/eunit.hrl")` to let our modules find out Erlang eunit application, we provide `-include_lib("amqp_client/include/amqp_client.hrl")` to each module utilises application `ampq_client`.

By now, we are for programming against RabbitMQ Broker.

### Management Plugin

[Management Plugin](https://www.rabbitmq.com/management.html)

>

0. sudo rabbitmq-plugins enable rabbitmq_management
1. localhost:15672

>

Credentials:
geust; guest
