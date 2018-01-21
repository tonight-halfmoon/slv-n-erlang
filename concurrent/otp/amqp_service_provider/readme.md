# AMQP Service Provider

AMQP Service Provider is a AMQP Client that implements [Erlang RabbitMQ Client library](http://www.rabbitmq.com/erlang-client-user-guide.html) to define an Exchange and a Queue of another AMQP Client of choice. The purpose of this client is to help in demonstrating the sceanrio of [GenRS's Integration Test](../genrs/integration.test/test/ait.erl). I do not see a meaning of this client implementation in real-world scenario.

# Future Work

## Runtime Configuration for Non-default Behaviour of Exchange and Queue

Currently, a default behaviour of exchange and queue is defined in [amqp_service_provider.app](./ebin/amqp_service_provider.app). In case a client requires a non-default behaviour of exchange and queue then it must be configurable in runtime.

## Distributed Nodes

There won't be any chance for a system to be a real game unless it can play a role in distributed setting. The challenge is to take this element and the containing prototype to a real game as a distributed system composed of nodes which communicated with nodes and publish and store data to other nodes.