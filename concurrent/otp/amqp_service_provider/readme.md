# AMQP Service Provider

AMQP Service Provider is a AMQP Client that implements [Erlang RabbitMQ Client library](http://www.rabbitmq.com/erlang-client-user-guide.html) to define an Exchange and a Queue of another AMQP Client of choice.

# Future Work

## Runtime Configuration for Non-default Behaviour of Exchange and Queue

Currently, a default behaviour of exchange and queue is defined in [amqp_service_provider.app](./ebin/amqp_service_provider.app). In case a client requires a non-default behaviour of exchange and queue then it must be configurable in runtime.