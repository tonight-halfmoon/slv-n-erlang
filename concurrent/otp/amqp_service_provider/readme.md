# AMQP Service Provider

AMQP Service Provider is a AMQP Client that implements [Erlang RabbitMQ Client library](http://www.rabbitmq.com/erlang-client-user-guide.html) to define an Exchange and a Queue of another AMQP Client of choice.

# Future Work

## Configuration of Exchange and Queue

Currently, exchange and queue names are defined in [amqp_service_provider.app](./ebin/amqp_service_provider.app). It must be configurable in runtime to give a chance for a client to pass desired names.