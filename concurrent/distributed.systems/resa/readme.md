# Prototype on Distributed System

In Erlang a beautiful game on how processes on distributed nodes communicate

## Goal

To learn building a distributed system. The prototype shows how processes on different nodes can communiate with each other only by sending and receiving messages.

## Setting

The Server / client design assume they are on nodes of the same domain. 

## Integration Test

To ensure the communication happens between the client and server, an integration test module is provided.

## Future Work

### Integration Test

It must confirm the server's internal modules. It still lacks to assert the internal protocols.

### Security

This prototype is not aware of any security concept, but one. Internal protocols are hidden from the User Interface of client. Only internal server's processes can request data models.

### Internet 

Nodes reside on machines of different domain. 

### OTP

This is a pure Erlang implementation without pursuing OTP. The purpose is to cover many topics like net_kernel, net_adm and etc. Applying OTP will come with a prototype on Internet of Things IoT soon, with MQTT broker.

