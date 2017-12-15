# Prototype on Distributed System

In Erlang a beautiful game on how processes on distributed nodes communicate

## Goal

To learn building a distributed system. The prototype shows how processes on different nodes can communiate with each other only by sending and receiving messages.

## Setting

The Server / client design assume they are on nodes of the same domain. 

## Protocols

Client is given certain public protocols to send requests to Server. No knowledge is given to client about Server's internal Communication protocols.

## Server Internal Data Model

Server's data model and data structure are kept hidden from client. One occasion for example, the record '#res_ds'. It is defined in 'interface_server.hrl' and only utilised by module 'resa_server' and the internal data handler, i.e., module 'handler'.

## Server As a Network of Communicating Processes

Server is composed of service implementors. The server receives messages from client. Then, the different business interfaces are implemented by internally defined modules. For example, Handler module's process manages the server's data and implements the allocate and free resources. Another example, Stats provider which implements the process which is responsible to compute some kind of statistics on the data model. Server delegates such requests and these implementors do the job and sends back the relevant message.

## Integration Test

To ensure the communication happens between the client and server, an integration test module is provided. In addition, the integration test checks the messages passing between Server's internal processes.

## Future Work

### Integration Test

It must confirm the server's internal modules. It still lacks to assert the internal protocols.

### Security

This prototype is not aware of any security concept, but one. Internal protocols are hidden from the User Interface of client. Only server's internal processes can request data models.

### Internet 

Nodes reside on machines of different domain. 

### OTP

This is a pure Erlang implementation without pursuing OTP. The purpose is to cover many topics like net_kernel, net_adm and etc. Applying OTP will come with a prototype on Internet of Things IoT soon, with MQTT broker.

