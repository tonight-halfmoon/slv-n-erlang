# A Supervised Math Server - A Minimal Supervisor with Connected Clients
This supervisor extends the minimal supervisor capability. If the module of a child is not available, the supervisor tries to start the child a maximum 5 times per minute. In addition, the supervisor handles two types of children. Transient and permanenet. If a child is transient and it terminate normally, then the supervisor does not restart the child. The supervisor restarts a transient child only if the child terminates abnormally.

