Echo
====

Simple echo server, taken from the Shootout Benchmarks.
Run `main.rkt` to test -- starts a client and a server to read the client's messages.
Terminates after the client has made 200,000 transmissions.

Contains 4 simple files:
- `constants.rkt` defines constants for the client and server
- `client.rkt` exports a function simulating a TCP client who spams the port
- `server.rkt` exports a function simulating a TCP server, reads contents of a port
- `main.rkt` starts a client and server, prints server's running time

The dependency graph is a simple diamond.
Both `client.rkt` and `server.rkt` depend on `constants.rkt`.
The main module depends on the client and server.

Gradual typing adds very little overhead.
