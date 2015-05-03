Sieve
=====

Simple implementation of the Sieve of Eratosthenes.

Implemented using two modules:
- `stream.rkt` implements a simple stream library based on lambdas
- `main.rkt` implements the Sieve using the streams library

The file `main.rkt` depends on `streams.rkt`; `streams.rkt` only depends on the base libraries.

Running `racket main.rkt` will compute a large prime number (1,000th or 10,000th) and print it.
Gradual typing has significant performance costs, but the untyped and fully-typed versions run fine.
