Sieve
=====

Simple implementation of the Sieve of Eratosthenes.
Implemented using two modules:
- `stream.rkt` implements a simple stream library based on lambdas
- `primes.rkt` implements the Sieve using the streams library

The file `primes.rkt` depends on `streams.rkt`; `streams.rkt` only depends on the base libraries.

Running `racket primes.rkt` will compute the 10,000th prime number and print it to console.
At last check, this took about 10 seconds to run.
