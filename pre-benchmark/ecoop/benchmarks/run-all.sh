#!/usr/bin/env bash

racket setup.rkt acquire
racket setup.rkt gofish

racket run.rkt -c acquire main.rkt
racket run.rkt -c gofish runner.rkt

racket run.rkt -i 30 -l acquire.png -o acquire.rktd acquire main.rkt
racket run.rkt -i 30 -l gofish.png -o gofish.rktd gofish runner.rkt
