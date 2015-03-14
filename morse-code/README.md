morse-code
==========

John Clement's [morse code trainer](https://github.com/jbclements/morse-code-trainer), minus the I/O.

Run `main.rkt` to generate a few random sequences of English words, convert them to morse code, and compare the original strings' Levenshtein distance 

- `main.rkt` contains some constants, some tools for making random strings, and the actual test
- `morse-code-table.rkt` hash table dictating morse code conversion
- `morse-code-string.rkt` convert an A-Z string to morse code

History
-------

The original morse code trainer used the RSound library to play a random morse code word.
Then it read a line of user input and compared that value with the expected word.
It also had some neat tools for picking words to focus on.

(And thankfully, the original project was already partially typed.)

