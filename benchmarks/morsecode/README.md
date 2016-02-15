morsecode
==========

John Clement's [morse code trainer](https://github.com/jbclements/morse-code-trainer), minus the I/O.

Converts many words to morse code and compares many pairs of words for their Levenshtein distance.

Summary
-------

- `main.rkt` contains some constants, some tools for making random strings, and the actual test
- `morse-code-table.rkt` hash table dictating morse code conversion
- `morse-code-string.rkt` convert an A-Z string to morse code

- `frequency.rktd` 75,000 characters
- `frequency-small.rktd` 5,000 characters

History
-------

The original morse code trainer used the RSound library to play a random morse code word.
Then it read a line of user input and compared that value with the expected word.
It also had some neat tools for picking words to focus on.

(And thankfully, the original project was already partially typed.)
