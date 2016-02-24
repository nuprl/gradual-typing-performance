macro-intro
===

Do we trace macro-introduced identifiers?

1. Create a helper module with an identifier & a macro that references the ID
2. Create a main module with a macro introducing refs to the ID
3. Call both macros in the main

We don't if the macro came from the library file
