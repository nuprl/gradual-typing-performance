k-cfa
=====

Simple k-CFA implementation from Matt Might's [blog](http://matt.might.net/articles/implementation-of-kcfa-and-0cfa/).

Notes
-----
- Approx. 300 lines of code stretched very thin across modules
- Uses many adapters because many modules introduced structs
  - Porting was easy, knowing the adapter method ahead of time
  - Adapters saved some `require/typed/check` annotations in other modules
