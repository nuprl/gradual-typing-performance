htdp
====

Script for rendering a version of How To Design Programs.
Running `main.rkt` invokes the renderer and generates an HTML document from the scribble sources in `base/`.

Files are:
- `x-info.rkt` paths and helper functions for rendering
- `xnotes.rkt` functions for compiling the notes
- `xhtml.rkt` functions for compiling the main textbook
- `main.rkt` invokes `xhtml.rkt` to render the document

This benchmark was proposed because it uses mixins, but they actually play a very small role.
They pass untouched through the typed (and untyped) code.
