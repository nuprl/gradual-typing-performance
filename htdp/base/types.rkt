#lang typed/racket

(provide Tag Block part)

(require/typed
 scribble/core
 [#:opaque Tag tag?]
 [#:opaque Block block?]
 [#:struct part ([tag-prefix : (U #f String)]
                 [tags : (Listof Tag)]
                 [title-content : (U #f (Listof Any))]
                 [style : Any] ;; "A style property can be anything, including a symbol or a structure such as color-property." http://docs.racket-lang.org/scribble/core.html?q=scribble%2Frender#%28tech._style._property%29
                 [to-collect : (Listof Any)]
                 [blocks : (Listof Block)]
                 [parts : (Listof part)])])

