#lang typed/racket

(require/typed/provide
 scribble/core
 #;[#:opaque Element-Style element-style?]
 [#:opaque Tag tag?] ; should fix
 [#:opaque Block block?] ; should fix
 [#:opaque Content content?] ; should fix
 [#:struct style
           ([name : (U String Symbol False)]
            [properties : (Listof Any)])
           #:extra-constructor-name make-style]
 [#:struct part
           ([tag-prefix : (Option String)]
            [tags : (Listof Tag)]
            [title-content : (Option (Listof Content))]
            [style : style]
            [to-collect : (Listof Any)]
            [blocks : (Listof Block)]
            [parts : (Listof part)])
           #:extra-constructor-name make-part]
 [#:struct element
           ([style : (U String Symbol False style)]
            [content : Content])
           #:extra-constructor-name make-element]
 [#:struct multiarg-element
           ([style : (U String Symbol False)]
            [contents : (Listof Content)])
           #:extra-constructor-name make-multiarg-element]
 [#:struct collected-info
           ([number : (Listof (U False Natural String))]
            [parent : (Option part)]
            [info : Any])]
 [#:struct resolve-info
           ([ci : Any]
            [delays : Any]
            [undef : Any]
            [searches : Any])
           #:extra-constructor-name make-resolve-info]
 [#:struct table
           ([style : style]
            [blockss : (Listof (Listof (U Block 'cont)))])
           #:extra-constructor-name make-table]
 [#:struct itemization
           ([style : style]
            [blockss : (Listof (Listof Block))])
           #:extra-constructor-name make-itemization]
 [#:struct target-url
           ([addr : Path-String])
           #:extra-constructor-name make-target-url]
 [#:struct nested-flow
           ([style : Any]
            [blocks : (Listof Block)])
            #:extra-constructor-name make-nested-flow]
 [#:struct collect-info
           ([fp : Any]
            [ht  : Any]
            [ext-ht : Any]
            [ext-demand : (Tag collect-info -> Any)]
            [parts : Any]
            [tags : Any]
            [gen-prefix : Any]
            [relatives : Any]
            [parents : (Listof part)])
           #:extra-constructor-name make-collect-info]
 [#:struct paragraph 
           ([style : style]
            [content : Content])
           #:extra-constructor-name make-paragraph]
 [#:struct compound-paragraph
           ([style : style]
            [blocks : (Listof Block)])
           #:extra-constructor-name make-compound-paragraph]
 [#:struct delayed-block
           ([resolve : (Any part resolve-info -> Block)])
           #:extra-constructor-name make-delayed-block]
 [#:struct traverse-block
           ([traverse : (Rec BTP ((Symbol Any -> Any) (Symbol Any -> Any) -> (U BTP Block)))])
           #:extra-constructor-name make-traverse-block]
 [part-collected-info (part resolve-info -> collected-info)])

