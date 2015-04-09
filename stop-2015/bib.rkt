#lang at-exp racket
(require scribble/manual
         scriblib/autobib)

(provide (all-defined-out))

;; shortens names
(abbreviate-given-names #t)

;; ----------------------------------------

(define short? #t)
(define-syntax define/short
  (syntax-rules ()
    [(_ i e e*) (define i (if short? e e*))]
    [(_ i e) (define i e)]))

(define IEEE "IEEE ")
(define ACM "ACM ")
(define International "Intl. ")
(define Conference "Conf. ")
(define Workshop "Wksp. ")
(define Journal "J. ")
(define Symposium "Sym. ")
(define Transactions "Trans. ")


(define/short asplas "APLAS" (string-append "Asian " Symposium "Programming Languages and Systems"))
(define/short fpca "FPCA" (string-append ACM International Conference "Functional Programming Languages and Computer Architecture"))
(define/short icfp "ICFP" (string-append ACM International Conference "Functional Programming"))
(define/short pldi "PLDI" (string-append ACM Conference "Programming Language Design and Implementation"))
(define/short popl "POPL" (string-append ACM Symposium "Principles of Programming Languages"))
(define/short lncs "LNCS" "Lecture Notes in Computer Science")
(define/short sigplan-notices "SIGPLAN Notices" (string-append ACM "SIGPLAN Notices"))
(define/short scheme-workshop "SFP" (string-append Workshop "Scheme and Functional Programming"))
(define/short jfp "JFP" (string-append Journal "Functional Programming"))
(define/short hosc "HOSC" "Higher-Order and Symbolic Programming")
(define/short lfp "LFP" "LISP and Functional Programming")
(define/short lsc "LSC" "LISP and Symbolic Computation")
(define/short ifl "IFL" (string-append International Symposium "Functional and Logic Programming"))
(define/short tfp "TFP" (string-append Symposium "Trends in Functional Programming"))
(define/short ecoop "ECOOP" (string-append "European " Conference "Object-Oriented Programming"))
(define/short oopsla "OOPSLA" (string-append ACM Conference "Object-Oriented Programming, Systems, Languages and Applications"))
(define/short ieee-software (string-append IEEE "Software"))
(define/short toplas "TOPLAS" (string-append Transactions "Programming Languages and Systems"))
(define/short dls "DLS" "Dynamic Languages Symposium")
(define/short flops "FLOPS" (string-append Symposium "Functional and Logic Programming"))
(define/short esop "ESOP" (string-append "European " Symposium "on Programming"))
(define/short cc "CC" (string-append International Conference "on Compiler Construction"))
(define/short iclp "ICLP" (string-append  International Conference "on Logic Programming"))
(define/short fse "FSE" (string-append International Symposium "on the Foundations of Software Engineering"))
(define/short aosd "AOSD" (string-append International Conference "on Aspect-Oriented Software Development"))
(define/short foal "FOAL" "Foundations of Aspect-Oriented Languages")
(define/short tlca "TLCA" (string-append International Conference "Typed Lambda Calculi and Applications"))
(define/short i&c "Info. & Comp." "Information and Computation")
(define/short haskell "Haskell Workshop")
(define/short tcs "Theoretical Computer Science")
(define/short tacs (string-append International Symposium "Theoretical Aspects of Computer Science"))
(define/short ml-workshop "ML Workshop")
(define/short sac "SAC" (string-append Symposium "on Applied Computing"))
(define/short gpce "GPCE" "Generative Programming: Concepts & Experiences")
(define/short dyla "DYLA" (string-append Workshop "on Dynamic Languages and Applications"))

;; ----------------------------------------

(define TypedRacket
  (make-bib
   #:author (authors "Sam Tobin-Hochstadt" "Matthias Felleisen")
   #:title "The Design and Implementation of Typed Scheme"
   #:location (proceedings-location popl
                                    #:pages '(395 406))
   #:date "2008"))

(define Contracts
  (make-bib
   #:author (authors "Robert Bruce Findler" "Matthias Felleisen")
   #:title @elem{Contracts for Higher-Order Functions}
   #:location (proceedings-location icfp
                                    #:pages '(48 59))
   #:date "2002"))

(define KillSafety
  (make-bib
   #:author (authors "Matthew Flatt" "Robert Bruce Findler")
   #:title @elem{Kill-Safe Synchronization Abstractions}
   #:location (proceedings-location pldi
                                    #:pages '(47 58))
   #:date "2004"))

(define JavascriptProxies
  (make-bib
   #:author (authors (author-name "Tom" "Van Cutsem") "Mark Miller")
   #:title @elem{Proxies: Design Principles for Robust Object-oriented Intercession APIs}
   #:location (proceedings-location "Dynamic Languages Symposium"
                                    #:pages '(59 72))
   #:date "2010"))

(define DirectProxies
  (make-bib
   #:author (authors (author-name "Tom" "Van Cutsem") "Mark Miller")
   #:title @elem{On the design of the ECMAScript Reflection API}
   #:location (techrpt-location #:institution "Vrije Universiteit Brussel"
				#:number "VUB-SOFT-TR-12-03")
   #:date "2012"))


(define grf:lazy-contracts
  (make-bib
   #:author (authors "Robert Bruce Findler" "Shu-yu Guo" "Anne Rogers")
   #:title "Lazy Contract Checking for Immutable Data Structures"
   #:location (proceedings-location "Implementation and Application of Functional Languages"
                                    #:pages '(111 128))
   #:date "2007"))

(define hjl:haskell-contracts
  (make-bib
   #:title "Typed Contracts for Functional Programming"
   #:author (authors "Ralf Hinze" "Johan Jeuring" "Andres Löh")
   #:location (proceedings-location flops
                                    #:pages '(208 225))
   #:date "2006"))

(define cmr:lazy-assertions
  (make-bib
   #:author (authors "Olaf Chitil" "Dan McNeill" "Colin Runciman")
   #:title "Lazy Assertions"
   #:date "2003"
   #:location (proceedings-location ifl
                                    #:pages '(1 19))))

(define ch:lazy-assertions
  (make-bib
   #:title "A pattern logic for prompt lazy assertions"
   #:author (authors "Olaf Chitil" "Frank Huch")
   #:location (proceedings-location ifl
                                    #:pages '(126 144))
   #:date "2006"))

(define MOP
  (make-bib
   #:author (authors "Gregor J. Kiczales"
                     (author-name "James" "des Rivieres")
                     "Daniel G. Bobrow")
   #:is-book? #t
   #:title "The Art of the Metaobject Protocol"
   #:location (book-location #:publisher "MIT Press")
   #:date "1991"))

(define AOP
  (make-bib
   #:author (authors "Gregor Kiczales"
                     "John Lamping"
                     "Anurag Mendhekar"
                     "Chris Maeda"
                     "Cristina Lopes"
                     "Jean-Marc Loingtier"
                     "John Irwin")
   #:date "1997"
   #:title "Aspect-Oriented Programming"
   #:location (proceedings-location ecoop
                                    #:pages '(220 242))
   ;#:note "LNCS 1241"
   ))

(define |Can Aspects Implement Contracts?|
  (make-bib
   #:author (authors "Stephanie Balzer"
                     "Patrick Eugster"
                     "Bertrand Meyer")
   #:title "Can Aspects Implement Contracts?"
   #:date "2005"
   #:location (proceedings-location
               "Rapid Implemetation of Software Engineering Techniques"
               #:pages '(145 157))))

(define java.lang.reflect.Proxy
  (make-bib
   #:date 2000
   #:author "Oracle"
   #:title @elem{java.lang.reflect.Proxy}
   #:url "http://download.oracle.com/javase/6/docs/api/java/lang/reflect/Proxy.html"))

(define gal-firefox
  (make-bib
   #:date 2010
   #:author "Andreas Gal"
   #:title "Proxies in Tracemonkey"
   #:url "http://hg.mozilla.org/tracemonkey/"))


(define Eiffel
  (make-bib
   #:author "Bertrand Meyer"
   #:title "Eiffel : The Language"
   #:is-book? #t
   #:date "1991"
   #:location (book-location #:publisher "Prentice Hall PTR")))

(define AmbientTalk
  (make-bib
   #:title "Ambient-Oriented Programming"
   #:author (authors "Jessie Dedecker"
                     (author-name "Tom" "Van Cutsem")
                     "Stijn Mostinckx"
                     "Theo D'Hondt"
                     (author-name "Wolfgang" "De Meuter"))
   #:date "2005"
   #:location (proceedings-location oopsla
                                    #:pages '(31 40))))

(define Euclid
  (make-bib
   #:author (authors "B. W. Lampson"
                     "J. J. Horning"
                     "R. L. London"
                     "J. G. Mitchell"
                     "G. J. Popek")
   #:title "Report on the programming language Euclid"
   #:location (journal-location sigplan-notices
                                #:volume 12
                                #:number 2
                                #:pages '(1 79))
   #:date "1977"))


(define Anna
  (make-bib
   #:author (authors "D. C. Luckham"
                     "F. W. von Henke")
   #:title "An overview of Anna, a specification language for Ada"
   #:location (journal-location ieee-software
                                #:volume 2
                                #:number 2
                                #:pages '(9 22))
   #:date "1985"))


(define D
  (make-bib
   #:author (org-author-name "Digital Mars")
   #:date "1999"
   #:title "D Programming Language"
   #:url "http://www.digitalmars.com/d/"))


(define |Operational Semantics for Multi-Language Programs|
  (make-bib
   #:title "Operational Semantics for Multi-Language Programs"
   #:author (authors "Jacob Matthews"
                     "Robert Bruce Findler")
   #:date "2009"
   #:location (journal-location toplas
                                #:volume 31
                                #:number 3
                                #:pages '("12:1" "12:44"))))

(define |Space Efficient Gradual Typing|
  (make-bib
   #:title "Space Efficient Gradual Typing"
   #:author (authors "David Herman" "Aaron Tomb" "Cormac Flanagan")
   #:location (proceedings-location tfp)
   #:date "2007"))

(define |Nested and Dynamic Contract Boundaries|
  (make-bib
   #:title "Nested and Dynamic Contract Boundaries"
   #:author (authors "T. Stephen Strickland"
                     "Matthias Felleisen")
   #:location (proceedings-location ifl
                                    #:pages '(141 158))
   #:date "2009"))

 (define ClassContracts
   (make-bib
    #:title "Contracts for First-Class Classes"
    #:author (authors "T. Stephen Strickland"
                     "Matthias Felleisen")
    #:location (proceedings-location dls
                                     #:pages '(97 112))
    #:date "2010"))

(define vvle-tr
  (make-bib
   #:title "Virtual Values for Language Extension"
   #:author (authors "Thomas H. Austin" "Tim Disney" "Cormac Flanagan")
   #:location (proceedings-location oopsla)
   #:date "2011"))

(define ContractsCoffee
  (make-bib
   #:title "Contracts.coffee"
   #:author "Tim Disney"
   #:date "2012"
   #:url "http://disnetdev.com/contracts.coffee/"))

(define redex-book
  (make-bib
    #:author (authors "Matthias Felleisen" "Robert Bruce Findler" "Matthew Flatt")
    #:title "Semantics Engineering with PLT Redex"
    #:location (book-location #:publisher "MIT Press")
    #:is-book? #t
    #:date "2010"))

(define sfp2009-kf
  (make-bib
   #:author (authors "Casey Klein" "Robert Bruce Findler")
   #:title "Randomized Testing in PLT Redex"
   #:location (proceedings-location scheme-workshop #:pages '(26 36))
   #:date "2009"))

(define poly-sealing
  (make-bib
   #:title "Parametric Polymorphism Through Run-Time Sealing, or, Theorems for Low, Low Prices!"
   #:author (authors "Jacob Matthews" "Amal Ahmed")
   #:location (proceedings-location esop #:series "LNCS 4960" #:pages '(16 31))
   #:date 2008))

(define ciao-contracts
  (make-bib
   #:author (authors "E. Mera" "P. Lopez-Garcia" "M. Hermenegildo")
   #:title "Integrating Software Testing and Run-Time Checking in an Assertion Verification Framework"
   #:location (proceedings-location iclp #:series "LNCS 5649")
   #:date 2009))

(define no-more-scapegoating
  (make-bib
   #:title "Correct Blame for Contracts: No More Scapegoating"
   #:author (authors "Christos Dimoulas" "Robert Bruce Findler" "Cormc Flanagan" "Matthias Felleisen")
   #:location (proceedings-location popl)
   #:date 2011))

(define harmless-advice
  (make-bib
   #:title "Harmless Advice"
   #:author (authors "Daniel S. Dantas" "David Walker")
   #:location (proceedings-location popl)
   #:date 2006))

(define aspect-classification
  (make-bib
   #:title "A Classification System and Analysis for Aspect-Oriented Programs"
   #:author (authors "Martin Rinard" "Alexandru Salcianu" "Suhabe Bugrara")
   #:location (proceedings-location fse)
   #:date 2004))

(define observers-and-assistants
  (make-bib
   #:author (authors "Curtis Clifton" "Gary T. Leavens")
   #:title "Observers and assistants: A proposal for modular aspect-oriented reasoning"
   #:location (proceedings-location foal)
   #:date 2002))

(define plt-tr1
  (make-bib
   #:title    "Reference: Racket"
   #:author   (authors "Matthew Flatt" "PLT")
   #:date     "2010"
   #:location (techrpt-location #:institution "PLT Inc." #:number "PLT-TR-2010-1")
   #:url      "http://racket-lang.org/tr1/"))

(define plt-tr3
  (make-bib
   #:title "GUI: Racket Graphics Toolkit"
   #:author (authors "Matthew Flatt" "Robert Bruce Findler" "John Clements")
   #:date     "2010"
   #:location (techrpt-location #:institution "PLT Inc." #:number "PLT-TR-2010-3")
   #:url      "http://racket-lang.org/tr3/"))

(define EffectiveAdvice
  (make-bib
   #:title "EffectiveAdvice: Disciplined Advice with Explicit Effects"
   #:author (authors (author-name "Bruno C. d. S." "Oliveira")
                     "Tom Schrijvers"
                     "William R. Cook")
   #:date 2010
   #:location (proceedings-location aosd)))

(define OpenModules
  (make-bib
   #:title "Open Modules: Modular Reasoning About Advice"
   #:author "Jonathan Aldrich"
   #:date 2005
   #:location (proceedings-location ecoop)))

(define MillerPhD
  (make-bib
   #:title "Robust Composition: Towards a Unified Approach to Access Control and Concurrency Control"
   #:author "M. S. Miller"
   #:is-book? #t
   #:location (dissertation-location #:institution "John Hopkins University")
   #:date 2006))

(define js-type-inference
  (make-bib #:title "Fast and precise type inference for JavaScript"
            #:author (authors (author-name "Brian" "Hackett")
                              (author-name "Shu-Yu" "Guo"))
            #:location (proceedings-location pldi) ; TODO pages
            #:date "2012"))

(define min-cover-salient-curves
  (make-bib
   #:author (authors "Pedro Felzenszwalb" "David McAllester")
   #:title "A min-cover approach for finding salient curves"
   #:location (proceedings-location (string-append IEEE Workshop "Perceptual Organization in Computer Vision"))
   #:date 2006))

(define type-soundness
  (make-bib
   #:author (authors "Andrew K. Wright" "Matthias Felleisen")
   #:title "A Syntactic Approach to Type Soundness"
   #:location (journal-location i&c #:pages '(38 94))
   #:date 1994))

(define expressive-power
  (make-bib
   #:author (authors "Matthias Felleisen")
   #:title "On the Expressive Power of Programming Languages"
   #:location (journal-location "Science of Computer Programming"
                                #:volume 17
                                #:number "1--3"
                                #:pages '(35 75))
   #:date 1991))

;; ----------------------------------------
; Software engineering

(define crtr-empirical-2013
  (make-bib
   #:author (authors "Oscar Callaú" "Romain Robbes"
                     "Éric Tanter" "David Röthlisberger")
   #:title "How (and Why) Developers Use the Dynamic Features of Programming Languages: the Case of Smalltalk"
   #:location (journal-location "Empirical Software Engineering"
                                #:volume 18
                                #:number 6
                                #:pages '(1156 1194))
   #:date 2013))

;; ----------------------------------------
; Misc

(define cm-tech-1985
  (make-bib
   #:author (authors "Robert L. Constable" "Nax P. Mendler")
   #:title "Recursive Definitions in Type Theory"
   #:location (techrpt-location #:institution "Cornell University"
                                #:number "TR 85-659")
   #:date 1985))

(define mff-popl-2006
  (make-bib
   #:author (authors "Phillipe Meunier" "Robert Bruce Findler" "Matthias Felleisen")
   #:title "Modular Set-Based Analysis from Contracts"
   #:location (proceedings-location popl #:pages '(218 231))
   #:date 2006))

;; ----------------------------------------
; Software engineering and types

(define hkrts-empirical-2013
  (make-bib
   #:author (authors "Stefan Hanenberg" "Sebastian Kleinschmager"
                     "Romain Robbes" "Éric Tanter" "Andreas Stefik")
   #:title "An Empirical Study on the Impact of Static Typing on Software Maintainability"
   #:location (journal-location "Empirical Software Engineering"
                                ;; there really isn't any volume/number listed on Springer
                                #:pages '(1 48))
   #:date 2013))

;; ----------------------------------------
; Objects, theory

(define ac-book-1996
  (make-bib
   #:author (authors "Martin Abadi" "Luca Cardelli")
   #:title "A Theory of Objects"
   #:date 1996
   #:location (book-location #:publisher "Springer-Verlag")))

;; ----------------------------------------
; Objects, real languages

(define remy-tacs-1994
  (make-bib
    #:author "Didier Rémy"
    #:title "Programming Objects with ML-ART an Extension to ML with Abstract and Record Types"
    #:date 1994
    #:location (proceedings-location tacs #:pages '(321 346))))

(define oacddemmmsssz-tech-2006
  (make-bib
   #:title "An Overview of the Scala Programming Language"
   #:author (authors "Martin Odersky" "Philippe Altherr"
                     "Vincent Cremet" "Iulian Dragos"
                     "Gilles Dubochet" "Burak Emir"
                     "Sean McDirmid" "Stéphane Micheloud"
                     "Nikolay Mihaylov" "Michel Schinz"
                     "Erik Stenman" "Lex Spoon"
                     "Matthias Zenger")
   #:date 2006
   #:location (techrpt-location #:institution "École Polytechnique Fédérale de Lausanne"
                                #:number "LAMP-REPORT-2006-001")))

(define mme-gpce-2013
  (make-bib
   #:title "Template Constructors for Resuable Object Initialization"
   #:author (authors "Marko Martin" "Mira Mezini" "Sebastian Erdweg")
   #:location (proceedings-location gpce #:pages '(43 52))
   #:date 2013))

;; ----------------------------------------
; Macrology

(define ts-tcs-2000
  (make-bib
   #:title "MetaML and Multi-stage Programming with Explicit Annotations"
   #:author (authors "Walid Taha" "Tim Sheard")
   #:location (journal-location tcs
                                #:volume 248
                                #:number "1-2"
                                #:pages '(211 242))
   #:date 2000))

(define spj-haskell-2002
  (make-bib
   #:title "Template Meta-programming for Haskell"
   #:author (authors "Tim Sheard" "Simon Peyton Jones")
   #:location (proceedings-location haskell)
   #:date 2002))

(define burmako-scala-2013
  (make-bib
   #:title "Scala Macros: Let Our Powers Combine!"
   #:author "Eugene Burmako"
   #:location (proceedings-location "Scala Workshop")
   #:date 2013))

;; ----------------------------------------
; Contracts

(define complete-monitors
  (make-bib
   #:author (authors "Christos Dimoulas" "Sam Tobin-Hochstadt" "Matthias Felleisen")
   #:title "Complete Monitors for Behavioral Contracts"
   #:location (proceedings-location esop #:pages '(214 233))
   #:date 2012))

(define ho-contract-satisfaction
  (make-bib
   #:author (authors "Christos Dimoulas" "Matthias Felleisen")
   #:title "On Contract Satisfaction in a Higher-Order World"
   #:location (journal-location toplas
                                #:volume 33
                                #:number 5
                                #:pages '("16:1" "16:29"))
   #:date 2011))

(define dimoulas-diss
  (make-bib
   #:author "Christos Dimoulas"
   #:title "Foundations for Behavioral Higher-Order Contracts"
   #:is-book? #t
   #:location (dissertation-location #:institution "Northeastern University")
   #:date 2012))

;; ----------------------------------------
; Proxies

(define chaperones-impersonators
  (make-bib
   #:author (authors "T. Stephen Strickland" "Sam Tobin-Hochstadt" "Robert Bruce Findler" "Matthew Flatt")
   #:title "Chaperones and Impersonators: Run-time Support for Reasonable Interposition"
   #:location (proceedings-location oopsla #:pages '(943 962))
   #:date 2012))

;; ----------------------------------------
; Continuations

(define ConstrainingControl
  (make-bib
   #:author (authors "Daniel P. Friedman" "Christopher T. Haynes")
   #:title "Constraining Control"
   #:location (proceedings-location popl #:pages '(245 254))
   #:date 1985))

(define ContinuationMultiprocessing
  (make-bib
   #:author (authors "Mitchell Wand")
   #:title "Continuation-Based Multiprocessing"
   #:location (journal-location hosc
                                #:volume 12
                                #:number 3
                                #:pages '(285 299))
   #:date 1999))

(define Engines
  (make-bib
   #:author (authors "Christopher T. Haynes" "Daniel P. Friedman")
   #:title "Engines Build Process Abstractions"
   #:location (proceedings-location lfp #:pages '(18 24))
   #:date 1984))

(define Coroutines
  (make-bib
   #:author (authors "Christopher T. Haynes" "Daniel P. Friedman" "Mitchell Wand")
   #:title "Continuations and Coroutines"
   #:location (proceedings-location lfp #:pages '(293 298))
   #:date 1984))

(define GuilePrompts
  (make-bib
   #:author "Free Software Foundation"
   #:date 2012
   #:title "Guile Reference Manual: Prompts"
   #:url "http://www.gnu.org/software/guile/manual/html_node/Prompts.html"))

;; ----------------------------------------
; Continuation marks / dynamic binding

(define algebraic-stepper
  (make-bib
   #:author (authors "John Clements" "Matthew Flatt" "Matthias Felleisen")
   #:title "Modeling an Algebraic Stepper"
   #:location (proceedings-location esop #:pages '(320 334))
   #:date 2001))

(define DDBinding
  (make-bib
   #:author (authors "Oleg Kiselyov" "Chung-chieh Shan" "Amr Sabry")
   #:title "Delimited Dynamic Binding"
   #:location (proceedings-location icfp #:pages '(26 37))
   #:date 2006))

(define GenStackInspection
  (make-bib
   #:author (authors "Greg Pettyjohn" "John Clements" "Joe Marshall"
                     "Shriram Krishnamurthi" "Matthias Felleisen")
   #:title "Continuations from Generalized Stack Inspection"
   #:location (proceedings-location icfp #:pages '(216 227))
   #:date 2005))

(define AOPinHOLs
  (make-bib
   #:author (authors "David B. Tucker" "Shriram Krishnamurthi")
   #:title "Pointcuts and Advice in Higher-Order Languages"
   #:location (proceedings-location aosd #:pages '(158 167))
   #:date 2003))

(define CMsinJS
  (make-bib
   #:author (authors "John Clements" "Ayswarya Sundaram" "David Herman")
   #:title "Implementing Continuation Marks in Javascript"
   #:location (proceedings-location scheme-workshop)
   #:date 2008))

(define clements-diss
  (make-bib
   #:title "Portable and High-level Access to the Stack with Continuation Marks"
   #:author "John Clements"
   #:is-book? #t
   #:location (dissertation-location #:institution "Northeastern University")
   #:date 2006))

;; ----------------------------------------
; Delim control

(define Felleisen88
  (make-bib
   #:author (authors "Matthias Felleisen")
   #:title "The Theory and Practice of First-Class Prompts"
   #:location (proceedings-location popl #:pages '(180 190))
   #:date 1988))

(define Sitaram1993
  (make-bib
   #:author (authors "Dorai Sitaram")
   #:title "Handling Control"
   #:location (proceedings-location pldi #:pages '(147 155))
   #:date 1993))

(define DelimCompControl
  (make-bib
   #:author (authors "Matthew Flatt" "Gang Yu"
                     "Robert Bruce Findler" "Matthias Felleisen")
   #:title "Adding Delimited and Composable Control to a Production Programming Environment"
   #:location (proceedings-location icfp #:pages '(165 176))
   #:date 2007))

(define Subcontinuations
  (make-bib
   #:author (authors "Robert Hieb" "R. Kent Dybvig" "Claude W. Anderson")
   #:title "Subcontinuations"
   #:location (journal-location lsc #:pages '(83 110))
   #:date 1994))

(define DelimiterHierarchies
  (make-bib
   #:author (authors "Dorai Sitaram" "Matthias Felleisen")
   #:title "Control Delimiters and their Hierarchies"
   #:location (journal-location lsc #:pages '(67 99))
   #:date 1990))

(define MachIV
  (make-bib
   #:author "Richard P. Draves"
   #:title "Control Transfer in Operating System Kernels"
   #:is-book? #t
   #:location (dissertation-location #:institution "Carnegie Mellon University")
   #:date 1994))

;; ----------------------------------------
; Types for untyped languages

(define rtsf-oops-2013
  (make-bib
   #:author (authors "Brianna M. Ren" "John Toman" "T. Stephen Strickland" "Jeffrey S. Foster")
   #:title "The Ruby Type Checker"
   #:location (proceedings-location sac #:pages '(1565 1572))
   #:date 2013))

;; ----------------------------------------
; Gradual typing

;; for these papers, see
;; https://github.com/samth/gradual-typing-bib/blob/master/main.rkt

(define typescript
  (make-bib
   #:title "Typescript Language Specification"
   #:location (techrpt-location #:number "Version 0.9.1"
                                #:institution "Microsoft")
   #:date 2013))

;; ----------------------------------------
; Components and modules

(define mfh-oopsla-2001
  (make-bib
   #:title "Jiazzi: New-Age Components for Old-Fashioned Java"
   #:author (authors "Sean McDirmid" "Matthew Flatt"
                     "Wilson C. Hsleh")
   #:date 2001
   #:location (proceedings-location oopsla #:pages '(211 222))))

(define fg-ml-2010
  (make-bib
   #:title "First-class Modules and Composable Signatures in Objective Caml 3.12"
   #:author (authors "Alain Frisch" "Jacques Garrique")
   #:date 2010
   #:location (proceedings-location ml-workshop)))

;; ----------------------------------------
; Mixins and traits

(define fkf-popl-1998
  (make-bib
   #:title "Classes and Mixins"
   #:author (authors "Matthew Flatt" "Shriram Krishnamurthi"
                     "Matthias Felleisen")
   #:location (proceedings-location popl #:pages '(171 183))
   #:date 1998))

(define alz-ecoop-2000
  (make-bib
   #:title "Jam - A Smooth Extension of Java with Mixins"
   #:author (authors "Davide Ancona" "Giovanni Lagorio" "Elena Zucca")
   #:location (proceedings-location esop #:pages '(154 178))
   #:date 2000))

(define abc-oopsla-2003
  (make-bib
   #:title "A First-class Approach to Genericity"
   #:author (authors "Eric Allen" "Jonathan Bannet"
                     "Robert Cartwright")
   #:location (proceedings-location oopsla #:pages '(96 114))
   #:date 2003))

(define sdnb-ecoop-2003
  (make-bib
   #:title "Traits: Composable Units of Behaviour"
   #:author (authors "Nathanael Schärli" "Stéphane Ducasse"
                     "Oscar Nierstrasz" "Andrew P. Black")
   #:location (proceedings-location ecoop #:pages '(248 274))
   #:date 2003))

(define kt-asplas-2004
  (make-bib
   #:title "McJava – A Design and Implementation of Java with Mixin-Types"
   #:author (authors "Tetsuo Kamina" "Tetsuo Tamai")
   #:location (proceedings-location asplas #:pages '(398 414))
   #:date 2004))

(define sd-ecoop-2005
  (make-bib
   #:title "Chai: Traits for Java-Like Languages"
   #:author (authors "Charles Smith" "Sophia Drossopoulou")
   #:location (proceedings-location ecoop #:pages '(453 478))
   #:date 2005))

(define sz-oopsla-2010
  (make-bib
   #:title "MetaFJig: a Meta-circular Composition Language for Java-like Classes"
   #:author (authors "Marco Servetto" "Elena Zucca")
   #:location (proceedings-location oopsla #:pages '(464 483))
   #:date 2010))

;; ----------------------------------------
; Beta and Beta-style programming

(define mmpn-book-1993
  (make-bib
   #:title "Object-Oriented Programming in the BETA Programming Language"
   #:author (authors "Ole Lehrmann Madsen" "Birger Møller-Pedersen"
                     "Kristen Nygaard")
   #:date 1993
   #:location (book-location #:publisher "Addison-Wesley Publishing Co.")))

(define gff-oopsla-2004
  (make-bib
   #:title "Super and Inner: Together at Last!"
   #:author (authors "David S. Goldberg" "Robert Bruce Findler"
                     "Matthew Flatt")
   #:date 2004
   #:location (proceedings-location oopsla #:pages '(116 129))))

;; ----------------------------------------
; Types for delim control

(define Danvy1989
  (make-bib
   #:author (authors "Olivier Danvy" "Andrzej Filinski")
   #:title "A Functional Abstraction of Typed Contexts"
   #:location (techrpt-location #:institution "University of Copenhagen"
                                ;; I'm not 100% sure of the TR number since
                                ;; it's not listed anywhere officially
				#:number "DIKU Report 89/12")
   #:date 1989))

(define AbstractingControl
  (make-bib
   #:author (authors "Olivier Danvy" "Andrzej Filinski")
   #:title "Abstracting Control"
   #:location (proceedings-location lfp #:pages '(151 160))
   #:date 1990))

(define Gunter1995
  (make-bib
    #:author (authors "Carl A. Gunter" "Remy Didier" "Jon G. Riecke")
    #:title "A Generalization of Exceptions and Control in ML-like Languages"
    #:location (proceedings-location fpca #:pages '(12 23))
    #:date 1995))

(define Kiselyov2007
  (make-bib
   #:author (authors "Oleg Kiselyov" "Chung-chieh Shan")
   #:title "A Substructural Type System for Delimited Continuations"
   #:location (proceedings-location tlca #:pages '(223 239))
   #:date 2007))

(define Asai2007
  (make-bib
    #:author (authors "Kenichi Asai" "Yukiyoshi Kameyama")
    #:title "Polymorphic Delimited Continuations"
    #:location (proceedings-location asplas #:pages '(239 254) #;#:series #;"LNCS 4807")
    #:date 2007))

(define Dybvig2007
  (make-bib
   #:author (authors "Kent Dybvig"
                     "Simon Peyton-Jones"
                     "Amr Sabry")
   #:title "A Monadic Framework for Delimited Continuations"
   #:location (journal-location jfp
                                #:volume 17
                                #:number 6
                                #:pages '(687 730))
   #:date 2007))

(define James2011
  (make-bib
   #:author (authors "Roshan P. James" "Amr Sabry")
   #:title "Yield: Mainstream Delimited Continuations"
   #:location (proceedings-location "Theory and Practice of Delimited Continuations" #:pages '(20 32))
   #:date 2011))

(define control-tr
  (make-bib
   #:author (authors "Asumu Takikawa" "T. Stephen Strickland" "Sam Tobin-Hochstadt")
   #:title "Constraining Delimited Control with Contracts"
   #:location (techrpt-location #:institution "Northeastern University"
				#:number "NU-CCIS-13-01")
   #:date "2013"))

;; ----------------------------------------
; Education

(define scriven-chapter-1967
  (make-bib
   #:author "Michael Scriven"
   #:title "The Methodology of Evaluation. Perspectives of Curriculum Evaluation"
   #:location (book-location #:publisher "Rand McNally")
   #:date 1967))

(define fcffksf-jfp-2002
  (make-bib
   #:author (authors "Robert Bruce Findler" "John Clements"
                     "Cormac Flanagan" "Matthew Flatt"
                     "Shriram Krishnamurthi" "Paul Steckler"
                     "Matthias Felleisen")
   #:title "DrScheme: a Programming Environment for Scheme"
   #:location (journal-location jfp
                                #:volume 12
                                #:number 2
                                #:pages '(159 182))
   #:date 2002))

(define fffk-icfp-2009
  (make-bib
   #:author (authors "Matthias Felleisen" "Robert Bruce Findler"
                     "Matthew Flatt" "Shriram Krishnamurthi")
   #:title "A Functional I/O System (or Fun for Freshman Kids)"
   #:location (proceedings-location icfp #:pages '(47 58))
   #:date 2009))

;; ----------------------------------------
; Racket

(define ffkf-icfp-1999
  (make-bib
   #:author (authors "Matthew Flatt" "Rober Bruce Findler"
                     "Shriram Krishnamurthi" "Matthias Felleisen")
   #:title "Programming Languages as Operating Systems (or Revenge of the Son of the Lisp Machine)"
   #:location (proceedings-location icfp #:pages '(138 147))
   #:date 1999))

(define fff-asplas-2006
  (make-bib
   #:author (authors "Matthew Flatt" "Robert Bruce Findler"
                     "Matthias Felleisen")
   #:title "Scheme with Classes, Mixins, and Traits"
   #:location (proceedings-location asplas #:pages '(270 289))
   #:date 2006))

(define fbf-icfp-2009
  (make-bib
   #:author (authors "Matthew Flatt" "Eli Barzilay"
                     "Robert Bruce Findler")
   #:title "Scribble: Closing the Book on Ad Hoc Documentation Tools"
   #:location (proceedings-location icfp #:pages '(109 120))
   #:date 2009))

(define st-icfp-2013
  (make-bib
   #:author (authors "Vincent St-Amour" "Neil Toronto")
   #:title "Applying Random Testing to a Base Type Environment"
   #:location (proceedings-location icfp #:pages '(351 356))
   #:date 2013))

(define saf-cc-2015
  (make-bib
   #:author (authors "Vincent St-Amour" "Leif Andersen" "Matthias Felleisen")
   #:title "Feature-specific Profiling"
   #:location (proceedings-location cc)
   #:date 2015))

;; ----------------------------------------
; Pycket

(define fbpsth-dyla-2014
  (make-bib
   #:author (authors "Carl Friedrich Bolz" "Tobias Pape"
                     "Jeremy G. Siek" "Sam Tobin-Hochstadt")
   #:title "Meta-tracing makes a fast Racket"
   #:location (proceedings-location dyla)
   #:date 2014))

(define bauman-et-al-icfp-2015
  (make-bib
   #:author (authors "Spenser Bauman" "Carl Friedrich Bolz" "Robert Hirschfield"
                     "Vasily Kirilichev" "Tobias Pape" "Jeremy G. Siek"
                     "Sam Tobin-Hochstadt")
   #:title "Pycket: A Tracing JIT For a Functional Language"
   #:location "In submission"
   #:date 2015))
