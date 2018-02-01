#lang at-exp racket
(require scribble/manual
         scriblib/autobib
         (only-in scribble/core make-element make-style plain))

(provide (all-defined-out))

;; shortens names
(abbreviate-given-names #f)

;; ----------------------------------------

(define short? #f)
(define-syntax define/short
  (syntax-rules ()
    [(_ i e e*) (define i (if short? e e*))]
    [(_ i e) (define i e)]))

(define IEEE "IEEE ")
(define ACM "ACM ")
(define International "International ")
(define Conference "Conference ")
(define Workshop "Workshop ")
(define Journal "Journal ")
(define Symposium "Symposium ")
(define Transactions "Transactions on ")

(define racket-con (list "In " (emph "RacketCon")))

(define/short aplas "APLAS" (string-append "Asian " Symposium "Programming Languages and Systems"))
(define/short asplos "ASPLOS" (string-append ACM International Conference "on Architectural Support for Programming Languages and Operating Systems"))
(define/short fpca "FPCA" (string-append ACM International Conference "Functional Programming Languages and Computer Architecture"))
(define/short sde "SDE" (string-append ACM Symposium "on Practical software development environments"))
(define/short lfp "LFP" (string-append ACM Symposium "on LISP and functional programming"))
(define/short sas "SAS" (string-append International "Static Analysis Symposium"))
(define/short icfp "ICFP" (string-append ACM International Conference "on Functional Programming"))
(define/short pldi "PLDI" (string-append ACM Conference "on Programming Language Design and Implementation"))
(define/short popl "POPL" (string-append ACM Symposium "on Principles of Programming Languages"))
(define/short lncs "LNCS" "Lecture Notes in Computer Science")
(define/short ismm "ISMM" (string-append ACM International Symposium "on Memory Management"))
(define/short sigplan-notices "SIGPLAN Notices" (string-append ACM "SIGPLAN Notices"))
(define/short algorithmica "Algorithmica"  "Algorithmica")
(define/short scheme-workshop "SFP" (string-append "Scheme and Functional Programming Workshop"))
(define/short jfp "JFP" (string-append Journal "of Functional Programming"))
(define/short spe "SPE" (string-append Journal "of Software --- Practices & Experience"))
(define/short cj "The Computer Journal" "The Computer Journal")
(define/short asa "JASA" (string-append Journal "the American Statistical Association"))
(define/short hosc "HOSC" "Higher-Order and Symbolic Computation")
(define/short lsc "LSC" "LISP and Symbolic Computation")
(define/short ifl "IFL" (string-append International Symposium "Functional and Logic Programming"))
(define/short ip "Information Processing" "Information Processing")
(define/short tfp "TFP" (string-append Symposium "Trends in Functional Programming"))
(define/short ecoop "ECOOP" (string-append "European " Conference "Object-Oriented Programming"))
(define/short oopsla "OOPSLA" (string-append ACM Conference "on Object-Oriented Programming, Systems, Languages and Applications"))
(define/short ieee-software (string-append IEEE "Software"))
(define/short toplas "TOPLAS" (string-append ACM Transactions "Programming Languages and Systems"))
(define/short aplwa "APLWA" "Analysis and Programming Languages for Web Applications and Cloud Applications")
(define/short dls "DLS" "Dynamic Languages Symposium")
(define/short flops "FLOPS" (string-append Symposium "Functional and Logic Programming"))
(define/short snapl "SNAPL" "Summit on Advances in Programming Languages")
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
(define/short issta "ISSTA" (string-append International Symposium "on Software Testing and Analysis"))
(define/short pepm "PEPM" "ACM SIGPLAN Workshop on Partial Evaluation and Program Manipulation")
(define/short ppdp "PPDP" (string-append International Symposium "on Principles and Practice of Declarative Programming"))
(define/short stop "STOP" (string-append "Script to Program Evolution Workshop"))
(define/short pacm "PACM" (string-append "Proceedings of the ACM on Programming Languages"))

;; ----------------------------------------

(define tfgnvf-popl-2016
  (make-bib
   #:author (authors "Asumu Takikawa" "Daniel Feltey" "Ben Greenman" "Max S. New" "Jan Vitek" "Matthias Felleisen")
   #:title "Is Sound Gradual Typing Dead?"
   #:location (proceedings-location popl #:pages '(456 468))
   #:date 2016))

(define f-popl-2016
  (make-bib
   #:author (authors "Matthew Flatt")
   #:title "Bindings as Sets of Scopes"
   #:location (proceedings-location popl #:pages '(705 717))
   #:date 2016))

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

(define ls-ppdp-2006
  (make-bib
   #:author (authors "Tobias Lindahl" "Konstantinos Sagonas")
   #:title "Practical Type Inference Based on Success Typings"
   #:location (proceedings-location ppdp #:pages '(167 178))
   #:date 2006))

(define l-stop-2015
  (make-bib
   #:author "Jukka Lehtosalo"
   #:title "MyPy, an Optionally-Typed Python"
   #:location (proceedings-location stop)
   #:date 2015))

(define aemopssy-oopsla-2014
  (make-bib
   #:author (authors "Keith Adams"
                     "Jason Evans"
                     "Bertrand Maher"
                     "Guilherme Ottoni"
                     "Andrew Paroski"
                     "Brett Simmers"
                     "Edwin Smith"
                     "Owen Yamauchi")
   #:title "The HipHop Virtual Machine"
   #:location (proceedings-location oopsla #:pages '(777 790))
   #:date 2014))

;(define t-artima-2009
;  (make-bib
;   #:author "Bill Venners"
;   #:title "Twitter on Scala"
;   #:location (hyperlink "http://www.artima.com/scalazine/articles/twitter_on_scala.html" "http://www.artima.com/scalazine/articles/twitter_on_scala.html")
;   #:date 2009))
; "https://blog.twitter.com/2011/twitter-search-is-now-3x-faster"

(define armstrong-2007
  (make-bib
   #:author "Joe Armstrong"
   #:is-book? #t
   #:title "Programming Erlang: software for a concurrent world"
   #:location (book-location #:publisher "Pragmatic Bookshelf")
   #:date "2007"))

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

(define TypeDynamic
  (make-bib
    #:title "Dynamic Typing in a Statically Typed Language"
    #:author (authors
	       "Martin Abadi"
	       "Luca Cardelli"
	       "Benjamin C. Pierce"
	       "Gordon D. Plotkin")
    #:date 1991
    #:location (journal-location toplas
		 #:volume 13
		 #:number 2
		 #:pages '("237" "268"))))

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
   #:author (authors "Christos Dimoulas" "Robert Bruce Findler" "Cormac Flanagan" "Matthias Felleisen")
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
            #:location (proceedings-location pldi #:pages '(239 250))
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

(define r-icfp-2014
  (make-bib
    #:author "Norman Ramsey"
    #:title "On Teaching 'How to Design Programs'"
    #:location (proceedings-location icfp #:pages '(153 166))
    #:date 2014))

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

(define nthvh-icfp-2014
  (make-bib
   #:author (authors (make-element (make-style #f '(exact-chars)) "Ph\\'{u}c C. Nguy\\~{\\^{e}}n")
                     "Sam Tobin-Hochstadt" "David Van Horn")
   #:title "Soft Contract Verification"
   #:location (proceedings-location icfp #:pages '(139 152))
   #:date 2014))

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

;; for these papers, see 'gradual-bib.rkt'
;; or https://github.com/samth/gradual-typing-bib/blob/master/main.rkt

(define typescript
  (make-bib
   #:title "Typescript Language Specification"
   #:location (techrpt-location #:number "Version 0.9.1"
                                #:institution "Microsoft")
   #:date 2013))

(define g-popl-2015
  (make-bib
   #:title "Space-Efficient Manifest Contracts"
   #:author "Michael Greenberg"
   #:location (proceedings-location popl #:pages '(181 194))
   #:date 2015))

(define g-tfp-2016
  (make-bib
   #:author "Michael Greenberg"
   #:title "Space-Efficient Latent Contracts"
   #:location (proceedings-location tfp)
   #:date 2016))

;; ----------------------------------------
; Components and modules

(define sfrbcsb-popl-2014
  (make-bib
   #:author (authors "Nikhil Swamy" "Cedric Fournet" "Aseem Rastogi" "Karthikeyan Bhargavan" "Juan Chen" "Pierre-Yves Strub" "Gavin Bierman")
   #:title "Gradual Typing Embedded Securely in JavaScript"
   #:location (proceedings-location popl #:pages '(425 437))
   #:date 2014))

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

(define kt-aplas-2004
  (make-bib
   #:title "McJava – A Design and Implementation of Java with Mixin-Types"
   #:author (authors "Tetsuo Kamina" "Tetsuo Tamai")
   #:location (proceedings-location aplas #:pages '(398 414))
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
    #:location (proceedings-location aplas #:pages '(239 254) #;#:series #;"LNCS 4807")
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

(define fff-aplas-2006
  (make-bib
   #:author (authors "Matthew Flatt" "Robert Bruce Findler"
                     "Matthias Felleisen")
   #:title "Scheme with Classes, Mixins, and Traits"
   #:location (proceedings-location aplas #:pages '(270 289))
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
   #:location (proceedings-location cc #:pages '(49 68))
   #:date 2015))

(define stf-optimization-coaching
  (make-bib
    #:author (authors "Vincent St-Amour" "Sam Tobin-Hochstadt" "Matthias Felleisen")
    #:title "Optimization coaching"
    #:location (proceedings-location oopsla #:pages '(163 178))
    #:date 2012))

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
   #:location (proceedings-location icfp #:pages '(22 34))
   #:date 2015))

;; ----------------------------------------
; Pluggable types

(define bracha-pluggable-types
  (make-bib
   #:author "Gilad Bracha"
   #:title "Pluggable Type Systems"
   #:location (proceedings-location "OOPSLA Workshop on Revival of Dynamic Languages")
   #:date 2004))

(define pacpe-issta-2008
  (make-bib
   #:author (authors "Matthew M. Papi" "Mahmood Ali" "Telmo Louis Correa, Jr."
                     "Jeff H. Perkins" "Michael D. Ernst")
   #:title "Practical Pluggable Types for Java"
   #:location (proceedings-location issta #:pages '(201 212))
   #:date 2008))

;; ----------------------------------------
; Ancient history

(define moon-maclisp-1974
  (make-bib
   #:author "David A. Moon"
   #:title "MACLISP Reference Manual"
   #:date 1974))

(define shivers-dissertation-1991 ;)
  (make-bib
   #:author "Olin Shivers"
   #:title "Control-Flow Analysis of Higher-Order Languages"
   #:location (dissertation-location #:institution "Carnegie Mellon University")
   #:date "1991"))

;; ----------------------------------------
;; Blog Posts and User Links

(define v-aplwa-2010
  (make-bib
   #:author "Jan Vitek"
   #:title "Of Scripts and Programs, Tall Tales, Urban Legends and Future Prospects"
   #:location (proceedings-location aplwa)
   #:date 2010))

(define b-rc-2015
  ;; Uses Typed Racket
  ;; http://con.racket-lang.org/2015/burns.pdf
  (make-bib
    #:author "Marc Burns"
    #:title "Rocking with Racket"
    #:location racket-con
    #:date 2015))

(define p-rc-2014
  ;; Uses Racket (probably not Typed)
  (make-bib
    #:author "Daniel Prager"
    #:title "YouPatch: A Racket-powered startup"
    #:location racket-con
    #:date 2014))

(define l-rc-2014
  ;; Uses Racket (maybe Typed, but not sure)
  (make-bib
    #:author "Dan Liebgold"
    #:title "Racket on the Playstation 3? It's Not What you Think!"
    #:location racket-con
    #:date 2013))

(define tmv-esop-2014
  (make-bib
   #:title "Running Probabilistic Programs Backwards"
   #:author (authors "Neil Toronto" "Jay McCarthy" "David Van Horn")
   #:location (proceedings-location esop #:pages '(53 79))
   #:date 2014))

(define r-ip-1983
  (make-bib
   #:title "Types, Abstraction, and Parametric Polymorphism"
   #:author "John C. Reynolds"
   #:location (proceedings-location ip)
   #:date 1983))

(define u-algorithmica-1995
  (make-bib
   #:title "On-line construction of suffix trees"
   #:author "Esko Ukkonen"
   #:location (journal-location algorithmica
                                #:volume 14
                                #:number 3
                                #:pages '(249 260))
   #:date 1995))

;; -- GC
; Zorn's diss : http://www.hpl.hp.com/techreports/Compaq-DEC/SRC-RR-25.pdf
; Hertz/Berger, GC vs. Explicit : https://people.cs.umass.edu/~emery/pubs/gcvsmalloc.pdf
; Blackburn, Cheng, McKinley : http://www.cs.utexas.edu/users/mckinley/papers/mmtk-icse-2004.pdf
; Jones, GC handbook

(define u-sde-1984
  (make-bib
   #:title "Generation Scavenging: A non-disruptive high performance storage reclamation algorithm"
   #:author "David Ungar"
   #:location (proceedings-location sde #:pages '(157 167))
   #:date 1984))

(define m-lfp-1984
  (make-bib
   #:title "Garbage collection in a large LISP system"
   #:author "David A. Moon"
   #:location (proceedings-location lfp #:pages '(235 246))
   #:date 1984))

;; http://plt.eecs.northwestern.edu/racket-machine/racket-machine.pdf
(define kff-hosc-2013
  (make-bib
   #:title "The Racket Virtual Machine and Randomized Testing"
   #:author (authors "Casey Klein" "Matthew Flatt" "Robert Bruce Findler")
   #:location (proceedings-location hosc #:pages '(1 45))
   #:date 2013))

;; http://kar.kent.ac.uk/33611/7/paper.pdf
(define kj-ismm-2013
  (make-bib
   #:title "Rigorous Benchmarking in Reasonable Time"
   #:author (authors "Tomas Kalibera" "Richard E. Jones")
   #:location (proceedings-location ismm #:pages '(63 74))
   #:date 2013))

;; http://www.cs.umass.edu/~emery/pubs/stabilizer-asplos13.pdf
(define cb-asplos-2013
  (make-bib
   #:title "Stabilizer: Statistically Sound Performance Evaluation"
   #:author (authors "Charlie Curtsinger" "Emery Berger")
   #:location (proceedings-location asplos #:pages '(219 228))
   #:date 2013))

;; -- stats
(define kj-tr-2013
  (make-bib
   #:title "Quantifying performance changes with effect size confidence intervals"
   #:author (authors "Tomas Kalibera" "Richard Jones")
   #:location "Technical Report 4--12, University of Kent"
   #:date 2012))

(define s-asa-1974
  (make-bib
   #:title "EDF Statistics for Goodness of Fit and Some Comparisons"
   #:author "M. A. Stephens"
   #:location (journal-location asa
                                #:volume 69
                                #:number 347
                                #:pages '(730 737))
   #:date 1974))

(define ad-asa-1954
  (make-bib
   #:title "A Test of Goodness of Fit"
   #:author (authors "T. W. Anderson" "D. A. Darling")
   #:location (journal-location asa
                                #:volume 49
                                #:number 268
                                #:pages '(765 769))
   #:date 1954))

(define bbdt-ecoop-2016
  (make-bib
   #:title "Fine-grained Language Composition: A Case Study"
   #:author (authors "Edd Barrett" "Carl Friedrich Bolz" "Lukas Diekmann" "Laurence Tratt")
   #:location (proceedings-location ecoop #:pages '(3:1 3:27))
   #:date 2016))

(define bbkmt-oopsla-2017
  (make-bib
   #:title "Virtual Machine Warmup Blows Hot and Cold"
   #:author (authors "Edd Barrett" "Carl Friedrich Bolz-Tereick" "Rebecca Killick" "Sarah Mount" "Laurence Tratt")
   #:location (journal-location pacm #:volume "1, OOPSLA")
   #:date 2017))

(define g-icfp-2013
  (make-bib
    #:title "Calculating Threesomes, with Blame"
    #:author (authors "Ronald Garcia")
    #:location (proceedings-location icfp #:pages '(417 428))
    #:date 2013))

(define gct-popl-2016
 (make-bib
  #:title "Abstracting Gradual Typing"
  #:author (authors "Ronald Garcia" "Alison M. Clark" "Éric Tanter")
  #:location (proceedings-location popl #:pages '(429 442))
  #:date 2016))

(define mfsw-hosc-2005
  (make-bib
    #:title "Selectors Make Set-Based Analysis Too Hard"
    #:author (authors "Philippe Meunier" "Robert Bruce Findler" "Paul Steckler" "Mitchell Wand")
    #:location (journal-location hosc
                                 #:volume 18
                                 #:number 3
                                 #:pages '(245 269))
    #:date 2005))

(define n-mthesis-2014
  (make-bib
   #:title "Tough Behavior in Repeated Bargaining game, A Computer Simulation Study"
   #:author "Linh Chi Nguyen"
   #:location (dissertation-location #:institution "University of Trento" #:degree "Master in Economics")
   #:date 2014))

(define h-cj-1989
  (make-bib
   #:title "Why Functional Programming Matters"
   #:author "John Hughes"
   #:location (journal-location cj
                                #:volume 32
                                #:number 2
                                #:pages '(98 107))
   #:date 1989))

(define l-mthesis-2016
  (make-bib
    #:title "Typed Contracts for Gradual Typing"
    #:author "Brian LaChance"
    #:location (dissertation-location #:institution "Northeastern University" #:degree "Master in Computer Science")
    #:date 2016))

(define stf-esop-2009
  (make-bib
   #:title "Practical Variable-Arity Polymorphism"
   #:author (authors "T. Stephen Strickland" "Sam Tobin-Hochstadt" "Matthias Felleisen")
   #:location (proceedings-location esop #:pages '(32 46))
   #:date 2009))

(define b-spe-1993
  ; http://www.cs.colorado.edu/department/publications/reports/docs/CU-CS-573-92.pdf
  (make-bib
   #:title "The measured cost of conservative garbage collection"
   #:author "Ben Zorn"
   #:location (journal-location spe
                                #:volume 23
                                #:number 7
                                #:pages '(733 756))
   #:date 1993))

(define rf-pldi-2016
  (make-bib
   #:title "Just-in-time Static Type Checking for Dynamic Languages"
   #:author (authors "Brianna M. Ren" "Jeffrey S. Foster")
   #:location (proceedings-location pldi #:pages '(462 476))
   #:date 2016))

(define f-icfp-2014
  (make-bib
   #:title "Behavioral Software Contracts"
   #:author "Robert Bruce Findler"
   #:location (list "KEYNOTE speech: " (proceedings-location icfp))
   #:date 2014))

; http://www.ncbi.nlm.nih.gov/pmc/articles/PMC2064100/

(define f-arxiv-2006
  ; https://arxiv.org/pdf/0710.2024v1.pdf
  (make-bib
   #:title "Ratios: A short guide to confidence limits and proper use"
   #:author "Volker H. Franz"
   #:location "Unpublished Manuscript"
   #:date 2007))

(define f-rss-1957
  (make-bib
   #:title "Some Problems in Interval Estimation"
   #:author "E.C. Fieller"
   #:location (journal-location "Journal of the Royal Statistical Society"
                                #:volume 16
                                #:number 2
                                #:pages '(175 185))
   #:date 1957))

(define n-ptrs-1937
  (make-bib
   #:title "Outline of a Theory of Statistical Estimation Based on the Classical Theory of Probability"
   #:author "J. Neyman"
   #:location (journal-location "Philosophical Transactions of the Royal Society of London"
                                #:volume 236
                                #:number 767
                                #:pages '(333 380))
   #:date 1937))

;(define lf-statistica-2009
;  (make-bib
;   #:title "A Geometric Approach to Confidence Sets for Ratios: Fieller's Theorem, Generalizations, and Bootstrap"
;   #:author (authors "Ulrike von Luxburg" "Volker H. Franz")
;   #:location (journal-location "Statistica Sinica"
;                                #:volume 19
;                                #:number 3
;                                #:pages '(1095 1117))
;   #:date 2009))

(define bfwc-pm-2005
  (make-bib
   #:title "Researchers Misunderstand Confidence Intervals and Standard Error Bars"
   #:author (authors "Sarah Belia" "Fiona Fidler" "Jennifer Williams" "Geoff Cumming")
   #:location (journal-location "Psychological Methods"
                                #:volume 10
                                #:number 4
                                #:pages '(389 396))
   #:date 2005))

(define mdhs-asplos-2009
  (make-bib
   #:title "Producing Wrong Data Without Doing Anything Obviously Wrong"
   #:author (authors "Todd Mytkowicz" "Amer Diwan" "Matthais Hauswirth" "Peter F. Sweeney")
   #:location (proceedings-location asplos #:pages '(265 276))
   #:date 2009))

(define gvg-siu-2005
  (make-bib
   #:title "Code Layout as a Source of Noise in JVM Performance"
   #:author (authors "Dayong Gu" "Clark Verbrugge" "Etienne Gagnon")
   #:location (journal-location "Studia Informatica Universalis"
                                #:volume 4
                                #:number 1
                                #:pages '(83 99))
   #:date 2005))

(define stw-pldi-2015
  (make-bib
   #:title "Blame and Coercion: Together again for the first time"
   #:author (authors "Jeremy Siek" "Peter Thiemann" "Philip Wadler")
   #:location (proceedings-location pldi #:pages '(425 435))
   #:date 2015))

(define gff-oopsla-2005
  (make-bib
   #:title "Fine-Grained Interoperability Through Mirrors and Contracts"
   #:author (authors "Kathryn E. Gray" "Robert Bruce Findler" "Matthew Flatt")
   #:location (proceedings-location oopsla #:pages '(231 245))
   #:date 2005))

(define agd-ecoop-2005
  (make-bib
   #:title "Toward Type Inference for JavaScript"
   #:author (authors "Christopher Anderson" "Paul Giannini" "Sophia Drossopoulou")
   #:location (proceedings-location ecoop #:pages '(428 452))
   #:date 2005))

(define svcb-snapl-2015
  (make-bib
   #:title "Refined Criteria for Gradual Typing"
   #:author (authors "Jeremy G. Siek" "Michael M. Vitousek" "Matteo Cimini" "John Tang Boyland")
   #:location (proceedings-location snapl #:pages '(274 293))
   #:date 2015))

(define vss-popl-2017
  (make-bib
    #:title "Big Types in Little Runtime: Open-World Soundness and Collaborative Blame for Gradual Type Systems"
    #:author (authors "Michael M. Vitousek" "Cameron Swords" "Jeremy G. Siek")
    #:location (proceedings-location popl)
    #:date 2017))

(define tfffgksst-snapl-2017
  (make-bib
   #:title "Migratory Typing: Ten years later"
   #:author (authors "Sam Tobin-Hochstadt"
                     "Matthias Felleisen"
                     "Robert Bruce Findler"
                     "Matthew Flatt"
                     "Ben Greenman"
                     "Andrew M. Kent"
                     "Vincent St-Amour"
                     "T. Stephen Strickland"
                     "Asumu Takikawa")
   #:location (proceedings-location snapl)
   #:date 2017))

(define l-freenix-2006
  ; http://erwan.lemonnier.se/talks/pluto.html
  (make-bib
   #:title "Pluto: or how to make Perl juggle with billions"
   #:author "Erwan Lemonnier"
   #:location "Forum on Free and Open Source Software (FREENIX)"
   #:url "http://erwan.lemonnier.se/talks/pluto.html"
   #:date 2006))

(define h-lfp-1992
  (make-bib
   #:title "Global Tagging Optimization by Type Inference"
   #:author "Fritz Henglein"
   #:location (proceedings-location lfp #:pages '(205 215))
   #:date 1992))

(define jw-sas-1995
  (make-bib
   #:title "Effective Flow Analysis for Avoiding Run-Time Checks"
   #:author (authors "Suresh Jagannathan" "Andrew K. Wright")
   #:location (proceedings-location sas #:pages '(207 224))
   #:date 1995))

(define sw-sas-1995
  (make-bib
   #:title "Bigloo: A Portable and Optimizing Compiler for Strict Functional Languages"
   #:author (authors "Erick Gallesio" "Manuel Serrano")
   #:location (proceedings-location sas #:pages '(366 381))
   #:date 1995))

(define c-esop-1988
  (make-bib
   #:title "New Insights into Partial Evaluation: the SCHISM Experiment"
   #:author "Charles Consel"
   #:location (proceedings-location esop #:pages '(236 246))
   #:date 1988))

(define gm-pepm-2018
  (make-bib
   #:title "On the Cost of Type-Tag Soundness"
   #:author (authors "Ben Greenman" "Zeina Migeed")
   #:location (proceedings-location pepm)
   #:date 2018))

;; http://www.franktip.org/pubs/ecoop2014.pdf

(define gf-pldi-2018
  (make-bib
    #:title "The Spectrum of Soundness and Performance"
    #:author (authors "Ben Greenman" "Matthias Felleisen")
    #:location "Submitted for review"
    #:date 2018))

(define wmwz-ecoop-2017
  (make-bib
    #:title "Mixed Messages: Measuring Conformance and Non-Interference in TypeScript"
    #:author (authors "Jack Williams" "J. Garrett Morris" "Philip Wadler" "Jakub Zalewski")
    #:location (proceedings-location ecoop #:pages '(28:1 28:29))
    #:date 2017))

(define btsb-oopsla-2017
  (make-bib
    #:title "Sound Gradual Typing: Only Mostly Dead"
    #:author (authors "Spenser Bauman" "Sam Tobin-Hochstadt" "Jeremy G. Siek" "Carl Friedrich Bolz-Tereick")
    #:location (proceedings-location oopsla)
    #:date 2017))

