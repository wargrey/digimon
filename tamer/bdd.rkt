#lang scribble/lp2

@(require "../tamer.rkt")

@(define name "specmon")
@(define the-name (tech name))

@(define-bib RackUnit
   #:title  "RackUnit: Unit Testing"
   #:author (authors "Noel Welsh" "Ryan Culpepper")
   #:date   2019
   #:url    "https://docs.racket-lang.org/rackunit")

@(define-bib BDD
   #:title  "Behavior-driven development"
   #:author "Dan North"
   #:date   2009
   #:url    "http://dannorth.net/introducing-bdd")

@handbook-story{@(string-titlecase name): Behavior Driven Development}

@deftech[(string-titlecase name)] is a testing framework written in @bold{Typed Racket} completely.
It is deliberately designed to follow the principle of @deftech[#:key "BDD"]{Behavior-driven development}, or @~cite[BDD].

Why is there yet another testing framework when @bold{Racket} has already equipped with a builtin one @~cite[RackUnit]?
The short answer is @italic{Wording Matters}, the long answer is, well, what exactly this manual exists for.

@tamer-smart-summary[]

@handbook-scenario[]{Wording Matters}

Despite the fact that people invented so many concepts and methodologies in order to improve software quality. Amongst them,
@tech{BDD} is a communication technique and concentrates on bringing people involved in a software project closer.

@tamer-note['wording-matters]
@chunk[|<can-you-give-me-an-example?>|
       (example-begin wording-matters #:do
         (describe "normal issue" #:do
                   (it "intends to be true" #:do
                       (expect-true #true))
                   #;(it "intends to be false" #:do
                       (expect-false #true))))]

@handbook-reference[]

@; Chunks after `handbook-reference[]` will never be rendered in documents
@; <*> is the main chunk by convention.

@chunk[|<*>|
       (require digimon/tamer)
       (tamer-taming-start!)

       (module+ tamer
         (module story typed/racket/base
           (require digimon/spec)

           |<can-you-give-me-an-example?>|))]
