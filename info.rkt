#lang info

(define collection 'use-pkg-name)
(define pkg-desc "The (typed) core to construct the digital world")

(define deps '("base" "typed-racket-lib" "typed-racket-more"))
(define build-deps '("scribble-lib" "racket-doc"))

(define version "1.0")
(define pkg-authors '(wargrey))

(define scribblings '(["tamer/handbook.scrbl" (main-doc)]))
