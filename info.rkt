#lang info

(define collection "digitama")
(define pkg-desc "The (typed) core to build the digital world")

(define deps '("base" "typed-racket-lib"))
(define build-deps '("scribble-lib" "racket-doc"))

(define version "1.0")
(define pkg-authors '(wargrey))

(define scribblings '(["digitama/tamer/handbook.scrbl" (main-doc)]))
