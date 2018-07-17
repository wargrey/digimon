#lang info

(define collection 'use-pkg-name)
(define pkg-desc "Lorem Ipsum")

(define deps '("base" "make" "cext-lib" "dynext-lib" "scribble-lib" "rackunit-lib" "typed-racket-lib" "typed-racket-more"))
(define build-deps '("scribble-lib" "racket-doc"))

(define version "1.0")
(define pkg-authors '(wargrey))
(define test-omit-paths 'all)

(define raco-commands '(["wisemon" digimon/digivice/wisemon "yet another `make` utility" #false]))

(define scribblings '(["tamer/digimon.scrbl" (main-doc)]))
