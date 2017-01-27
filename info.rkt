#lang info

(define collection 'use-pkg-name)
(define pkg-desc "The (typed) core to construct the digital world")

(define deps '("base" "make" "cext-lib" "dynext-lib" "typed-racket-lib" "typed-racket-more"))
(define build-deps '("scribble-lib" "racket-doc"))

(define version "1.0")
(define pkg-authors '(wargrey))

(define raco-commands '(["wisemon" digimon/digivice/wisemon "a raco to manage the workspace" #false]))

(define scribblings '(["tamer/digimon.scrbl" (main-doc)]))
