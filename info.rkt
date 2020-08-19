#lang info

(define collection 'use-pkg-name)
(define pkg-desc "Lorem Ipsum")

(define build-deps '("scribble-lib" "racket-doc"))
(define deps '("base" "make" "typed-racket-lib" "typed-racket-more"
                      "racket-index" "sandbox-lib" "scribble-lib" "math-lib"))

(define version "1.0")
(define pkg-authors '(wargrey))
(define test-omit-paths 'all)

(define raco-commands '(["wisemon" digimon/digivice/wisemon "yet another `GNU make` utility" #false]
                        ["nanomon" digimon/digivice/nanomon "an utility for testing #lang" #false]))

(define scribblings '(["tamer/digimon.scrbl" (main-doc multi-page)]))

(define typesettings '(["tamer/tex/cjk.tex" "CJK"]
                       ["tamer/tex/pdftex.scrbl" pdflatex "pdflatex"]))
