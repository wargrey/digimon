#lang info

(define collection 'use-pkg-name)
(define pkg-desc "Lorem Ipsum")

(define build-deps '("scribble-lib" "racket-doc"))
(define deps '("base" "make" "typed-racket-lib" "typed-racket-more"
                      "racket-index" "sandbox-lib" "scribble-lib"))

(define version "1.0")
(define pkg-authors '(wargrey))
(define test-omit-paths 'all)

(define raco-commands '(["wisemon" digimon/digivice/wisemon "yet another `make` utility" #false]))

(define scribblings '(["tamer/digimon.scrbl" (main-doc multi-page)]))

(define typesettings '(["tamer/tex/cjk.tex"]
                       ["tamer/tex/pdftex.scrbl" pdflatex "pdflatex"]))
