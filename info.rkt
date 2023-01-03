#lang info

(define collection 'use-pkg-name)
(define pkg-desc "Lorem Ipsum")

(define build-deps '("scribble-lib" "racket-doc"))
(define deps '("w3s"
               "base" "gui-lib" "typed-racket-lib" "typed-racket-more"
                      "racket-index" "sandbox-lib" "scribble-lib" "math-lib"))

(define version "1.0")
(define pkg-authors '(wargrey))
(define test-omit-paths 'all)

(define raco-commands '(["wisemon" digimon/digivice/wisemon "compile and build collection before or after installing it" #false]
                        ["nanomon" digimon/digivice/nanomon "run and test `#lang`s" #false]))

(define scribblings '(["tamer/digimon.scrbl" (main-doc multi-page)]))

(define typesettings '(["tamer/tex/cjk.tex" "CJK"]
                       ["tamer/tex/pdftex.scrbl" pdflatex "pdflatex"]))

(define ffi-toolchain-config '())

(define native-launcher-names '(["tamer/cc/launcher/main.c" "cmain"]
                                ["tamer/cc/launcher/main.cpp"]))
