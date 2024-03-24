#lang info

(define collection 'use-pkg-name)
(define pkg-desc "Lorem Ipsum")

(define build-deps '("scribble-lib" "racket-doc"))
(define deps '("base" "gui-lib" "typed-racket-lib" "typed-racket-more"
                      "racket-index" "sandbox-lib" "scribble-lib" "math-lib"))

(define version "1.0")
(define pkg-authors '(wargrey))
(define test-omit-paths 'all)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define ffi-toolchain-config '())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define raco-commands
  '(["wisemon"  digimon/digivice/wisemon  "yet another building system for Racket, C/C++, Scribble/tex and more" #false]
    ["wizarmon" digimon/digivice/wizarmon "run source file of C/C++, Scribble/tex, Python, and more" #false]
    ["nanomon"  digimon/digivice/nanomon  "a utility to assist the building system" #false]))

(define scribblings
  '(["tamer/digimon.scrbl" (main-doc multi-page)]))

(define typesettings
  '(["tamer/tex/cjk.tex" "CJK"]
    ["tamer/tex/pdftex.scrbl" pdflatex "pdflatex"]))

(define native-launcher-names
  '(["tamer/cc/launcher/main.c" "cmain" console]
    ["tamer/cc/launcher/main.cpp" console]))
