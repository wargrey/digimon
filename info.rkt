#lang info

(define collection 'use-pkg-name)
(define pkg-desc "Lorem Ipsum")

(define build-deps '("scribble-lib" "racket-doc"))
(define deps '("base" "gui-lib" "typed-racket-lib" "typed-racket-more"
                      "racket-index" "sandbox-lib" "scribble-lib" "math-lib"))

(define version "1.0")
(define pkg-authors '(wargrey))
(define test-omit-paths 'all)

(define raco-commands '(["wisemon" digimon/digivice/wisemon "compile and build collection before or after installing it" #false]
                        ["nanomon" digimon/digivice/nanomon "run and test `#lang`s" #false]))

(define scribblings '(["tamer/digimon.scrbl" (main-doc multi-page)]))

(define typesettings '(["tamer/tex/cjk.tex" "CJK"]
                       ["tamer/tex/pdftex.scrbl" pdflatex "pdflatex"]))

(define msvc-kits-rootdir "C:\\Program Files (x86)\\Windows Kits\\10")
(define msvc-kits-version "10.0.19041.0")

(define native-launcher-names '(["tamer/cc/launcher/main.c" "cmain"]
                                ["tamer/cc/launcher/main.cpp"]))
