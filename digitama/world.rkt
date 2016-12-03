#lang typed/racket/no-check

(provide (all-from-out typed/racket/no-check)
         (all-from-out "cheat.rkt" "main.rkt"))

(require (only-in "cheat.rkt" define-cheat-opaque define/make-is-a?))

(require "main.rkt"
         (only-in "../cheat.rkt" make-cheat-opaque?))
