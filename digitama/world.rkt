#lang typed/racket/no-check

(provide (all-from-out typed/racket/no-check)
         (all-from-out "cheat.rkt" "../main.rkt"))

(require racket/require
         "cheat.rkt"
         (subtract-in "../main.rkt" "cheat.rkt"))
