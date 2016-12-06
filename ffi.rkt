#lang racket

(provide (all-from-out racket))
(provide (all-from-out "digitama/ffi.rkt" "digitama/main.rkt"))

(require "digitama/ffi.rkt")
(require "digitama/main.rkt")

(module reader syntax/module-reader digimon/ffi)
