#lang racket

(provide (all-from-out racket))
(provide (all-from-out "digitama/ffi.rkt"))

(require "digitama/ffi.rkt")

(provide (for-syntax (all-from-out racket/string racket/syntax syntax/parse racket/sequence)))
(require (for-syntax racket/string racket/syntax syntax/parse racket/sequence))

(module reader syntax/module-reader digimon/ffi)
