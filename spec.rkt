#lang typed/racket/base

(provide (all-defined-out))
(provide (rename-out [spec-collapse %?-fail]))

(provide (all-from-out "digitama/spec/issue.rkt"))
(provide (all-from-out "digitama/spec/assert.rkt"))

(require "digitama/spec/issue.rkt")
(require "digitama/spec/assert.rkt")

(require "digitama/spec/prompt.rkt")
