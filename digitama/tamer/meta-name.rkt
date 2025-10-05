#lang typed/racket/base

(provide (all-defined-out))

(define handbook-tag-prefix : String "handbook-")
(define handbook-title-tag : String "tamer-handbook")

(define handbook-ack-tag : String (string-append handbook-tag-prefix "acknowledgment"))
(define handbook-bibliography-tag : String (string-append handbook-tag-prefix "bibliography"))
(define handbook-reference-tag : String (string-append handbook-tag-prefix "reference"))
(define handbook-index-tag : String (string-append handbook-tag-prefix "index"))
