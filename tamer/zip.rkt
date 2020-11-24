#lang typed/racket/base

(require digimon/archive)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define zip.zip (collection-file-path "hwzip-1.4.zip" "digimon" "tamer"))

(zip-list-entries* zip.zip)
