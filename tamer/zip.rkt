#lang typed/racket/base

(require digimon/archive)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define zip.zip
  (with-handlers ([exn:fail? (λ [[e : exn]] (collection-file-path "hwzip-1.4.zip" "digimon" "tamer"))])
    (vector-ref (current-command-line-arguments) 0)))

(zip-list-entries* zip.zip)
