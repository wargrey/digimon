#lang typed/racket/base

(require digimon/digitama/bintext/zip)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define zip.zip (collection-file-path "hwzip-1.4.zip" "digimon" "tamer"))

(call-with-input-file* zip.zip
  (λ [[/dev/zipin : Input-Port]]
    (when (zip-seek-signature /dev/zipin)
      (define eocdr (read-zip-end-of-central-directory /dev/zipin))

      eocdr)))
