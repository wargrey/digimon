#lang typed/racket/base

(provide (all-defined-out))

(require digimon/image)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(module+ main
  (define image.exe
    (with-handlers ([exn:fail? (λ _ #false)])
      (vector-ref (current-command-line-arguments) 0)))

  (with-asserts ([image.exe string?])
    (read-c-image image.exe)))
