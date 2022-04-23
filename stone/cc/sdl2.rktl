#lang typed/racket/base

(define cc-toolchain-config : (Listof (Pairof Symbol Any))
  '((macro _THREAD_SAFE)
    (lib SDL2 SDL2main SDL2_TTF)
    (lib [windows shell32])))
