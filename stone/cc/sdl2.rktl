#lang typed/racket/base

(define cc-toolchain-config : (Listof (Pairof Symbol Any))
  '((macro _THREAD_SAFE)
    (lib SDL2 SDL2_TTF SDL2_Image SDL2_GFX SDL2_Net SDL2_Mixer)
    (lib [windows shell32])))
