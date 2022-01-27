#lang typed/racket/base

(require digimon/echo)

(esc-clear-screen)

(displayln 'start)
(esc-save)

(esc-cell 10 10)
(display (cons 10 10))

(esc-screen-home)
(displayln (cons 0 0))

(esc-restore)
(esc-move-down)
(displayln (cons 12 0))

(esc-move-up 1)
(esc-move-right 12)
(esc-move-to 4)
(displayln (cons 12 4))

(esc-move-down)
(esc-move-right 12)
(esc-home)
(displayln (cons 14 0))
