#lang typed/racket/gui

(module unsafe racket/gui
  (provide (all-defined-out))

  (require framework)
    
  (define lang-frame%
    (class frame%
      (init-field file-path racket-editor env-thread)

      (super-new [label (format "Nanomon Colorize - ~a" (file-name-from-path file-path))]
                 [width 800] [height 600]
                 [stretchable-width #true] [stretchable-height #true])

      (make-object editor-canvas%
        this racket-editor '(auto-vscroll auto-hscroll))

      (define/augment (on-close)
        (thread-send env-thread eof))))

  (define drracket-colorize
    (lambda [file-path file-content env-thread]
      (define leditor (new racket:text%))
      
      (define lframe
        (make-object lang-frame%
          file-path leditor env-thread))
      
      (send leditor insert file-content)
      (send lframe show #true)
      (send lframe center 'both)

      (send leditor freeze-colorer)
      (send leditor thaw-colorer))))

(require/typed/provide
 (submod "." unsafe)
 [drracket-colorize (-> Path-String String Thread Void)])
