#lang typed/racket/gui

(module unsafe racket/gui
  (provide (all-defined-out))

  (require framework)

  (define drracket-colorize
    (lambda [file-path file-content]
      (define racket-editor (new racket:text%))
      (define DrFrame
        (new frame% [label (format "Nanomon Colorize - ~a" (file-name-from-path file-path))]
             [width 512] [height 512]))
      

      (make-object editor-canvas% DrFrame racket-editor)
      
      (send racket-editor insert file-content)
      (send DrFrame show #true)
      (send DrFrame center 'both)
      
      (with-handlers ([exn:fail? (λ [e] (send DrFrame show #false) (raise e))])
        (send racket-editor freeze-colorer)
        (send racket-editor thaw-colorer))

      (void))))

(require/typed/provide
 (submod "." unsafe)
 [drracket-colorize (-> Path-String String Void)])
