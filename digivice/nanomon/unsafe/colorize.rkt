#lang typed/racket/gui

(module unsafe racket/gui
  (provide (all-defined-out))

  (require framework)

  (define lang-canvas%
    (class editor-canvas%
      (init-field drframe racket-editor /dev/langout)

      (super-new [parent drframe]
                 [editor racket-editor]
                 [style '(auto-vscroll auto-hscroll)])

      (define/override (on-superwindow-show shown?)
        (super on-superwindow-show shown?)
        
        (when (not shown?)
          (write-special eof /dev/langout)))))

  (define drracket-colorize
    (lambda [file-path file-content]
      (define-values (/dev/langin /dev/langout) (make-pipe-with-specials #false '/dev/langin '/dev/langout))
      (define racket-editor (new racket:text%))
      
      (define DrFrame
        (new frame%
             [label (format "Nanomon Colorize - ~a" (file-name-from-path file-path))]
             [width 800] [height 600]
             [stretchable-width #true] [stretchable-height #true]))
      
      (make-object lang-canvas% DrFrame racket-editor /dev/langout)
      
      (send racket-editor insert file-content)
      (send DrFrame show #true)
      (send DrFrame center 'both)

      (with-handlers ([exn:fail? (λ [e] (write-special e /dev/langout))]
                      [exn:break? (λ [e] (write-special eof /dev/langout))])
        (send racket-editor freeze-colorer)
        (send racket-editor thaw-colorer)
        
        (sync/enable-break /dev/langin))
      
      (let ([datum (read-byte-or-special /dev/langin)])
        (send DrFrame show #false)
        
        (when (exn? datum)
          (raise datum))))))

(require/typed/provide
 (submod "." unsafe)
 [drracket-colorize (-> Path-String String Void)])
