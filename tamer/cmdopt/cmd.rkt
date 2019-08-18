#lang typed/racket/base

(provide (all-defined-out))

(require digimon/cmdopt)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-cmdlet-option cmd-flags #: Cmd-Flags
  #:program 'cmd

  #:ps "this cmdlet demonstrates multi-argument option"
  #:multi
  [[(#\+)         #:=> list user remote command #: (Listof String)
                  "execute ~3 after connecting to ~1@~2"]]
  #:once-any
  [[(#\i)         #:=> cons id id.pub #: (Pairof String String)
                  "use specific ~1 and ~2 pair"]])

(define-values (options λargv) (parse-cmd-flags))

(cond [(cmd-flags-help? options) (display-cmd-flags)]
      [else (with-handlers ([exn:fail:user? (λ [[e : exn:fail:user]] (display-cmd-flags #:user-error e #:exit 1))])
              (list options (λargv)))])
