#lang typed/racket/base

(provide (all-defined-out))

(require digimon/cmdopt)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-cmdlet-option scp-flags #: SCP-Flags
  #:program 'scp
  #:args (src ... dest)

  #:banner "secure copy (remote file copy program)"
  #:ps "this cmdlet demonstrates optional arguments appearing before the last one"
  #:ps "there must be at least one optional argument"
  #:once-each
  [[(#\P port)    port
                  "specifies the ~1 to connect to on the remote host"]
   [(#\p)         "preserves modification times, access times, and modes from the original file"]]
  #:multi
  [[(#\v verbose) "verbose mode"
                  "multiple -v options increase the verbosity"]]
  #:once-any
  [[(#\4)         "use IPv4 addresses only"]
   [(#\6)         "use IPv6 addresses only"]])

(define-values (options λargv) (parse-scp-flags))

(define scp : (-> (Listof String) String Void)
  (lambda [srcs dest]
    (displayln (list options srcs dest))))

(cond [(scp-flags-help? options) (display-scp-flags)]
      [else (with-handlers ([exn:fail:user? (λ [e] (display-scp-flags) (exit 1))])
              (apply scp (λargv)))])
