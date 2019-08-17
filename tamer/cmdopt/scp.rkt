#lang typed/racket/base

(provide (all-defined-out))

(require digimon/cmdopt)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-cmdlet-option scp-flags #: SCP-Flags
  #:program 'scp
  #:args (src ... dest)

  #:banner "secure copy (remote file copy program)"
  #:ps "these cmdlet demonstrates optional arguments appearing before the last one"
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

(define-values (options help? λargv) (parse-scp-flags))

(unless (not help?)
  (display-scp-flags))

(with-handlers ([exn:fail:user? (λ [e] (display-scp-flags) (exit 1))])
  (let-values ([(srcs dest) (λargv)])
    (list options srcs dest)))
