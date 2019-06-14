#lang typed/racket/base

(provide (all-defined-out))

(require "cc.rkt")

(define-type CC-Options (U 'flags 'macros 'includes 'infile 'outfile))

(define-type CC-CPP-Macros (-> Symbol (Listof String)))
(define-type CC-Flags (-> Symbol (Listof String)))
(define-type CC-Includes (-> Symbol (Listof String)))

(define-type CC-IO-File-Flag (-> Path-String Symbol (Listof String)))

(struct cc c-toolchain
  ([macros : CC-CPP-Macros]
   [flags : CC-Flags]
   [includes : CC-Includes]
   [infile : CC-IO-File-Flag]
   [outfile : CC-IO-File-Flag])
  #:constructor-name make-cc
  #:type-name CC)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define cc-default-macros : CC-CPP-Macros
  (lambda [system]
    (list (format "-D__~a__" system))))

(define cc-default-io-file : CC-IO-File-Flag
  (lambda [src system]
    (list (if (path? src) (path->string src) src))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define cc-database : (HashTable Symbol CC) (make-hasheq))

(define c-register-compiler : (-> Symbol (Listof (U CC-Options String))
                                  #:macros CC-CPP-Macros #:flags CC-Flags #:includes CC-Includes
                                  [#:infile CC-IO-File-Flag] [#:outfile CC-IO-File-Flag]
                                  [#:basename (Option Symbol)]
                                  Void)
  (lambda [name layout
                #:macros macros #:flags flags #:includes includes
                #:infile [infile cc-default-io-file] #:outfile [outfile cc-default-io-file]
                #:basename [basename #false]]
    (define program : (Option Path) (c-find-binary-path (or basename name)))

    (when (path? program)
      (hash-set! cc-database name
                 (make-cc program layout
                          macros flags includes
                          infile outfile)))))
