#lang typed/racket/base

(provide (all-defined-out))

(require "cc.rkt")
(require "../toolchain.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type CC-Options (U 'flags 'macros 'includes 'infile 'outfile))

(define-type CC-CPP-Macros (-> Symbol Boolean (Listof String)))
(define-type CC-Flags (-> Symbol Boolean (Listof String)))
(define-type CC-Includes (-> Symbol Boolean (Listof String)))

(define-type CC-IO-File-Flag (-> Path-String Symbol Boolean (Listof String)))

(struct cc toolchain
  ([macros : CC-CPP-Macros]
   [flags : CC-Flags]
   [includes : CC-Includes]
   [infile : CC-IO-File-Flag]
   [outfile : CC-IO-File-Flag]
   [++ : Path])
  #:constructor-name make-cc
  #:type-name CC)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define cc-default-macros : CC-CPP-Macros
  (lambda [system cpp?]
    (list (format "-D__~a__" system))))

(define cc-default-io-file : CC-IO-File-Flag
  (lambda [src system cpp?]
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
    (define cc : Symbol (or basename name))
    (define program : (Option Path) (c-find-binary-path cc))
    (define program++ : (Option Path) (c-find-binary-path (c-cpp-partner cc)))

    (when (path? program)
      (hash-set! cc-database name
                 (make-cc program layout
                          macros flags includes
                          infile outfile
                          (or program++ program))))))
