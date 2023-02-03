#lang typed/racket/base

(provide (all-defined-out))

(require racket/path)
(require racket/string)

(require "cc.rkt")
(require "../toolchain.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type CC-Macros (Listof (Pairof String (Option String))))
(define-type CC-Options (U 'flags 'macros 'includes 'infile 'outfile))

(define-type CC-CPP-Macros (-> CC-Macros Symbol Boolean CC-Macros (Listof String)))
(define-type CC-Flags (-> Symbol Boolean (Listof Any) Boolean Boolean (Listof String)))
(define-type CC-Includes (-> (Listof Path) Symbol Boolean (Listof String)))

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
(define cc-default-macros : (-> Symbol Boolean Boolean (Listof (Pairof String (Option String))))
  (lambda [system cpp? debug?]
    (define macros : (Listof (Pairof String (Option String)))
      (list (cons (format "__~a__" system) #false)
            (cons "__lambda__" (if (eq? system 'windows) "__declspec(dllexport)" ""))
            (cons "__ffi__" (if (eq? system 'windows) "__declspec(dllexport)" ""))
            (cons "__ZONE__" (string-append "\"" (string-replace (path->string (current-directory)) "\\" "\\\\") "\""))))

    (cond [(not debug?) (cons (cons (format "NDEBUG" system) #false) macros)]
          [else macros])))

(define cc-default-io-file : CC-IO-File-Flag
  (lambda [src system cpp?]
    (list (some-system-path->string (find-relative-path (current-directory) src)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define cc-database : (HashTable Symbol CC) (make-hasheq))

(define c-register-compiler : (-> Symbol (Listof (U CC-Options String))
                                  #:macros CC-CPP-Macros #:flags CC-Flags #:includes CC-Includes
                                  [#:infile CC-IO-File-Flag] [#:outfile CC-IO-File-Flag]
                                  [#:basename (Option Symbol)] [#:find-compiler (-> Symbol (Option Path))]
                                  [#:env (U False Environment-Variables (-> Environment-Variables))]
                                  Void)
  (lambda [name layout
                #:macros macros #:flags flags #:includes includes
                #:infile [infile cc-default-io-file] #:outfile [outfile cc-default-io-file]
                #:basename [basename #false] #:env [env #false] #:find-compiler [find-compiler c-find-binary-path]]
    (define cc : Symbol (or basename name))
    (define program : (Option Path) (find-compiler cc))
    (define program++ : (Option Path) (find-compiler (c-cpp-partner cc)))

    (when (path? program)
      (hash-set! cc-database name
                 (make-cc program layout env
                          macros flags includes
                          infile outfile
                          (or program++ program))))))
