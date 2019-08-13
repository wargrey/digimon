#lang typed/racket/base

(provide (all-defined-out))

(require "digitama/cmdopt.rkt")

(require (for-syntax racket/base))
(require (for-syntax racket/syntax))
(require (for-syntax syntax/parse))

(require (for-syntax "digitama/cmdopt.rkt"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-syntax (define-cmdlet-option stx)
  (syntax-parse stx #:datum-literals [:]
    [(_ opt:id (~or #: :) Opt
        (~alt (~optional (~seq #:program name) #:defaults ([name #'(find-system-path 'run-file)]))
              (~optional (~seq #:args args-form (~optional (~seq (~or #: :) Type)) body-expr ...) #:defaults ([args-form #'()] [Type #'Void]))
              
              (~optional (~seq #:multi (~optional mlabel:str) mflags) #:defaults ([mlabel #'"multi-options"] [mflags #'()]))
              (~optional (~seq #:once-each (~optional elabel:str) eflags) #:defaults ([elabel #'"individual options"] [eflags #'()]))
              (~optional (~seq #:once-any (~optional alabel:str) aflags) #:defaults ([alabel #'"mutually exclusive options"] [aflags #'()]))
              
              ; options that occure [0, n) times
              (~seq #:banner banner:expr)
              (~seq #:usage-help desc:expr)
              (~seq #:ps ps:expr)) ...)
     (with-syntax* ([parse-option (format-id #'opt "parse-~a" #'opt)]
                    [display-option (format-id #'opt "display-~a" #'opt)]
                    [([MType mfield msize (mopt mopts ...) string->mflags mdesc ...] ...) (cmd-parse-flags #'mflags)]
                    [([EType efield esize (eopt eopts ...) string->eflags edesc ...] ...) (cmd-parse-flags #'eflags)]
                    [([AType afield asize (aopt aopts ...) string->aflags adesc ...] ...) (cmd-parse-flags #'aflags)]
                    [(args [<String> ...] [<args> ...] [<idx> ...]) (cmd-parse-args #'args-form)])
       #`(begin (struct opt ([mfield : (Listof MType)] ...
                             [efield : (Option EType)] ...
                             [afield : (Option AType)] ...)
                  #:type-name Opt
                  #:transparent)
                
                (define display-option : (->* () (Output-Port #:program Any) Void)
                  (lambda [[/dev/stdout (current-output-port)] #:program [program name]]
                    (define mwidth : Natural (max 0 msize ...))
                    (define ewidth : Natural (max 0 esize ...))
                    (define awidth : Natural (max 0 asize ...))
                    (define width : Natural (max mwidth ewidth awidth))
                    
                    (cmdopt-display-banner /dev/stdout (list banner ...))

                    (fprintf /dev/stdout "usage: ~a" (cmdopt-program-name program))
                    (when (> width 0) (fprintf /dev/stdout " [<options>]"))
                    (for ([argv (in-list '(<args> ...))]) (fprintf /dev/stdout " ~a" argv))
                    (newline /dev/stdout)

                    (cmdopt-display-usage-help /dev/stdout (list desc ...))
                    
                    (when (> mwidth 0)
                      (fprintf /dev/stdout "~n  ~a~n" (cmdopt-help-identity mlabel))
                      (cmdopt-display-flags /dev/stdout 'mopt (list 'mopts ...) string->mflags (list (cmdopt-help-identity mdesc) ...) msize width) ...)
                      
                    (when (> ewidth 0)
                      (fprintf /dev/stdout "~n  ~a~n" (cmdopt-help-identity elabel))
                      (cmdopt-display-flags /dev/stdout 'eopt (list 'eopts ...) string->eflags (list (cmdopt-help-identity edesc) ...) esize width) ...)
                    
                    (when (> awidth 0)
                      (fprintf /dev/stdout "~n  ~a~n" (cmdopt-help-identity alabel))
                      (cmdopt-display-flags /dev/stdout 'aopt (list 'aopts ...) string->aflags (list (cmdopt-help-identity adesc) ...) asize width) ...)
                    
                    (cmdopt-display-postscript /dev/stdout (list ps ...))))

                (define parse-option : (->* () ((U (Listof String) (Vectorof String))) (Values Any (Listof String) Boolean))
                  (lambda [[argv (current-command-line-arguments)]]
                    (values (void) null #true)))))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-cmdlet-option cc-flags #: CC-Flags
  #:program 'test
  #:ps (version)
  #:usage-help (current-seconds)
  #:once-each
  [[(#\v verbose) "Compile with verbose messages"]
   [(#\p profile) "Compile with profiling"]]
  #:multi
  [[(#\l link-flags) lf ; flag takes one argument
                     "1-arity option which name is ~~1~1~"
                     ["Add a flag ~1 for the linker [default: ~a]" null]]]
  #:once-any
  [[(#\o optimize-1) "Compile with optimization level 1"]
   [optimize-2       "Compile with optimization level 2,"
                     "which includes all of level 1"]]
  #:args (src ... dest) #: Flonum 
  1.0)

(define-values (options names help?) (parse-cc-flags))

(when (and help?)
  (display-cc-flags #:program '(raco wisemon)))
