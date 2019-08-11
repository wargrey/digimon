#lang typed/racket/base

(provide (all-defined-out))

(require "digitama/cmdopt.rkt")

(require (for-syntax racket/base))
(require (for-syntax racket/syntax))
(require (for-syntax syntax/parse))

(require (for-syntax "digitama/cmdopt.rkt"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-syntax (define-cmdopt-parser stx)
  (syntax-parse stx #:datum-literals [:]
    [(_ id:id
        (~alt (~optional (~seq #:program name) #:defaults ([name #'(find-system-path 'run-file)]))
              (~optional (~seq #:args args-form (~optional (~seq (~or #: :) Type)) body-expr ...) #:defaults ([args-form #'()] [Type #'Void]))

              (~optional (~seq #:multi (~optional mlabel:str) mflags) #:defaults ([mlabel #'"<options> that can be specified any number of times"] [mflags #'()]))
              (~optional (~seq #:once-each (~optional elabel:str) eflags) #:defaults ([elabel #'"<options> that can be specified at most once"] [eflags #'()]))
              (~optional (~seq #:once-any (~optional alabel:str) aflags) #:defaults ([alabel #'"<options> that are mutually exclusive"] [aflags #'()]))
              
              ; options that occure [0, n) times
              (~seq #:banner banner:expr)
              (~seq #:usage-help desc:expr)
              (~seq #:ps ps:expr))
        ...)
     (with-syntax* ([display-help (format-id #'id "display-~a-help" #'id)]
                    [([msize (mopt mopts ...) string->mflags mdesc ...] ...) (cmd-parse-flags #'mflags)]
                    [([esize (eopt eopts ...) string->eflags edesc ...] ...) (cmd-parse-flags #'eflags)]
                    [([asize (aopt aopts ...) string->aflags adesc ...] ...) (cmd-parse-flags #'aflags)]
                    [(args [<String> ...] [<args> ...] [<idx> ...]) (cmd-parse-args #'args-form)])
       #`(begin (define display-help : (->* () (Output-Port #:program Any) Void)
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
                      (fprintf /dev/stdout "~n  ~a~n" mlabel)
                      (cmdopt-display-flags /dev/stdout 'mopt (list 'mopts ...) string->mflags (list mdesc ...) msize width) ...)
                      
                    (when (> ewidth 0)
                      (fprintf /dev/stdout "~n  ~a~n" elabel)
                      (cmdopt-display-flags /dev/stdout 'eopt (list 'eopts ...) string->eflags (list edesc ...) esize width) ...)
                    
                    (when (> awidth 0)
                      (fprintf /dev/stdout "~n  ~a~n" alabel)
                      (cmdopt-display-flags /dev/stdout 'aopt (list 'aopts ...) string->aflags (list adesc ...) asize width) ...)
                    
                    (cmdopt-display-postscript /dev/stdout (list ps ...))))

                (define id : (->* () ((U (Listof String) (Vectorof String)) #:/dev/hlpout Output-Port #:program Any) Any)
                  (lambda [[argv (current-command-line-arguments)] #:/dev/hlpout [/dev/hlpout (current-output-port)] #:program [program name]]
                    (define main : (-> <String> ... Type)
                      (lambda args
                        (void)
                        body-expr ...))
                    
                    (writeln '(-> <String> ... Type))
                    (writeln (map symbol->string '(<args> ...)))
                    (writeln (list '<idx> ...))))))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-cmdopt-parser id
  #:program 'test
  #:ps (version)
  #:usage-help (current-seconds)
  #:once-each
  [[(#\v verbose) "Compile with verbose messages"]
   [(#\p profile) "Compile with profiling"]]
  #:multi
  [[(#\l link-flags) "lf" ; flag takes one argument
                     "Add a flag <lf> for the linker"]]
  #:once-any
  [[(#\o optimize-1) "Compile with optimization level 1"]
   [optimize-2       "Compile with optimization level 2,"
                     "which includes all of level 1"]]
  #:args (src ... dest) #: Flonum 
  1.0)

(display-id-help #:program '(raco wisemon))
