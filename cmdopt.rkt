#lang typed/racket/base

(provide (all-defined-out))

(require "digitama/cmdopt.rkt")

(require (for-syntax racket/base))
(require (for-syntax racket/syntax))
(require (for-syntax syntax/parse))
(require (for-syntax racket/string))

(require (for-syntax "digitama/cmdopt.rkt"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-syntax (define-cmdlet-option stx)
  (syntax-parse stx #:datum-literals [:]
    [(_ opt:id (~or #: :) Opt
        (~alt (~optional (~seq #:program name) #:defaults ([name #'(find-system-path 'run-file)]))
              (~optional (~seq #:args args-form) #:defaults ([args-form #'()]))
              
              (~optional (~seq #:multi (~optional mlabel:str) mflags) #:defaults ([mlabel #'"multi-options"] [mflags #'()]))
              (~optional (~seq #:once-each (~optional elabel:str) eflags) #:defaults ([elabel #'"individual options"] [eflags #'()]))
              (~optional (~seq #:once-any (~optional alabel:str) aflags) #:defaults ([alabel #'"mutually exclusive options"] [aflags #'()]))
              
              ; options that occure [0, n) times
              (~seq #:banner banner:expr)
              (~seq #:usage-help desc:expr)
              (~seq #:ps ps:expr)) ...)
     (with-syntax* ([parse-option (format-id #'opt "parse-~a" #'opt)]
                    [display-option (format-id #'opt "display-~a" #'opt)]
                    [opt-construct (datum->syntax #'opt (gensym (syntax-e #'opt)))]
                    [(flags [mfield MType margc msize (moptions ...) string->mflags mopt-ref mdesc ...] ...) (cmd-parse-flags #'mflags (make-hasheq) #true)]
                    [(flags [efield EType eargc esize (eoptions ...) string->eflags eopt-ref edesc ...] ...) (cmd-parse-flags #'eflags (syntax-e #'flags) #false)]
                    [(flags [afield AType aargc asize (aoptions ...) string->aflags aopt-ref adesc ...] ...) (cmd-parse-flags #'aflags (syntax-e #'flags) #false)]
                    [([Type ...] [argu ...] [<args> ...] [ref ...]) (cmd-parse-args #'args-form)])
       #`(begin (struct opt ([mfield : (Listof MType)] ...
                             [efield : (Option EType)] ...
                             [afield : (Option AType)] ...
                             [help? : Boolean])
                  #:constructor-name opt-construct
                  #:type-name Opt
                  #:transparent)
                
                (define parse-option : (->* () ((U (Listof String) (Vectorof String)) #:program Any) (Values Opt (-> (List Type ...))))
                  (lambda [[argv (current-command-line-arguments)] #:program [program name]]
                    (define-values (options multi-options operands help?)
                      (let ([mfield (λ [opt] : (Pairof Any (List Symbol Byte Symbol)) (cons opt (list 'mfield margc 'multi)))] ...
                            [efield (λ [opt] : (Pairof Any (List Symbol Byte Symbol)) (cons opt (list 'efield eargc 'once-each)))] ...
                            [afield (λ [opt] : (Pairof Any (List Symbol Byte Symbol)) (cons opt (list 'afield aargc 'once-any)))] ...)
                        (cmdopt-parse-arguments program argv
                                                (make-immutable-hasheq (append (map mfield (list 'moptions ...)) ...
                                                                               (map efield (list 'eoptions ...)) ...
                                                                               (map afield (list 'aoptions ...)) ...))
                                                (list 'afield ...))))

                    (define cmdopt : Opt
                      (with-handlers ([exn:fail? (λ [[ef : exn:fail]] (cmdopt-error program (exn-message ef)))])
                        (opt-construct ((inst mopt-ref MType) multi-options 'mfield string->mflags) ...
                                       ((inst eopt-ref EType) options 'efield string->eflags) ...
                                       ((inst aopt-ref AType) options 'afield string->aflags) ...
                                       help?)))
                    
                    (values cmdopt (λ [] (let*-values ([(idx) 0]
                                                       [(argu idx) (ref program 'argu operands idx)] ...)
                                           (when (< idx (vector-length operands))
                                             (cmdopt-error program "too many arguments"))
                                           (list argu ...))))))
                
                (define display-option : (->* () (Output-Port #:program Any #:user-error (Option exn:fail:user) #:exit (Option Byte)) Void)
                  (lambda [[/dev/stdout (current-output-port)] #:program [program name] #:user-error [e #false] #:exit [retcode #false]]
                    (define mwidth : Natural (max 0 msize ...))
                    (define ewidth : Natural (max 0 esize ...))
                    (define awidth : Natural (max 0 asize ...))
                    (define width : Natural (max mwidth ewidth awidth))

                    (unless (not e)
                      (fprintf /dev/stdout "~a~n~n" (exn-message e)))
                    
                    (cmdopt-display-banner /dev/stdout (list banner ...))

                    (fprintf /dev/stdout "usage: ~a" (cmdopt-program-name program))
                    (when (> width 0) (fprintf /dev/stdout " [<options>]"))
                    (cmdopt-display-args /dev/stdout (list '<args> ...))

                    (cmdopt-display-usage-help /dev/stdout (list desc ...))

                    (when (> awidth 0)
                      (fprintf /dev/stdout "~n  ~a~n" (cmdopt-help-identity alabel))
                      (cmdopt-display-flags /dev/stdout (list 'aoptions ...) (list (cmdopt-help-identity adesc) ...) asize width) ...)
                    
                    (when (> ewidth 0)
                      (fprintf /dev/stdout "~n  ~a~n" (cmdopt-help-identity elabel))
                      (cmdopt-display-flags /dev/stdout (list 'eoptions ...) (list (cmdopt-help-identity edesc) ...) esize width) ...)
                    
                    (when (> mwidth 0)
                      (fprintf /dev/stdout "~n  ~a~n" (cmdopt-help-identity mlabel))
                      (cmdopt-display-flags /dev/stdout (list 'moptions ...) (list (cmdopt-help-identity mdesc) ...) msize width) ...)
                    
                    (cmdopt-display-postscript /dev/stdout (list ps ...))

                    (unless (not retcode)
                      (exit retcode))))))]))
