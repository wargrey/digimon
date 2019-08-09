#lang typed/racket/base

(provide (all-defined-out))
(provide (for-syntax (all-defined-out)))

(require (for-syntax racket/base))
(require (for-syntax racket/syntax))
(require (for-syntax syntax/parse))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(begin-for-syntax
  (define (cmd-parse-args <args>)
    (define hlpfmt "<~a>")
    (define args (syntax-e <args>))
    
    (if (pair? args)
        (let transform ([sgra null]
                        [sepyt null]
                        [spleh null]
                        [scidni null]
                        [args args])
          (cond [(pair? args)
                 (let ([<arg> (car args)])
                   (cond [(not (eq? (syntax-e <arg>) '...))
                          (transform (cons <arg> sgra)
                                     (cons #'String sepyt)
                                     (cons (format-id <arg> hlpfmt <arg>) spleh)
                                     (cons (datum->syntax <arg> (length scidni)) scidni)
                                     (cdr args))]
                         [(null? (cddr args))
                          (list (datum->syntax <args> (reverse (cons (cadr args) sgra)))
                                (reverse (list* #'String #'(Listof String) (cdr sepyt)))
                                (reverse (list* (format-id (cadr args) hlpfmt (cadr args)) <arg> spleh))
                                (reverse scidni))]
                         [else (raise-syntax-error 'cmd-parse-args "misplaced '...'" <arg>)]))]
                [(null? args) (list <args> (reverse sepyt) (reverse spleh) (reverse scidni))]
                [else (list <args>
                            (reverse (list* #'* #'String sepyt))
                            (reverse (list* #'[... ...] (format-id args hlpfmt args) spleh))
                            (reverse scidni))]))
        (list <args> (list #'String #'*) (list (format-id <args> hlpfmt args)) (list)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
