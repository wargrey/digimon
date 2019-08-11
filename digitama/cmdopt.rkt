#lang typed/racket/base

(provide (all-defined-out))
(provide (for-syntax (all-defined-out)))

(require racket/path)
(require racket/string)
(require racket/format)

(require (for-syntax racket/base))
(require (for-syntax racket/syntax))
(require (for-syntax syntax/parse))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(begin-for-syntax
  (define hlpfmt "<~a>")

  (define (cmd-parse-args <args>)
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
        (list <args> (list #'String #'*) (list (format-id <args> hlpfmt args)) (list))))
  
  (define (cmd-parse-flags <flags>)
    (syntax-parse <flags>
      [(deflag ...) (map cmd-parse-flag (syntax->list #'(deflag ...)))]
      [_ (raise-syntax-error 'cmdopt-parse-flags "malformed flag definition" <flags>)]))

  (define (cmd-parse-flag <flag>)
    (syntax-parse <flag>
      [(flag+alias description ...)
       (define-values (chars words size) (cmd-collect-flag-aliases #'flag+alias))
       (list* size (append chars words) #false (syntax->datum #'(description ...)))]
      [_ (raise-syntax-error 'cmdopt-parse-flags "malformed flag definition" <flag>)]))

  (define (cmd-collect-flag-aliases <flag+alias>)
    (define flags+alias (syntax-e <flag+alias>))
    (let collect ([srahc null]
                  [sdrow null]
                  [flags (if (list? flags+alias) flags+alias (list <flag+alias>))]
                  [size -2])
      (if (null? flags)
          (values (reverse srahc) (reverse sdrow) size)
          (let* ([<f> (car flags)]
                 [f (syntax-e <f>)])
            (cond [(char? f) (collect (cons f srahc) sdrow (cdr flags) (+ size 2 1 1))]
                  [(symbol? f) (collect srahc (cons f sdrow) (cdr flags) (+ size 2 2 (string-length (symbol->string f))))]
                  [else (raise-syntax-error 'cmdopt-parse-flags "expected char? or symbol?" <f>)]))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define cmdopt-program-name : (-> Any Any)
  (lambda [program]
    (cond [(path? program) (file-name-from-path program)]
          [(list? program) (string-join (map ~a program))]
          [else program])))

(define cmdopt-display-banner : (-> Output-Port (Listof Any) Void)
  (lambda [/dev/stdout banners0]
    (define banners : (Listof Any) (filter values banners0))
    
    (when (pair? banners)
      (for ([heading (in-list banners)])
        (displayln heading /dev/stdout))

      (newline /dev/stdout))))

(define cmdopt-display-usage-help : (-> Output-Port (Listof Any) Void)
  (lambda [/dev/stdout descriptions0]
    (define descriptions : (Listof Any) (filter values descriptions0))
    
    (when (pair? descriptions)
      (for ([description (in-list descriptions)])
        (fprintf /dev/stdout "  ~a~n" description)))))

(define cmdopt-display-flags : (-> Output-Port (U Char Symbol) (Listof (U Char Symbol)) (Option Procedure) (Listof Any) Natural Natural Void)
  (lambda [/dev/stdout flag alias transform descriptions0 this-width max-width]
    (fprintf /dev/stdout "    ~a~a" (if (char? flag) #\- '--) flag)
    (for ([alias (in-list alias)])
      (fprintf /dev/stdout ", ~a~a" (if (char? alias) #\- '--) alias))

    (let ([padding (- max-width this-width)])
      (when (> padding 0)
        (fprintf /dev/stdout (make-string padding #\space))))

    (let ([descriptions (filter values descriptions0)])
      (when (pair? descriptions)
        (fprintf /dev/stdout "  ~a" (car descriptions))
        (when (pair? (cdr descriptions))
          (define leading (make-string (+ max-width 4 2) #\space))

          (for ([desc (in-list (cdr descriptions))])
            (fprintf /dev/stdout "~n~a~a" leading desc)))

        (newline /dev/stdout)))))

(define cmdopt-display-postscript : (-> Output-Port (Listof Any) Void)
  (lambda [/dev/stdout ps0]
    (define postscripts : (Listof Any) (filter values ps0))
    
    (when (pair? postscripts)
      (newline /dev/stdout)
      (for ([postscript (in-list postscripts)])
        (fprintf /dev/stdout "~a~n" postscript)))))
