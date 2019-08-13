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

  (define-syntax-class help
    (pattern (fmt:string vs ...) #:attr value #'(list fmt vs ...))
    (pattern desc:string #:attr value #'desc)
    (pattern otherwise #:attr value (raise-syntax-error 'cmdopt-parse-flags "expect string? or (list*of string? (listof any))" #'otherwise)))

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
      [([flag+aliases (~optional (~seq #: Type) #:defaults ([Type #'Boolean])) rest ...] ...)
       (for/list ([<deflag> (syntax->list #'([flag+aliases rest ...] ...))]
                  [<Type> (syntax->list #'(Type ...))])
         (cons (syntax-e <Type>) (cmd-parse-flag <deflag>)))]
      [_ (raise-syntax-error 'cmdopt-parse-flags "malformed flag definition" <flags>)]))

  (define (cmd-parse-flag <deflag>)
    (syntax-parse <deflag>
      [(flag+alias arg:id args:id ... description:help ...)
       (define-values (flags size name) (cmd-collect-flag-aliases #'flag+alias))
       (list* name size flags #false
              (for/list ([<desc> (syntax-e #'(description.value ...))])
                (cmd-format-description <desc> (syntax->datum #'(arg args ...)) #true)))]
      [(flag+alias description:help ...)
       (define-values (flags size name) (cmd-collect-flag-aliases #'flag+alias))
       (list* name size flags #false (syntax-e #'(description.value ...)))]
      [_ (raise-syntax-error 'cmdopt-parse-flags "malformed flag definition" <deflag>)]))

  (define (cmd-collect-flag-aliases <flag+alias>)
    (define flags+alias (syntax-e <flag+alias>))
    (let collect ([srahc null]
                  [sdrow null]
                  [flags (if (list? flags+alias) flags+alias (list <flag+alias>))]
                  [size -2])
      (if (null? flags)
          (let ([chars (reverse srahc)]
                [words (reverse sdrow)])
            (cond [(pair? words) (values (append chars words) size (car words))]
                  [(pair? chars) (values chars size (string->symbol (string (car chars))))]
                  [else #| deadcode |# (raise-syntax-error 'cmdopt-parse-flags "requires at least one flag" <flag+alias>)]))
          (let* ([<f> (car flags)]
                 [f (syntax-e <f>)])
            (cond [(char? f) (collect (cons f srahc) sdrow (cdr flags) (+ size 2 1 1))]
                  [(symbol? f) (collect srahc (cons f sdrow) (cdr flags) (+ size 2 2 (string-length (symbol->string f))))]
                  [else (raise-syntax-error 'cmdopt-parse-flags "expected char? or symbol?" <f>)])))))

  (define (cmd-format-description <desc> args ~~?)
    (define desc (syntax-e <desc>))
    (cond [(list? desc) (list* #'list (cmd-format-description (cadr desc) args #false) (cddr desc))]
          [else (let ([arity (length args)]
                      [/dev/stdin (open-input-string desc)]
                      [/dev/stdout (open-output-string)])
                  (let ~n ()
                    (define ch (read-char /dev/stdin))
                    (cond [(eof-object? ch) (get-output-string /dev/stdout)]
                          [(not (eq? ch #\~)) (write-char ch /dev/stdout) (~n)]
                          [else (let ([maybe-n (regexp-match #px"~?\\d*" /dev/stdin)])
                                  (cond [(or (not maybe-n) (bytes=? (car maybe-n) #"")) (write-char #\~ /dev/stdout)]
                                        [(bytes=? (car maybe-n) #"~") (unless ~~? (write-char #\~ /dev/stdout)) (write-char #\~ /dev/stdout)]
                                        [(= (bytes-ref (car maybe-n) 0) #x7E) (unless ~~? (write-char #\~ /dev/stdout)) (write-bytes (car maybe-n) /dev/stdout)]
                                        [else (let ([n (string->number (bytes->string/utf-8 (car maybe-n)))])
                                                (cond [(= n 0) (raise-syntax-error 'cmdopt-parse-flags "flag index starts with 1" <desc>)]
                                                      [(> n arity) (raise-syntax-error 'cmdopt-parse-flags "flag index too large" <desc>)]
                                                      [else (write-char #\< /dev/stdout)
                                                            (write (list-ref args (sub1 n)) /dev/stdout)
                                                            (write-char #\> /dev/stdout)]))])
                                  (~n))])))])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type Cmdopt-Help (U String (Pairof String (Listof Any))))

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

(define cmdopt-display-flags : (-> Output-Port (U Char Symbol) (Listof (U Char Symbol)) (Option Procedure) (Listof String) Natural Natural Void)
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

(define cmdopt-help-identity : (-> Cmdopt-Help String)
  (lambda [help]
    (cond [(string? help) help]
          [else (apply format help)])))
