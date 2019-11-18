#lang typed/racket/base

(provide (all-defined-out))
(provide (for-syntax (all-defined-out)))

(require racket/path)
(require racket/string)
(require racket/vector)
(require racket/list)

(require "../format.rkt")

(require (for-syntax racket/base))
(require (for-syntax racket/syntax))
(require (for-syntax syntax/parse))
(require (for-syntax racket/string))
(require (for-syntax racket/list))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(begin-for-syntax
  (define hlpfmt "<~a>")
  (define fmargc 255)

  (define-syntax-class help
    (pattern (fmt:string vs ...) #:attr value #'(list fmt vs ...))
    (pattern desc:string #:attr value #'desc)
    (pattern otherwise #:attr value (raise-syntax-error 'cmdopt-parse-flags "expect string? or (list*of string? (listof any))" #'otherwise)))

  (define (make-cmdopt-ref <n>)
    (define n (syntax-e <n>))
    (define maybe-flags (datum->syntax <n> 'maybe-flags))
    
    #`(λ #:forall (a) [[opts : (HashTable Symbol (Listof String))] [name : Symbol] [strings->option : (-> #,@(make-list n #'String) a)]] : (Option a)
        (define #,maybe-flags : (Listof String) (hash-ref opts name (λ [] null)))
        
        (and (= (length #,maybe-flags) #,n)
             (strings->option #,@(build-list n (λ [i] (datum->syntax <n> (list #'list-ref maybe-flags i))))))))
  
  (define (make-cmdmopt-ref <n>)
    (define n (syntax-e <n>))
    (define flags (datum->syntax <n> 'flags))
    
    #`(λ #:forall (a) [[opts : (HashTable Symbol (Listof (Listof String)))] [name : Symbol] [strings->option : (-> #,@(make-list n #'String) a)]] : (Listof a)
        (define flagses : (Listof (Listof String)) (hash-ref opts name (λ [] null)))
        
        (filter-map (λ [[#,flags : (Listof String)]]
                      (and (= (length #,flags) #,n)
                           (strings->option #,@(build-list n (λ [i] (datum->syntax <n> (list #'list-ref flags i)))))))
                    (reverse flagses))))
  
  (define (cmd-parse-flags <flags> flagnames multiple?)
    (syntax-parse <flags>
      [(deflag ...) (cons flagnames
                          (for/list ([<deflag> (syntax->list #'(deflag ...))])
                            (cmd-parse-flag <deflag> flagnames multiple?)))]
      [_ (raise-syntax-error 'cmdopt-parse-flags "malformed flag definition" <flags>)]))

  (define (cmd-parse-flag <deflag> flagnames multiple?)
    (syntax-parse <deflag> #:datum-literals [: =>]
      [(flag+alias (~optional (~seq (~or #:=> =>) string->datum) #:defaults ([string->datum #'values]))
                   arg:id args:id ...
                   (~optional (~seq (~or #: :) Type) #:defaults ([Type #'String]))
                   description:help ...)
       (define-values (flags size name) (cmd-collect-flag-aliases #'flag+alias flagnames))
       (define argv (syntax-e #'(arg args ...)))
       (define arity (length argv))

       (unless (<= arity fmargc)
         (let ([over-argv (list-tail argv fmargc)])
           (raise-syntax-error 'cmdopt-parse-flags (format "no more than ~a arguments" fmargc)
                               (car over-argv) #false (cdr over-argv))))

       (list* name #'Type arity size flags #'string->datum
              (cond [(= arity 1) (if multiple? #'cmdmopt1-ref #'cmdopt1-ref)]
                    [else ((if multiple? make-cmdmopt-ref make-cmdopt-ref) (datum->syntax #'arg arity))])
              (let ([argv-data (map syntax-e argv)])
                (cons (cmd-format-description (cmd-flag-arguments-description arity) arity argv-data #true)
                      (for/list ([<desc> (syntax-e #'(description.value ...))])
                        (cmd-format-description <desc> arity argv-data #true)))))]
      [(flag+alias description:help ...)
       (define-values (flags size name) (cmd-collect-flag-aliases #'flag+alias flagnames))
       (list* name #'Boolean 0 size flags #false (if multiple? #'cmdmopt0-ref #'cmdopt0-ref)
              (syntax-e #'(description.value ...)))]
      [_ (raise-syntax-error 'cmdopt-parse-flags "malformed flag definition" <deflag>)]))

  (define (cmd-collect-flag-aliases <flag+alias> flagnames)
    (define flags+alias (syntax-e <flag+alias>))
    (let collect ([srahc null]
                  [sdrow null]
                  [flags (if (list? flags+alias) flags+alias (list <flag+alias>))]
                  [size -2])
      (if (null? flags)
          (let ([chars (reverse srahc)]
                [words (reverse sdrow)])
            (cond [(pair? words) (values (append chars words) size (cmd-flag->name (car words)))]
                  [(pair? chars) (values chars size (cmd-flag->name (car chars)))]
                  [else #| deadcode |# (raise-syntax-error 'cmdopt-parse-flags "requires at least one flag" <flag+alias>)]))
          (let* ([<f> (car flags)]
                 [f (syntax-e <f>)])
            (cond [(char? f) (collect (cons f srahc) sdrow (cdr flags) (+ size (cmd-option-size f <f> flagnames)))]
                  [(symbol? f) (collect srahc (cons f sdrow) (cdr flags) (+ size (cmd-option-size f <f> flagnames)))]
                  [else (raise-syntax-error 'cmdopt-parse-flags "expected char? or symbol?" <f>)])))))
  
  (define (cmd-option-size flag <flag> flagnames)
    (define name (cmd-flag->name flag))

    (when (hash-has-key? flagnames name)
      (raise-syntax-error 'cmdopt-parse-flags "duplicate flags"
                          <flag> #false (list (hash-ref flagnames name))))

    (when (memq name '(help -))
      (raise-syntax-error 'cmdopt-parse-flags "reserved options" <flag>))

    (hash-set! flagnames name <flag>)

    (cond [(char? flag) (+ 2 1 1)]
          [else (+ 2 2 (string-length (symbol->string flag)))]))

  (define (cmd-flag->name flag)
    (cond [(char? flag) (string->symbol (string flag))]
          [else flag]))

  (define (cmd-format-description <desc> arity args ~~?)
    (define desc (syntax-e <desc>))
    (cond [(list? desc) (list* #'list (cmd-format-description (cadr desc) arity args #false) (cddr desc))]
          [else (let ([/dev/stdin (open-input-string desc)]
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
                                  (~n))])))]))

  (define (cmd-flag-arguments-description arity)
    (datum->syntax #false
                   (string-join (build-list arity
                                            (λ [i] (format "~~~a" (+ i 1)))))))

  (define (cmd-parse-args <args>)
    (define args (syntax-e <args>))
    
    (cond [(pair? args)
           (let transform ([sepyt null]
                           [sgra null]
                           [spleh null]
                           [sfer null]
                           [args args])
             (cond [(pair? args)
                    (let ([<arg> (car args)])
                      (cond [(not (eq? (syntax-e <arg>) '...))
                             (transform (cons #'String sepyt)
                                        (cons <arg> sgra)
                                        (cons (format-id <arg> hlpfmt <arg>) spleh)
                                        (cons #'cmdarg-ref sfer)
                                        (cdr args))]
                            [(and (null? (cddr args)) (pair? sepyt))
                             (list (reverse (list* #'String #'(Listof String) (cdr sepyt)))
                                   (reverse (list* (cadr args) sgra))
                                   (reverse (list* (format-id (cadr args) hlpfmt (cadr args)) <arg> spleh))
                                   (reverse (list* #'cmdarg-ref #'cmdarg...-ref (cdr sfer))))]
                            [else (raise-syntax-error 'cmd-parse-args "misplaced '...'" <arg>)]))]
                   [(null? args) (list (reverse sepyt) (reverse spleh) (reverse sgra) (reverse sfer))]
                   [else ; improper list of arguments
                    (list (reverse (list* #'(Listof String) sepyt))
                          (reverse (cons <args> sgra))
                          (reverse (list* #'[... ...] (format-id args hlpfmt args) spleh))
                          (reverse #'cmdargs*-ref sfer))]))]
          [(not (null? args))
           (list (list #'(Listof String))
                 (list <args>)
                 (list (format-id <args> hlpfmt args) #'[... ...])
                 (list #'cmdargs*-ref))]
          [else (list null null null null)])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type Cmdopt-Help (U String (Pairof String (Listof Any))))
(define-type Cmdopt-Metaflag (Immutable-HashTable Any (List Symbol Byte Symbol)))
(define-type Cmdopt-Flags (HashTable Symbol (Listof String)))
(define-type Cmdopt-MFlags (HashTable Symbol (Listof (Listof String))))

(define vector-null : (Vectorof String) (vector))

(define cmdopt-parse-arguments : (-> Any (U (Listof String) (Vectorof String)) String Cmdopt-Metaflag (Listof Symbol)
                                     (Values Cmdopt-Flags Cmdopt-MFlags (Vectorof String) Boolean))
  (lambda [pname arguments --help all-flags oa-names]
    (define options : Cmdopt-Flags (make-hasheq))
    (define moptions : Cmdopt-MFlags (make-hasheq))
    (define argv : (Vectorof String) (if (list? arguments) (list->vector arguments) arguments))
    (define argc : Index (vector-length argv))
    
    (let parse ([i : Nonnegative-Fixnum 0]
                [help? : Boolean #false])
      (cond [(>= i argc) (values options moptions vector-null help?)]
            [else (let ([token (vector-ref argv i)])
                    (cond [(string=? token "") (parse (+ i 1) help?)]
                          [(string=? token "--") (values options moptions (vector-drop argv (+ i 1)) help?)]
                          [(string=? token --help) (parse (+ i 1) #true)]
                          [(cmdopt-operand? token) (values options moptions (if (eq? i 0) argv (vector-drop argv i)) help?)]
                          [(eq? (string-ref token 1) #\-)
                           (let ([--arg (string->symbol (substring token 2))])
                             (parse (cmdopt-parse-flags pname --arg argv argc all-flags oa-names options moptions (+ i 1)) help?))]
                          [else (parse (for/fold ([idx : Positive-Fixnum (+ i 1)])
                                                 ([-a (in-string (substring token 1))])
                                         (cmdopt-parse-flags pname -a argv argc all-flags oa-names options moptions idx))
                                       help?)]))]))))

(define cmdopt-parse-flags : (-> Any Any (Vectorof String) Index Cmdopt-Metaflag (Listof Symbol) Cmdopt-Flags Cmdopt-MFlags Positive-Fixnum Positive-Fixnum)
  (lambda [pname -/-- argv argc all-flags oa-names options moptions idx]
    (define flag.argc.type : (List Symbol Byte Symbol) (hash-ref all-flags -/-- (λ [] (cmdopt-error pname "unrecognized option: ~a" -/--))))
    (define-values (flag fargc type) (values (car flag.argc.type) (cadr flag.argc.type) (caddr flag.argc.type)))
    
    (unless (eq? type 'multi)
      (when (hash-has-key? options flag)
        (cmdopt-error pname "individual option has already been specified: ~a" -/--))
    
      (when (eq? type 'once-any)
        (for ([once-any-flag (in-list oa-names)])
          (when (hash-has-key? options once-any-flag)
            (cmdopt-error pname "mutually exclusive option(~a) has already been specified: ~a" once-any-flag -/--)))))

    (define-values (fargv nidx)
      (cond [(= fargc 0) (values (list ".") idx)]
            [else (let ([nidx (+ idx fargc)]
                        [given (- argc idx)])
                    (cond [(> nidx argc) (cmdopt-error pname "option(~a) expects ~a, but given ~a" -/-- (~n_w fargc "argument") (~n_w given "argument"))]
                          [(> fargc 1) (values (build-list fargc (λ [[i : Index]] (vector-ref argv (+ idx i)))) nidx)]
                          [else (values (list (vector-ref argv idx)) nidx)]))]))

    (cond [(not (eq? type 'multi)) (hash-set! options flag fargv)]
          [else (hash-set! moptions flag (cons fargv (hash-ref moptions flag (λ [] null))))])
    
    nidx))

(define cmdopt-operand? : (-> String Boolean)
  (lambda [arg]
    (or (not (eq? (string-ref arg 0) #\-))
        (string=? arg "-"))))

(define cmdopt0-ref : (All (a) (-> (HashTable Symbol (Listof String)) Symbol False Boolean))
  (lambda [opts name string->option]
    (pair? (hash-ref opts name (λ [] null)))))


(define cmdopt1-ref : (All (a) (-> (HashTable Symbol (Listof String)) Symbol (-> String a) (Option a)))
  (lambda [opts name string->option]
    (define maybe-flag : (Listof String) (hash-ref opts name (λ [] null)))

    (and (pair? maybe-flag)
         (string->option (car maybe-flag)))))

(define cmdmopt0-ref : (All (a) (-> (HashTable Symbol (Listof (Listof String))) Symbol False (Listof True)))
  (lambda [opts name string->option]
    (define flags : (Listof (Listof String)) (hash-ref opts name (λ [] null)))
    (make-list (length flags) #true)))

(define cmdmopt1-ref : (All (a) (-> (HashTable Symbol (Listof (Listof String))) Symbol (-> String a) (Listof a)))
  (lambda [opts name string->option]
    (define flagses : (Listof (Listof String)) (hash-ref opts name (λ [] null)))
    (filter-map (λ [[flags : (Listof String)]] (and (pair? flags) (string->option (car flags))))
                (reverse flagses))))

(define cmdarg-ref : (-> Any Symbol (Vectorof String) Nonnegative-Fixnum (Values String Nonnegative-Fixnum))
  (lambda [pname argu argv idx]
    (define argc : Index (vector-length argv))
    
    (cond [(<= argc idx) (cmdopt-error pname "insufficient arguments for <~a>" argu)]
          [else (values (vector-ref argv idx) (+ idx 1))])))

(define cmdarg...-ref : (-> Any Symbol (Vectorof String) Nonnegative-Fixnum (Values (Listof String) Nonnegative-Fixnum))
  (lambda [pname argu argv idx]
    (define argc-1 : Fixnum (- (vector-length argv) 1))
    (define optc : Integer (- argc-1 idx))

    (cond [(or (<= argc-1 0) (<= optc 0)) (cmdopt-error pname "insufficient arguments for <~a>" argu)]
          [else (values (cmdopt-subvector argv idx optc) argc-1)])))

(define cmdargs*-ref : (-> Any Symbol (Vectorof String) Integer (Values (Listof String) Integer))
  (lambda [pname argu argv idx]
    (define argc : Index (vector-length argv))
    (define optc : Integer (- argc idx))

    (cond [(< optc 0) (cmdopt-error pname "insufficient arguments for <~a>" argu)]
          [else (values (cmdopt-subvector argv idx optc) argc)])))

(define cmdopt-subvector : (-> (Vectorof String) Integer Integer (Listof String))
  (lambda [argv idx argc]
    (build-list argc (λ [[i : Index]] (vector-ref argv (+ idx i))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define cmdopt-program-name : (-> Any Symbol)
  (lambda [program]
    (cond [(symbol? program) program]
          [(path? program) (cmdopt-path->symbol program)]
          [(list? program) (string->symbol (string-join (map ~a program)))]
          [else (string->symbol (~a program))])))

(define cmdopt-display-banner : (-> Output-Port (Listof Any) Void)
  (lambda [/dev/stdout banners0]
    (define banners : (Listof Any) (filter values banners0))
    
    (when (pair? banners)
      (for ([heading (in-list banners)])
        (displayln heading /dev/stdout))

      (newline /dev/stdout))))

(define cmdopt-display-args : (-> Output-Port (Listof Symbol) Void)
  (lambda [/dev/stdout args]
    (define opt-idx : Integer (if (and (pair? args) (eq? (last args) '...)) (- (length args) 2) -2))
    (define ...idx : Integer (+ opt-idx 1))

    (for ([arg (in-list args)]
          [idx (in-naturals)])
      (fprintf /dev/stdout (cond [(= idx opt-idx) " [~a"]
                                 [(= idx ...idx) " ~a]"]
                                 [else " ~a"])
               arg))
    
    (newline /dev/stdout)))

(define cmdopt-display-usage-help : (-> Output-Port (Listof Any) Void)
  (lambda [/dev/stdout descriptions0]
    (define descriptions : (Listof Any) (filter values descriptions0))
    
    (when (pair? descriptions)
      (for ([description (in-list descriptions)])
        (fprintf /dev/stdout "  ~a~n" description)))))

(define cmdopt-display-flags : (-> Output-Port (Pairof (U Char Symbol) (Listof (U Char Symbol))) (Listof String) Natural Natural Void)
  (lambda [/dev/stdout flags descriptions0 this-width max-width]
    (define-values (flag alias) (values (car flags) (cdr flags)))
    
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define cmdopt-error : (-> Any String Any * Nothing)
  (lambda [pany msgfmt . args]
    (apply raise-user-error (cmdopt-program-name pany) msgfmt args)))

(define cmdopt-path->symbol : (-> Path Symbol)
  (lambda [p]
    (define basename : (Option Path)
      (let ([file (file-name-from-path p)])
        (and file (path-replace-extension file #""))))
    
    (string->symbol (path->string (or basename p)))))
