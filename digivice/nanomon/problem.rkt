#lang typed/racket/base

(provide (all-defined-out))

(require racket/list)

(require "parameter.rkt")

(require "../../filesystem.rkt")
(require "../../token.rkt")
(require "../../string.rkt")
(require "../../dtrace.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(struct problem-spec
  ([brief : String]
   [input : String]
   [output : (U String Regexp False)])
  #:type-name Problem-Spec
  #:constructor-name make-problem-spec
  #:transparent)

(struct problem-info
  ([title : (Option String)]
   [description : (Listof String)]
   [arguments : (Listof String)]
   [result : (Option String)]
   [specs : (Listof Problem-Spec)])
  #:constructor-name make-problem-info
  #:type-name Problem-Info
  #:transparent)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define read-cpp-problem-info : (-> Path (Option Problem-Info))
  (lambda [main.cpp]
    (call-with-input-file* main.cpp
      (λ [[/dev/stdin : Input-Port]]
        (let try-next-comment-block : (Option Problem-Info) ()
          (syn-token-skip-whitespace /dev/stdin)
          
          (cond [(regexp-try-match #px"^[/][*][*]\\s*" /dev/stdin)
                 (let*-values ([(head continue?) (read-cpp-problem-title /dev/stdin)]
                               [(body) (if (not continue?) null (read-cpp-problem-description /dev/stdin))]
                               [(args result rest) (problem-description-split-input-output body)]
                               [(specs description) (problem-description-split-spec main.cpp rest)])
                   (if (pair? specs)
                       (make-problem-info head description args result specs)
                       (try-next-comment-block)))]
                [(regexp-try-match #px"^[/][*]" /dev/stdin)
                 (regexp-match #px".+?[*][/]" /dev/stdin)
                 (try-next-comment-block)]
                [(regexp-try-match #px"^[/][/]" /dev/stdin)
                 (read-line /dev/stdin)
                 (try-next-comment-block)]
                [else #false]))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define read-cpp-problem-title : (-> Input-Port (Values (Option String) Boolean))
  (lambda [/dev/stdin]
    (define self (read-line /dev/stdin))
      
    (if (or (eof-object? self)
            (regexp-match #px"[*]+[/].*" self))
        (values #false #false)
        (values (string-trim self) #true))))

(define read-cpp-problem-description : (-> Input-Port (Listof String))
  (lambda [/dev/stdin]
    (let read-desc ([senil : (Listof String) null])
      (define self (read-line /dev/stdin))
      
      (if (string? self)
          (if (regexp-match #px"[*]+[/].*" self)
              
              (let ([last (string-trim self #px"(^\\s*[*]?\\s*)|(\\s*[*]+[/].*$)")])
                (if (string-blank? last)
                    (reverse senil)
                    (reverse (cons last senil))))

              (read-desc (cons (string-trim self #px"(^\\s*[*]\\s)|(\\s*$)") senil)))
          (reverse senil)))))

(define problem-description-split-input-output : (-> (Listof String) (Values (Listof String) (Option String) (Listof String)))
  (lambda [lines]
    (let split ([src : (Listof String) lines]
                [sgra : (Listof String) null]
                [result : String ""]
                [rest : (Listof String) null])
      (if (pair? src)
          (let-values ([(self tail) (values (car src) (cdr src))])
            (cond [(regexp-match #px"^(@|\\\\)(arg|param)" self) (split tail (cons (string-trim self #px"(^.\\w+\\s*)|(\\s*$)") sgra) result rest)]
                  [(regexp-match #px"^(@|\\\\)(returns?|result)" self) (split tail sgra (string-trim self #px"(^.\\w+\\s*)|(\\s*$)") rest)]
                  [else (split tail sgra result (cons self rest))]))
          (values (reverse sgra)
                  (if (string-blank? result) #false result)
                  (reverse rest))))))

(define problem-description-split-spec : (-> Path (Listof String) (Values (Listof Problem-Spec) (Listof String)))
  (lambda [main.cpp lines]
    (let split ([ids : (Listof Index) (indexes-where lines problem-spec-predicate)]
                [src : (Listof String) lines]
                [ss : (Listof Problem-Spec) null]
                [body : (Option (Listof String)) #false]
                [offset : Index 0])
      (define idx : Nonnegative-Fixnum (add1 (length ss)))
      
      (if (pair? ids)
          (let*-values ([(pos ids--) (values (car ids) (cdr ids))]
                        [(head tail) (split-at src (- pos offset))])
            (cond [(not body) (split ids-- tail ss head pos)]
                  [(null? head) (split ids-- tail ss head pos)]
                  [else (split ids-- tail (cons (problem-description->spec main.cpp (car head) (cdr head) idx) ss) body pos)]))
          (cond [(not body) (values null (string-list-normalize-empties src))]
                [(null? src) (values (reverse ss) (string-list-normalize-empties body))]
                [else (values (reverse (cons (problem-description->spec main.cpp (car src) (cdr src) idx) ss))
                              (string-list-normalize-empties body))])))))

(define problem-description->spec : (-> Path String (Listof String) Nonnegative-Fixnum Problem-Spec)
  (lambda [main.cpp head lines idx]
    (define brief : String (problem-spec-description head idx))

    (dtrace-debug #:topic the-name "parsing ~a" brief)
    
    (let parse ([src : (Listof String) lines]
                [is : (Listof String) null]
                [os : (Option (Listof String)) #false]
                [ydob : (Listof String) null]
                [dir : (Option Symbol) #false])
      (cond [(pair? src)
             (let-values ([(self rest) (values (car src) (cdr src))])
               (cond [(string-prefix? self "input") (parse rest is os ydob 'input)]
                     [(string-prefix? self "output") (parse rest is os ydob 'output)]
                     [(regexp-match? #px"^(@|\\\\)(file|include)\\s+" self) (parse (problem-spec-include main.cpp self rest) is os ydob dir)]
                     [(eq? dir 'input) (parse rest (cons self is) os ydob dir)]
                     [(eq? dir 'output) (parse rest is (cons self (or os null)) ydob dir)]
                     [else (parse rest is os (cons self ydob) dir)]))]

            [(not dir) ; no #:input ior #:output
             (let-values ([(is os) (splitf-at (string-list-trim-empties (reverse ydob)) string!blank?)])
               (make-problem-spec brief (problem-spec-join is)
                                  (let ([output (problem-spec-join os)])
                                    (if (string-blank? output) #false output))))]

            [else
             (make-problem-spec brief
                                (problem-spec-join (reverse is))
                                (and os (problem-spec-join (reverse os))))]))))

(define problem-spec-description : (-> String Nonnegative-Fixnum String)
  (lambda [headline idx]
    (define maybe-brief (substring headline 5))

    (if (string-blank? maybe-brief)
        (format "case ~a" idx)
        (format "case ~a: ~a" idx (string-trim maybe-brief)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define problem-spec-include : (-> Path String (Listof String) (Listof String))
  (lambda [src line rest]
    (define file : String (string-trim line #px"(^\\S+\\s*)|(\\s*$)"))
    (define path : Path (build-path (assert (path-only src)) file))

    (if (file-exists? path)
        (append (file->lines path) rest)
        (let ()
          (dtrace-warning #:topic the-name "ignored `~a` due to not found" file)
          rest))))

(define problem-spec-join : (-> (Listof String) String)
  (lambda [lines]
    (string-join (string-list-normalize-empties lines) "\n")))

(define problem-spec-predicate : (-> String Any)
  (lambda [self]
    (regexp-match #px"^(@|\\\\)(test)" self)))
