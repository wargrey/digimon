#lang typed/racket/base

(provide (all-defined-out) s/day)

(require racket/string)
(require racket/match)

(require "../../number.rkt")
(require "../../date.rkt")

(require "../exec.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type Git-Numstat-Line (Immutable-Vector Natural Natural (U String (Pairof String String))))
(define-type Git-Numstat (Pairof Natural (Listof Git-Numstat-Line)))

(define-type Git-Date-Datum (U Integer String date))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define git-silents : (Listof Exec-Silent) (list 'stdout 'stderr))

(define git:%at : String "--pretty=format:%at")
(define px:rename:normal : Regexp #px"[{](.+) => (.+)[}]")
(define px:rename:sub : Regexp #px"/[{] => (.+)[}]")
(define px:rename:sup : Regexp #px"[{](.+) => [}]/")

(define current-git-procedure : (Parameterof Any) (make-parameter 'git-numstat))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define git-numstat-exec : (-> Path (Listof (Listof String)) (-> String (Listof Git-Numstat) (Listof Git-Numstat)) (Listof Git-Numstat) (Listof Git-Numstat))
  (lambda [git options numstat-fold initial]
    ((inst fg-recon-exec* (Listof Git-Numstat))
     #:silent git-silents (current-git-procedure)
     git options numstat-fold initial)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define git-numstat-make-fold : (-> (Option Natural) (-> String (Listof Git-Numstat) (Listof Git-Numstat)))
  (lambda [group-span:s]
    (define make-empty-numstat : (-> Natural Git-Numstat)
      (lambda [timestamp]
        (cons (cond [(not group-span:s) timestamp]
                    [else (floor-seconds timestamp group-span:s)])
              null)))
    
    (λ [line stats]
      (define-values (self rest)
        (cond [(pair? stats) (values (car stats) (cdr stats))]
              [else (values (make-empty-numstat (max (current-seconds) 0)) null)]))
      
      (let ([tokens (git-numstat-line-split line)])
        (cond [(list? tokens)
               (let ([insertion (string->natural (car tokens))]
                     [deletion (string->natural (cadr tokens))])
                 (cond [(and insertion deletion)
                        (let ([self++ ((inst cons Natural (Listof Git-Numstat-Line)) (car self) (cons (vector-immutable insertion deletion (caddr tokens)) (cdr self)))])
                          (cons self++ rest))]
                       [else (cons self rest)]))]
              [(string? tokens)
               ;;; another timestamp follows closely, rarely happens. or,
               ;;; `git log` always provides timestamps whereas `git diff` doesn't
               (let ([timestamp (string->natural tokens)])
                 (cond [(not timestamp) '#:deadcode (cons self rest)]
                       [(null? (cdr self)) (cons (make-empty-numstat timestamp) rest)]
                       [else (cons (make-empty-numstat timestamp) (cons self rest))]))]
              [(vector? tokens)
               (let ([insertion (string->natural (vector-ref tokens 0))]
                     [deletion (string->natural (vector-ref tokens 1))])
                 (cond [(and insertion deletion)
                        (let ([self++ ((inst cons Natural (Listof Git-Numstat-Line)) (car self) (cons (vector-immutable insertion deletion (vector-ref tokens 2)) (cdr self)))])
                          (cons self++ rest))]
                       [else (cons self rest)]))]
              [else (cons ((inst cons Natural (Listof Git-Numstat-Line)) (car self) (reverse (cdr self))) rest)])))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define git-numstat-line-split : (-> String (U False String (List String String String) (Vector String String (Pairof String String))))
  (lambda [line]
    ; TODO: seriously deal with spaces in filename
    (match (string-split line)
      [(list a d f) (list a d f)]
      [(list ts) ts]
      [(list a d f "=>" rest ...)
       (let ([raw (string-join (cons f (cons "=>" rest)))])
         (cond [(regexp-match? px:rename:normal raw) (vector a d (cons (regexp-replace px:rename:normal raw "\\1") (regexp-replace px:rename:normal raw "\\2")))]
               [(regexp-match? px:rename:sub raw) (vector a d (cons (regexp-replace px:rename:sub raw "") (regexp-replace px:rename:sub raw "/\\1")))]
               [(regexp-match? px:rename:sup raw) (vector a d (cons (regexp-replace px:rename:sup raw "\\1/") (regexp-replace px:rename:sup raw "")))]
               [else #false]))]
      [(list a d f ...) (list a d (string-join f " "))]
      [_ #false])))

(define git-numstat-date-option : (-> String Git-Date-Datum Boolean (Listof String))
  (lambda [opt d local?]
    (list (string-append "--" opt "="
                         (cond [(string? d) d]
                               [(date? d) (date->string d local?)]
                               [(> d 0) (date->string (seconds->date d local?) local?)]
                               [else (string-append "'" (number->string (- d)) " days ago'")])))))
