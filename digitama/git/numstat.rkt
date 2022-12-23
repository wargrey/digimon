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
(define px:rename:normal : Regexp #px"[{]?(.+) => (.+)[}]?")
(define px:rename:sub : Regexp #px"/[{] => (.+)[}]")
(define px:rename:sup : Regexp #px"[{](.+) => [}]/")
(define px:submodule:status : Regexp #px"^\\s?\\S+\\s([^(]+)\\s[(][^)]+[)]$")
(define px:submodule:sync : Regexp #px"^[^']+'([^.]+)'$")

(define current-git-procedure : (Parameterof Any) (make-parameter 'git-numstat))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define git-numstat-exec : (-> Path (Listof (Listof String)) (-> String (Listof Git-Numstat) (Listof Git-Numstat)) (Listof Git-Numstat) (Listof Git-Numstat))
  (lambda [git options numstat-fold initial]
    ((inst fg-recon-exec* (Listof Git-Numstat))
     #:silent git-silents (current-git-procedure)
     git options numstat-fold initial)))

(define git-submodule-list : (-> Path (Listof String))
  (lambda [git]
    (reverse
     ((inst fg-recon-exec* (Listof String))
      #:silent git-silents (current-git-procedure)
      git (list (list "submodule" "status"))
      git-submodule-name-fold null))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define git-numstat-make-fold : (-> (Option Natural) (Option String) (-> String (Listof Git-Numstat) (Listof Git-Numstat)))
  (lambda [group-span:s subpath/]
    (define adjust-timestamp : (case-> [Natural -> Natural]
                                       [(Option Natural) -> (Option Natural)])
      (λ [timestamp]
        (and timestamp
             (cond [(not group-span:s) timestamp]
                   [else (floor-seconds timestamp group-span:s)]))))

    (define adjust-path : (-> (U String (Pairof String String)) (U String (Pairof String String)))
      (cond [(not subpath/) values]
            [else (λ [path] (cond [(string? path) (string-append subpath/ path)]
                                  [else (cons (string-append subpath/ (car path))
                                              (string-append subpath/ (cdr path)))]))]))

    (define make-empty-numstat : (-> Natural Git-Numstat)
      (λ [timestamp]
        (cons timestamp null)))

    (λ [line stats]
      (define-values (self rest)
        (cond [(pair? stats) (values (car stats) (cdr stats))]
              [else (values (make-empty-numstat (adjust-timestamp (max (current-seconds) 0))) null)]))
      
      (let ([tokens (git-numstat-line-split line)])
        (cond [(list? tokens) ; normal
               (let ([insertion (string->natural (car tokens))]
                     [deletion (string->natural (cadr tokens))])
                 (cond [(and insertion deletion)
                        (let ([num-line (vector-immutable insertion deletion (adjust-path (caddr tokens)))])
                          (cons ((inst cons Natural (Listof Git-Numstat-Line)) (car self) (cons num-line (cdr self))) rest))]
                       [else (cons self rest)]))]
              [(string? tokens)
               ;;; another timestamp follows closely, rarely happens. or,
               ;;; `git log` always provides timestamps whereas `git diff` doesn't
               (let ([timestamp (adjust-timestamp (string->natural tokens))])
                 (cond [(not timestamp) '#:deadcode (cons self rest)]
                       [(= timestamp (car self)) (cons self rest)]
                       [(null? (cdr self)) (cons (make-empty-numstat timestamp) rest)]
                       [else (cons (make-empty-numstat timestamp) (cons self rest))]))]
              [(vector? tokens) ; rename
               (let ([insertion (string->natural (vector-ref tokens 0))]
                     [deletion (string->natural (vector-ref tokens 1))])
                 (cond [(and insertion deletion)
                        (let ([rename-num-line (vector-immutable insertion deletion (adjust-path (vector-ref tokens 2)))])
                          (cons ((inst cons Natural (Listof Git-Numstat-Line)) (car self) (cons rename-num-line (cdr self))) rest))]
                       [else (cons self rest)]))]
              [else (cons ((inst cons Natural (Listof Git-Numstat-Line)) (car self) (reverse (cdr self))) rest)])))))

(define git-submodule-name-fold : (-> String (Listof String) (Listof String))
  (lambda [line names]
    (define maybe-name (regexp-match px:submodule:status line))
    
    (cond [(not maybe-name) names]
          [else (cons (assert (cadr maybe-name)) names)])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define git-numatat-merge : (-> (Listof Git-Numstat) (Listof Git-Numstat) Boolean (Listof Git-Numstat))
  (lambda [lstats rstats reverse?]
    (define lt? (if reverse? > <))
    (let merge ([lstats : (Listof Git-Numstat) lstats]
                [rstats : (Listof Git-Numstat) rstats]
                [stats : (Listof Git-Numstat) null])
      (cond [(null? lstats) (append (reverse stats) rstats)]
            [(null? rstats) (append (reverse stats) lstats)]
            [else (let*-values ([(lstat lrest) (values (car lstats) (cdr lstats))]
                                [(rstat rrest) (values (car rstats) (cdr rstats))]
                                [(lself rself) (values (car lstat) (car rstat))])
                    (cond [(lt? lself rself) (merge lstats rrest (cons rstat stats))]
                          [(not (= lself rself)) (merge lrest rstats (cons lstat stats))]
                          [else (let ([self ((inst cons Natural (Listof Git-Numstat-Line)) lself (append (cdr lstat) (cdr rstat)))])
                                  (merge lrest rrest (cons self stats)))]))]))))

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
