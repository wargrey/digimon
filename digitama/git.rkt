#lang typed/racket/base

(provide (all-defined-out))

(require racket/string)

(require "../number.rkt")
(require "../date.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type Git-Numstat-Line (List Natural Natural String))
(define-type Git-Numstat (Pairof Natural (Listof Git-Numstat-Line)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define git:%at : String "--pretty=format:%at")

(define git-numstat-exec : (-> Boolean Path String * (Listof Git-Numstat))
  (lambda [day? git . args]
    (parameterize ([subprocess-group-enabled #true]
                   [current-custodian (make-custodian)]
                   [current-subprocess-custodian-mode 'kill])
      (begin0
        (let-values ([(git.exe /dev/gitin _do _de) (apply subprocess #false #false #false git args)])
          (let pretty-numstat ([timestamp : Any (number->string (current-seconds))]
                               [tats : (Listof Git-Numstat) null])
            (cond [(not (string? timestamp)) (reverse tats)]
                  [else (let numstat ([stats : (Listof Git-Numstat-Line) null])
                          (define +-path (read-line /dev/gitin))
                          (define tokens (if (eof-object? +-path) null (git-numstat-line-split +-path)))
                          (cond [(= (length tokens) 3)
                                 (let ([insertion (string->natural (car tokens))]
                                       [deletion (string->natural (cadr tokens))])
                                   (numstat (if (and insertion deletion) (cons (list insertion deletion (caddr tokens)) stats) stats)))]
                                [(null? tokens)
                                 (let ([ts (string->natural timestamp)])
                                   (pretty-numstat (read-line /dev/gitin)
                                                   (cond [(not ts) tats]
                                                         [(and day?) (cons ((inst cons Natural (Listof Git-Numstat-Line)) (floor-seconds ts s/day) (reverse stats)) tats)]
                                                         [else (cons ((inst cons Natural (Listof Git-Numstat-Line)) ts (reverse stats)) tats)])))]
                                [else ;;; another timestamp follows closely, rarely happen, is `git filter-branch` the root cause?
                                 (pretty-numstat +-path tats)]))])))
        (custodian-shutdown-all (current-custodian))))))

(define git-numstat-line-split : (-> String (Listof String))
  (lambda [line]
    ; Note: lines like `59 53 {src.ext => renamed.ext}` are only produced by `git diff`
    ; TODO: seriously deal with spaces in filename
    (define tokens : (Listof String) (string-split line))
    (cond [(<= (length tokens) 3) tokens]
          [(regexp-match? #px" => " line) (list (car tokens) (cadr tokens) (caddr tokens))] ; TODO: correct?
          [else (list (car tokens) (cadr tokens) (string-join (cddr tokens) " "))])))

