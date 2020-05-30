#lang racket/base

(provide (all-defined-out))

(require racket/file)
(require racket/path)
(require racket/format)

(require make)

(require (only-in "parameter.rkt" make-dry-run make-just-touch make-always-run))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define unsafe-wisemon-make
  (lambda [rules [targets null]]
    (when (pair? rules)
      (define specs (map unsafe-hack-rule rules))
      (when (pair? specs)
        (make/proc specs (cond [(pair? targets) targets]
                               [else (map car rules)]))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define unsafe-hack-rule
  (lambda [r]
    (define t (car r))
    (define ds (cadr r))
    (define f
      (λ [] (let* ([t-already-exists? (file-exists? t)]
                   [tmp (make-temporary-file (~a (file-name-from-path t) ".~a") (and t-already-exists? t))])
              (dynamic-wind (λ [] (make-parent-directory* t))
                            (λ [] (void ((caddr r) t)))
                            (λ [] (when (make-dry-run)
                                    (cond [t-already-exists? (rename-file-or-directory tmp t #true)]
                                          [(file-exists? t) #| now exists |# (delete-file t)])))))))
    
    (list t (if (make-always-run) (cons (current-directory) ds) ds)
          (cond [(not (make-just-touch)) f]
                [else (λ [] (file-or-directory-modify-seconds t (current-seconds) f))]))))
