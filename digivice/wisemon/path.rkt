#lang typed/racket/base

(provide (all-defined-out))

(require racket/list)
(require racket/path)
(require racket/format)
(require racket/string)
(require racket/sequence)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define find-digimon-files : (-> (-> Path Boolean) Path [#:search-compiled? Boolean] (Listof Path))
  (lambda [predicate start-path #:search-compiled? [search-compiled? #false]]
    (define px.exclude : PRegexp
      (let ([cmpls (remove-duplicates (map (λ [[dir : Path]] (~a (file-name-from-path dir))) (use-compiled-file-paths)))])
        (pregexp (if search-compiled? "(/|\\\\)\\.git$" (string-join #:before-first "(/|\\\\)(\\.git|" #:after-last ")$" cmpls "|")))))
    (cond [(not (directory-exists? start-path)) null]
          [else (filter predicate (sequence->list (in-directory start-path (λ [[dir : Path]] (not (regexp-match? px.exclude dir))))))])))

(define make-regexps-filter : (-> (Listof (U Regexp Byte-Regexp)) (-> Path Boolean))
  (lambda [regexps]
    (case (length regexps)
      [(0) (λ [[p : Path]] #false)]
      [(1) (let ([rx (car regexps)]) (λ [[p : Path]] (regexp-match? rx p)))]
      [else (λ [[p : Path]]
              (for/or ([rx (in-list regexps)])
                (regexp-match? rx p)))])))
