#lang racket/base

(provide (all-defined-out))
(provide (rename-out [tamer-image handbook-image]))

(require file/convertible)
(require racket/file)
(require racket/symbol)

(require scribble/core)
(require scribble/manual)

(require "backend.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define default-tex-requests '(pdf png))
(define default-web-requests '(svg png gif))

(define mime-maps
  (hasheq 'svg 'svg-bytes
          'pdf 'pdf-bytes
          'png 'png-bytes
          'gif 'gif-bytes))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define tamer-image
  (lambda [path #:scale [scale 1.0] #:style [style #false] #:requests [requests null] #:base-dir [base-dir #false]. pre-contents]
    (cond [(convertible? path)
           (make-traverse-element
            (λ [get set!]
              (define mimes
                (cond [(pair? requests) requests]
                      [(handbook-latex-renderer? get) default-tex-requests]
                      [else default-web-requests]))
              (let request ([mimes mimes])
                (if (pair? mimes)
                    (let ([path.img (make-image path (car mimes) base-dir)])
                      (cond [(not path.img) (request (cdr mimes))]
                            [else (apply image
                                         #:scale scale #:style style
                                         path.img pre-contents)]))
                    pre-contents))))]
          [(not (element? path))
           (apply image
                  #:scale scale #:style style
                  path pre-contents)]
          [else path])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define make-image
  (lambda [datum mime base-dir]
    (define raw (convert datum (hash-ref mime-maps mime (λ [] mime))))

    (and (bytes? raw)
         (let ([temp-dir (or base-dir (build-path (find-system-path 'temp-dir) "tamer-handbook"))])
           (make-directory* temp-dir)
           (call-with-output-file* #:exists 'truncate/replace
             (make-temporary-file #:base-dir temp-dir
                                  (string-append "image~a." (symbol->immutable-string mime)))
             (λ [/dev/imgout]
               (write-bytes raw /dev/imgout)
               (object-name /dev/imgout)))))))
