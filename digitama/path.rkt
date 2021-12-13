#lang typed/racket/base

(provide (all-defined-out))

(require racket/path)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define native-rootdir : (-> Path-String Path)
  (lambda [src]
    (build-path (or (path-only src) (current-directory))
                (system-library-subpath #false))))

(define native-rootdir/compiled : (-> Path-String Path)
  (lambda [src]
    (build-path (or (path-only src) (current-directory))
                (car (use-compiled-file-paths)) "native"
                (system-library-subpath #false))))
