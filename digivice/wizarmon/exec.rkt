#lang typed/racket/base

(provide (all-defined-out))

(require "parameter.rkt")

(require "exec/spec.rkt")
(require "exec/scrbl.rkt")
(require "exec/dot.rkt")

(require "../../digitama/minimal/dtrace.rkt")

(require "../../digitama/git/lstree.rkt")
(require "../../digitama/git/langstat.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define shell-exec : (-> Path Any)
  (lambda [path]
    (dtrace-notice #:topic the-name "source: ~a" path)
    
    (define lang : (Option String)
      (or (wizarmon-lang)
          (let* ([gf (make-git-file path)]
                 [gl (git-files->langfiles (list gf) null git-default-subgroups)])
            (and (= (hash-count gl) 1)
                 (git-language-name (cdar (hash->list gl)))))))

    (when (or lang)
      (dtrace-notice #:topic the-name "language: ~a" lang))

    (case (and lang (string-downcase lang))
      [("c++") (shell-spec/flags path 'cpp)]
      [("cpp") (shell-spec/flags path 'cpp)]
      [("c") (shell-spec/flags path 'c)]
      [("lean") (shell-spec/flags path 'lean)]
      [("python") (shell-spec/flags path 'python)]
      [("scribble") (shell-typeset/flags path 'scribble)]
      [("tex") (shell-typeset path 'tex)]
      [("graphviz (dot)") (shell-dot path 'png #".png")]
      [else (wizarmon-errno 126)
            (raise (make-exn:fail:unsupported (if (not lang)
                                                  (format "exec: don't know how to run this file: ~a" path)
                                                  (format "exec: don't know how to run ~a file: ~a" lang path))
                                              (continuation-marks #false)))])))
