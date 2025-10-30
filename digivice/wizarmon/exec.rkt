#lang typed/racket/base

(provide (all-defined-out))

(require digimon/digitama/minimal/dtrace)
(require digimon/digitama/git/lstree)
(require digimon/digitama/git/langstat)

(require "parameter.rkt")
(require "spec.rkt")
(require "typeset.rkt")

(require "exec/scrbl.rkt")
(require "exec/dot.rkt")

(require "../wisemon/display.rkt")
(require "../wisemon/parameter.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define shell-exec : (-> Path Any)
  (lambda [path]
    (dtrace-notice #:topic (the-cmd-name) "source: ~a" path)
    
    (define lang : (Option String)
      (or (wizarmon-lang)
          (let* ([gf (make-git-file path)]
                 [gl (git-files->langfiles (list gf) null git-default-subgroups)])
            (and (= (hash-count gl) 1)
                 (git-language-name (cdar (hash->list gl)))))))

    (when (or lang)
      (dtrace-notice #:topic (the-cmd-name) "language: ~a" lang))

    (parameterize ([make-verbose (wizarmon-verbose)]
                   [make-trace-log (wizarmon-debug)]
                   [current-make-phony-goal 'exec]
                   [current-make-real-targets (list path)])
      (case (and lang (string-downcase lang))
        [("c++") (shell-spec/flags path 'cpp)]
        [("cpp") (shell-spec/flags path 'cpp)]
        [("c") (shell-spec/flags path 'c)]
        [("lean") (shell-spec/flags path 'lean)]
        [("python") (shell-spec/flags path 'python)]
        [("scribble") (shell-typeset/flags path 'scribble shell-typeset)]
        [("tex") (shell-typeset path 'tex)]
        [("graphviz (dot)") (shell-dot path 'png #".png")]
        [else (wizarmon-errno 126)
              (raise (make-exn:fail:unsupported (if (not lang)
                                                    (format "exec: don't know how to run this file: ~a" path)
                                                    (format "exec: don't know how to run ~a file: ~a" lang path))
                                                (continuation-marks #false)))]))))
  