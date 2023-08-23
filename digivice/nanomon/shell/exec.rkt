#lang typed/racket/gui

(provide (all-defined-out))

(require racket/pretty)

(require "../shell.rkt")
(require "../parameter.rkt")

(require "../../wisemon/phony/cc.rkt")
(require (only-in "../../wisemon/parameter.rkt" current-make-real-targets))

(require "../../../dtrace.rkt")
(require "../../../wisemon.rkt")
(require "../../../digitama/collection.rkt")
(require "../../../digitama/git/lstree.rkt")
(require "../../../digitama/git/langstat.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define shell~~exec : (-> Path Thread Any)
  (lambda [path env-thread]
    (define lang : (Option String)
      (or (nanomon-lang)
          (let* ([gf (make-git-file path)]
                 [gl (git-files->langfiles (list gf) null #false)])
            (and (= (hash-count gl) 1)
                 (git-language-name (cdar (hash->list gl)))))))

    (define exitcode : Byte
      (case (and lang (string-downcase lang))
        [("racket") (shell-rkt path)]
        [("c++") (shell-cpp path 'cpp)]
        [("cpp") (shell-cpp path 'cpp)]
        [("c") (shell-c path 'c)]
        [else (nanomon-errno 126)
              (raise (make-exn:fail:unsupported (format "exec: don't know how to run this script: ~a" path)
                                                (continuation-marks #false)))]))
    
    (thread-send env-thread 0)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define shell-rkt : (-> Path Byte)
  (lambda [path.rkt]
    (parameterize ([current-namespace (make-gui-namespace)]
                   [global-port-print-handler pretty-print])
      (define main `(submod ,path.rkt main))
      
      (if (module-declared? main #true)
          (dynamic-require main 0)
          (dynamic-require path.rkt 0))

      0)))

(define shell-c : (-> Path Symbol Byte)
  (lambda [path.c lang-name]
    (define maybe-info : (Option Pkg-Info)
      (single-collection-info #:bootstrap? #true
                              (or (path-only path.c)
                                  (current-directory))))

    (parameterize ([current-make-real-targets (list path.c)])
      (define-values (cc-specs targets) (make-cc-spec+targets (and maybe-info (pkg-info-ref maybe-info)) #false lang-name))

      (if (and cc-specs (pair? targets))
          (let ([argv (vector->list (current-command-line-arguments))])
            (wisemon-make cc-specs targets #:name the-name #:keep-going? #false)
            (dtrace-info #:topic 'exec "~a ~a" (car targets) (string-join argv))
            (apply system*/exit-code (car targets) argv))
          127 #| deadcode, command cannot be found |#))))

(define shell-cpp : (-> Path Symbol Byte)
  (lambda [path.c lang-name]
    (shell-c path.c lang-name)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define exec-shell : Nanomon-Shell
  (nanomon-make-shell #:name 'exec #:shell shell~~exec
                      #:desc "Run source file (of Racket, C, and C++)"))
