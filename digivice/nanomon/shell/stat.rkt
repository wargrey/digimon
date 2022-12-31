#lang typed/racket

(provide (all-defined-out))

(require "../shell.rkt")

(require "../../../format.rkt")

(require "../../../git.rkt")
(require "../../../echo.rkt")

(require "../../../digitama/git/langstat.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define shell~~stat : (-> Path Thread Any)
  (lambda [path env-thread]
    (define all-files : (Listof Git-File) (git-list-tree path #:recursive? #true))
    (define all-numstats : (Listof Git-Numstat) (git-numstat path #:recursive? #true))
    (define lang-files : (Immutable-HashTable Index (Git-Language-With (Listof Git-File))) (git-files->langfiles all-files null git-default-subgroups))
    (define lang-sizes : (Immutable-HashTable Index (Git-Language-With Natural)) (git-files->langsizes all-files null git-default-subgroups))
    (define lang-stats : (Immutable-HashTable Index (Git-Language-With (Listof Git-Numstat))) (git-numstats->langstats all-numstats null git-default-subgroups))

    (define total-size : Natural (apply + (map git-file-size all-files)))
    (define src-file : Natural (for/fold ([count : Natural 0]) ([lf (in-hash-values lang-files)]) (+ count (length (git-language-content lf)))))
    (define src-size : Natural (apply + (map (inst git-language-content Natural) (hash-values lang-sizes))))
    (define-values (additions deletions) (git-numstats->additions+deletions* all-numstats))

    (printf "~a in total, source: ~a ~a(~a) " (~size total-size) (~n_w src-file "file") (~size src-size) (~% (/ src-size total-size)))
    (echof "~a++ " (~integer additions) #:fgcolor 'green)
    (echof "~a--" (~integer deletions) #:fgcolor 'red)
    (newline)
    
    (echof "====================================================================================================" #:fgcolor 'darkgrey)
    (newline)

    (for ([lang (in-list ((inst sort (Git-Language-With Natural) Natural) (hash-values lang-sizes) >= #:key (inst git-language-content Natural)))])
      (define id (git-language-id lang))
      (define size (git-language-content lang))
      (define-values (adds dels)
        (cond [(not (hash-has-key? lang-stats id)) (values 0 0)]
              [else (git-numstats->additions+deletions* (git-language-content (hash-ref lang-stats id)))]))
      
      (printf "~a: ~a " (git-language-name lang) (~n_w (length (git-language-content (hash-ref lang-files id))) "file"))
      (echof "~a++ " (~integer adds) #:fgcolor 'green)
      (echof "~a-- " (~integer dels) #:fgcolor 'red)
      (echof "~a (~a) " (~% (/ size src-size)) (~size size) #:fgcolor 'yellow)
      (echof "~a " (~n_w (- adds dels) "line") #:fgcolor 'darkgrey)
      (newline))
    
    (thread-send env-thread 0)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define stat-shell : Nanomon-Shell
  (nanomon-make-shell #:name 'stat #:shell shell~~stat
                      #:desc "display various `git` statistics"))
