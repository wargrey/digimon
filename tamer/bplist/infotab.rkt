#lang typed/racket

(require digimon/plist)
(require digimon/location)

(define /dev/bplout (open-output-bytes))

(plist-copy-info.rkt (build-path (assert (path-only (#%file)) path?) 'up 'up "info.rkt") /dev/bplout)

(define plist : PList-Object (read-plist (get-output-bytes /dev/bplout)))
