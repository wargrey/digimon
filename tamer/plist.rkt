#lang typed/racket

(require digimon/plist)
(require digimon/location)

(define Info.plist (build-path (assert (path-only (#%file)) path?) "Info.plist"))
(define Temp.plist (open-output-bytes))

(bplist-dissect Info.plist)

(define origin : PList-Datum (read-plist Info.plist))

origin

(write-plist origin Temp.plist)

(define bplist (get-output-bytes Temp.plist))

(bplist-dissect bplist)
(equal? origin (read-plist bplist))
