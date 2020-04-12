#lang typed/racket

(require digimon/plist)
(require digimon/location)

(define Info.plist (build-path (assert (path-only (#%file)) path?) "Info.plist"))
(define Temp.plist (build-path (find-system-path 'temp-dir) "Info.plist"))

(bplist-dissect Info.plist)

(define original : PList-Datum (read-plist Info.plist))

(write-plist original Temp.plist)

(define bplist : PList-Datum (read-plist Temp.plist))

(bplist-dissect Temp.plist)

(equal? original (read-plist Temp.plist))

Temp.plist
original
