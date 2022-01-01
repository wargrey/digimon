#lang racket/base

(define ENV (current-environment-variables))
(define /dev/stdout (current-output-port))

(for ([var (in-list (environment-variables-names ENV))])
  (fprintf /dev/stdout "~a~n~a~n" var (environment-variables-ref ENV var))
  (flush-output /dev/stdout))
