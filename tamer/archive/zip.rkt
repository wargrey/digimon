#lang scribble/lp2

@(require digimon/tamer)

@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
@handbook-story{PKZIP}

This module provides universal APIs for creating and extracting zip files,
and all APIs are implemented in pure Typed Racket.

@tamer-smart-summary[]

@handbook-scenario[]{}

Despite the fact that people invented so many concepts and methodologies in order to improve software quality. Amongst them,
@tech{BDD} is a communication technique and concentrates on bringing people involved in a software project closer.

@handbook-reference[]

@; Chunks after `handbook-reference[]` will never be rendered in documents
@; <*> is the main chunk by convention.

@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
@chunk[|<*>|
       (require digimon/tamer)
       (tamer-taming-start!)

       (module tamer typed/racket/base
         (require digimon/spec)
         
         (spec-begin bdd #:do))]
