#lang scribble/acmart @natbib @nonacm @screen @timestamp @acmthm

@(require digimon/tamer)

@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
@handbook-title[
 #:λtitle title
 #:tex-CJK? #true
 #:author @author[#:orcid "https://orcid.org/0009-0009-0375-2359"
                  #:affiliation (affiliation #:institution "Education" #:country "Earth") ; sigplan requires `country`
                  ]{WarGrey Gyoudmon Ju}
 #:tex-bib (build-path "bibtex.bib")
 ]{BibTex 测试}

@authorsaddresses{}

@abstract{This file is used to test Scribble as a flexible front-end for writing formal @texbook-prefab-name{tex} documents.}

@; https://dl.acm.org/ccs
@ccsdesc[#:number 500]{Applied computing~Document scripting languages}
@ccsdesc[#:number 500]{General and reference~Biographies}

@keywords{Scribble, @texbook-prefab-name{tex}}

@handbook-smart-table[]

@$cite[plt-tr1]
@$cite{knuth:1984}

@include-section{pdftex.scrbl}

@handbook-bibliography[#:numbered? #false]
