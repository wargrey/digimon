#lang scribble/acmart @natbib @nonacm @screen @timestamp @acmthm

@(require digimon/tamer)

@handbook-bibtex-load[@build-path{bibtex.bib}]

@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
@handbook-title[
 #:λtitle title
 #:tex-CJK? #true
 #:author @author[#:orcid "https://orcid.org/0009-0009-0375-2359"
                  #:affiliation (affiliation #:institution "Education" #:country "Earth") ; sigplan requires `country`
                  ]{WarGrey Gyoudmon Ju}
 ]{BibTex 测试}

@authorsaddresses{}

@abstract{This document is used to test @litchar{Scr}ib@litchar{bl}e as a flexible front-end
 for writing formal @texbook-prefab-name{tex} documents.

 顺便也帮忙测试“字数统计”功能，该功能由 digitama/tamer/stat.rkt 提供。}

@; https://dl.acm.org/ccs
@ccsdesc[#:number 500]{Applied computing~Document scripting languages}
@ccsdesc[#:number 500]{General and reference~Biographies}

@keywords{Scribble, @texbook-prefab-name{tex}}

@handbook-smart-table[]

@include-section{pdftex.scrbl}

@acks{
 This example is written with Racket@$cite[plt-tr1] and Scribble
 and translated into @texbook-prefab-name{tex}@$cite{knuth:1984} before generating.
}

@handbook-bonus-appendix[]
