#!/usr/bin/env racket

#lang racket

(require net/url)

(let* ([url   "http://www.esvapi.org/v2/rest/dailyVerse?key=IP&output-format=plain-text&include-first-verse-numbers=false&include-verse-numbers=false&include-footnotes=false&include-short-copyright=false&include-passage-horizontal-lines=false&include-heading-horizontal-lines=false&include-headings=false&include-subheadings=false"]
       [verse (bytes->string/utf-8 (port->bytes (get-pure-port (string->url url))))])
  (display (string-append verse "\n")))