#!/usr/bin/env racket -tm

(module daily-verse racket
  (provide main)
  (require net/url)

  (define (get-daily-verse)
    (let ([s (string-append "http://www.esvapi.org/v2/rest/dailyVerse"
                            "?key=IP"
                            "&output-format=plain-text"
                            "&include-first-verse-numbers=false"
                            "&include-verse-numbers=false"
                            "&include-footnotes=false"
                            "&include-short-copyright=false"
                            "&include-passage-horizontal-lines=false"
                            "&include-heading-horizontal-lines=false"
                            "&include-headings=false"
                            "&include-subheadings=false")])
      (bytes->string/utf-8 (port->bytes (get-pure-port (string->url s))))))

  (define (main)
    (displayln (get-daily-verse))))
