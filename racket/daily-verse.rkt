(require net/url)

(define (get-daily-verse)
  (let* ([s (string-append "http://www.esvapi.org/v2/rest/dailyVerse"
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

(define (display-daily-verse)
  (display (string-append (get-daily-verse) "\n")))
