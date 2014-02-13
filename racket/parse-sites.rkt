#lang racket

(define (read-sites port)
  ;; reads a specification of sites and machines installed there, from a port
  (define (site? s)
    ;; a string designates a site if it ends with a colon ':'
    (eq? #\: (string-ref s (sub1 (string-length s)))))
  (define (iter sites site machines)
    ;; iterate over a file and collect an association list of sites
    ;; and machines installed there
    (let ([line (read-line port)])
      (if (eof-object? line)
          ;; done, add any outstanding machines
          (if (empty? machines)
              sites
              (cons (cons site machines) sites))
          (let* ([trimmed (string-trim line)]
                 [length  (string-length trimmed)])
            (cond
              ;; empty line
              [(zero? length) (iter sites site machines)]
              ;; site designator
              [(site? trimmed) (iter (if (null? site)
                                         sites
                                         (cons (cons site machines) sites))
                                     (substring trimmed 0 (sub1 length))
                                     empty)]
              ;; machine designator
              [else (iter sites site (cons trimmed machines))])))))
  (iter empty null empty))

(define (read-sites-from-file filename)
  ;; calls READ-SITES on the contents of a file
  (let* ([port  (open-input-file filename #:mode 'text)]
         [sites (read-sites port)])
    (close-input-port port)
    sites))
    
(define (test-read-sites)
  ;; tests the READ-SITES function, returning '(("there" "d" "c") ("here" "b" "a"))
  (let* ([port  (open-input-string "here:\na\nb\nthere:\nc\nd\n")]
         [sites (read-sites port)])
    (close-input-port port)
    sites))
    