#lang racket/gui

(require ffi/unsafe)
(require (planet jaymccarthy/zeromq))


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

(define (start-worker-thread machine)
  (thread
   (lambda ()
     (let* ([endpoint (format "tcp://~a:55555" machine)]
            [ctxt (context 1)]
            [sock (socket ctxt 'SUB)]
            [topic (malloc _msg 'raw)]
            [delay 0.01])
       (set-socket-option! sock 'SUBSCRIBE #"")
       (socket-connect! sock endpoint)
       (set-cpointer-tag! topic 'msg)
       (let loop ()
         (let ([stop-request (thread-try-receive)])           
           (unless stop-request
             (msg-init! topic)
             (with-handlers ([(lambda (exn) #t) (lambda (exn) 'ignore)])
               (socket-recv-msg! sock topic 'NOBLOCK))
             (unless (zero? (msg-size topic))
               (displayln (msg-data topic))
               (msg-close! topic)
               (socket-recv! sock))
             (sleep delay)
             (loop))))
       (free topic)))))

(define (main)
  (let ([sites (read-sites-from-file "sites.txt")]
        [worker-thread null])
    
    (define (stop-worker-thread)
      (unless (or (null? worker-thread)
                  (not (thread-running? worker-thread)))
        (thread-send worker-thread 'stop)
        (thread-wait worker-thread)
        (kill-thread worker-thread)))
    
    (define frame
      (new (class frame%
             (super-new)
             (define/augment (on-close)
               (stop-worker-thread)))
           [label "Diagnostics"]))
                           
    (new combo-field%
         [parent frame]
         [label "Site:"]
         [choices (sort (map first sites) string-ci<?)]
         [callback (lambda (field event)
                     (let* ([site (send field get-value)]
                            [machines (rest (assoc site sites))]
                            [menu (send machines-combo get-menu)])
                       ;; clear text
                       (send machines-combo set-value "")
                       ;; remove existing machines
                       (for ([machine (send menu get-items)])
                         (send machine delete))
                       ;; add new machines
                       (for ([machine (sort machines string-ci<?)])                         
                         (send machines-combo append machine))))])
    
    (define machines-combo (new combo-field%
                                [parent frame]
                                [label "Machine:"]
                                [choices empty]
                                [callback (lambda (field event)
                                            (stop-worker-thread)
                                            (let ([machine (send field get-value)])
                                              (unless (zero? (string-length machine))
                                                (set! worker-thread (start-worker-thread machine)))))]))
    (define log (new text-field%
                     [label "Log:"]
                     [parent frame]
                     [style '(multiple)]))
    
    (send frame show #t)))
  