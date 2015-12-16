#lang racket

(provide dine main)

(define *dinner-table* '((plato . (red-fork blue-fork))
                         (confucius . (blue-fork green-fork))
                         (socrates . (green-fork yellow-fork))
                         (voltaire . (yellow-fork purple-fork))
                         (descartes . (purple-fork red-fork))))

(define *threads* '())

(define (main . args)
  "Run from command-line: $ racket -tm philosophers.rkt"
  (dine))

;; -----------------------------------------------------------------------------
;;  Dining.
;; -----------------------------------------------------------------------------

(define (dine)
  (random-seed (current-seconds))
  (create-logger)
  (create-forks)
  (create-philosophers)
  (read))  ; to allow ^C to interrupt

;; -----------------------------------------------------------------------------
;;  Philosophers.
;; -----------------------------------------------------------------------------

(define (philosopher-thread philosopher)
  (let-values (((choice _) (shuffle 'eat 'think)))
    (case choice
      ((eat) (try-eat philosopher))
      ((think) (think philosopher)))))

(define (try-eat philosopher)
  (case (acquire-forks philosopher)
    ((success) (eat philosopher))
    ((failure) (think philosopher))))

(define (eat philosopher)
  (log philosopher 'eating)
  (busy)
  (release-forks philosopher)
  (think philosopher))

(define (think philosopher)
  (log philosopher 'thinking)
  (busy)
  (try-eat philosopher))

(define (acquire-forks philosopher)
  (let-values (((first-fork second-fork) (apply shuffle (forks philosopher))))
    (case (acquire-fork philosopher first-fork)
      ((success) (case (acquire-fork philosopher second-fork)
                   ((success) 'success)
                   ((failure) (release-fork first-fork)
                              'failure)))
      ((failure) 'failure))))

(define (acquire-fork philosopher fork)
  (thread-send (thread-id fork) (list 'acquire (thread-id philosopher)))
  (thread-receive))

(define (release-forks philosopher)
  (let-values (((first-fork second-fork) (apply shuffle (forks philosopher))))
    (release-fork first-fork)
    (release-fork second-fork)))

(define (release-fork fork)
  (thread-send (thread-id fork) 'release))

;; -----------------------------------------------------------------------------
;;  Forks.
;; -----------------------------------------------------------------------------

(define (fork-thread fork)
  (fork-loop fork 'available))

(define (fork-loop fork state)
  (case state
    ((available) (match (thread-receive)
                   ((list 'acquire philosopher) (thread-send philosopher 'success)
                                                (fork-loop fork 'inuse))))
    ((inuse) (match (thread-receive)
               ((list 'acquire philosopher) (thread-send philosopher 'failure)
                                            (fork-loop fork 'inuse))
               ('release (fork-loop fork 'available))))))

;; -----------------------------------------------------------------------------
;;  Create a thread for each philosopher and each fork.
;; -----------------------------------------------------------------------------

(define (create-forks)
  (let ((forks (map cadr *dinner-table*)))
    (for ((fork forks))
      (let ((thread-id (thread (lambda () (fork-thread fork)))))
        (set! *threads* (cons (cons fork thread-id) *threads*))))))

(define (create-philosophers)
  (let ((philosophers (map car *dinner-table*)))
    (for ((philosopher philosophers))
      (let ((thread-id (thread (lambda () (philosopher-thread philosopher)))))
        (set! *threads* (cons (cons philosopher thread-id) *threads*))))))
  
;; -----------------------------------------------------------------------------
;;  Logger.
;; -----------------------------------------------------------------------------

(define (create-logger)
  (let ((thread-id (thread (lambda () (logger)))))
    (set! *threads* (cons (cons logger thread-id) *threads*))))

(define (logger)
  (displayln (thread-receive))
  (logger))

(define (log philosopher task)
  (let ((thread-id (cdr (assoc logger *threads*)))
        (message (format "~a is ~a..." philosopher task)))
    (thread-send thread-id message)))

;; -----------------------------------------------------------------------------
;;  Small helper functions.
;; -----------------------------------------------------------------------------

(define (forks philosopher)
  (cdr (assoc philosopher *dinner-table*)))

(define (shuffle a b)
  (case (random 2)
    ((0) (values a b))
    ((1) (values b a))))

(define (busy)
  (sleep (/ (+ 500 (random 4500)) 1000)))

(define (thread-id philosopher-or-fork)
  (cdr (assoc philosopher-or-fork *threads*)))
