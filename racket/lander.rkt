#lang racket/gui

(define frame-rate-hz 30)

(define delta-angle (* 5 (/ pi 180)))

(define ship-x 100)
(define ship-y 100)
(define ship-orientation-angle (- (/ pi 2)))
(define ship-movement-angle ship-orientation-angle)
(define ship-speed 0.5)

(define turn-left? #f)
(define turn-right? #f)
(define thrust? #f)

(define gravity -9.81)

(define (draw-ship x y angle dc)
  (define (to-origin p)
    (list (- (first p) 15)
          (- (second p) 10)))
  (define (from-origin p)
    (list (+ (first p) 15)
          (+ (second p) 10)))
  (define (translate p)
    (list (+ (first p) x)
          (+ (second p) y)))
  (define (rotate p)
    (let ([x (first p)]
          [y (second p)])
      (list (- (* (cos angle) x) (* (sin angle) y))
            (+ (* (sin angle) x) (* (cos angle) y)))))
  (define (transform p)
    (translate (from-origin (rotate (to-origin p)))))
  (let ([outline (new dc-path%)]
        [points '((30 10)
                  (25 5) (20 5) (15 0) (0 0) (5 5)
                  (5 15)
                  (0 20) (15 20) (20 15) (25 15))])
    (apply (lambda (x y)
             (send outline move-to x y))
           (transform (first points)))
    (for [(point (in-list (rest points)))]
      (apply (lambda (x y)
               (send outline line-to x y))
             (transform point)))
    (send outline close)
    (send dc set-pen "white" 1 'solid)
    (send dc set-brush "white" 'transparent)
    (send dc draw-path outline)))

(define my-frame%
  (class frame%
    (super-new)
    (field [timer (new timer% [notify-callback game-tick])])
    (define/override (on-activate active?)
      (if active?
          (send timer start (exact-floor (/ 1000.0 frame-rate-hz)))
          (send timer stop)))
    (define (on-close)
      (send timer stop))
    (augment on-close)))

(define my-canvas%
  (class canvas%
    (super-new)
    (define/override (on-event event)
      (when (send event button-down? 'left)
        (set! ship-x (send event get-x))
        (set! ship-y (send event get-y))
        (set! ship-orientation-angle 0)
        (set! ship-movement-angle 0)
        (set! ship-speed 0)))
    (define/override (on-char event)
      (let ([key (send event get-key-code)])
        (set! turn-left? (eq? key 'left))
        (set! turn-right? (eq? key 'right))
        (set! thrust? (or (eq? key 'shift)
                          (eq? key 'rshift)))))))

(define (game-tick)
  (when turn-left?
    (set! ship-orientation-angle (- ship-orientation-angle delta-angle)))
  (when turn-right?
    (set! ship-orientation-angle (+ ship-orientation-angle delta-angle)))

  (let* ([delta-time (/ 1000.0 frame-rate-hz)]
         [gravity-dx 0]
         [gravity-dy 0.02]
         [movement-dx (* ship-speed (cos ship-movement-angle))]
         [movement-dy (* ship-speed (sin ship-movement-angle))]
         [thrust-dx (if thrust? (* 0.25 (cos ship-orientation-angle)) 0)]
         [thrust-dy (if thrust? (* 0.25 (sin ship-orientation-angle)) 0)]
         [dx (+ movement-dx thrust-dx gravity-dx)]
         [dy (+ movement-dy thrust-dy gravity-dy)])
    (set! ship-movement-angle (atan dy dx))
    (set! ship-speed (sqrt (+ (* dx dx) (* dy dy)))))

  (set! ship-x (+ ship-x (* ship-speed (cos ship-movement-angle))))
  (set! ship-y (+ ship-y (* ship-speed (sin ship-movement-angle))))

  (send speed-message set-label (~r #:precision 3 ship-speed))
  (send direction-message set-label (~r #:precision 3 ship-movement-angle))
  
  (send canvas refresh-now))

(define (paint-scene canvas dc)
  (send canvas set-canvas-background (send the-color-database find-color "black"))
  (draw-ship ship-x ship-y ship-orientation-angle dc))

(define frame (new my-frame% [label "Lander"]))

(define speed-message (new message%
                           [parent frame]
                           [label "No speed"]))

(define direction-message (new message%
                               [parent frame]
                               [label "No direction"]))
                               

(define canvas (new my-canvas%
                    [parent frame]
                    [paint-callback paint-scene]))
  
(send frame show #t)

