#lang racket/gui

(define frame-rate-hz 30)

(define delta-angle (* 5 (/ pi 180)))

(define ship-x 100)
(define ship-y 100)
(define ship-orientation-angle 0)
(define ship-movement-angle 0)
(define ship-speed 0)

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
  (when thrust?
    (let* ([delta-time (/ 1000.0 frame-rate-hz)]
           [movement-dx (* ship-speed (cos ship-movement-angle))]
           [movement-dy (* ship-speed (sin ship-movement-angle))]
           [thrust-dx (* 0.1 (cos ship-orientation-angle))]
           [thrust-dy (* 0.1 (sin ship-orientation-angle))]
           [dx (+ movement-dx thrust-dx)]
           [dy (+ movement-dy thrust-dy)])
      (set! ship-movement-angle (atan dy dx))
      (set! ship-speed (+ ship-speed
                          (sqrt (+ (* dx dx) (* dy dy)))))
      (display (format "ship-orientation: ~a rad\n" ship-orientation-angle))
      (display (format "dx=~a dy=~a\n" dx dy))
      (display (format "ship-movement ~a rad\n" ship-movement-angle))))
  
  (set! ship-x (+ ship-x (* ship-speed (cos ship-movement-angle))))
  (set! ship-y (+ ship-y (* ship-speed (sin ship-movement-angle))))
  
  (send canvas refresh-now))

(define (paint-scene canvas dc)
  (send canvas set-canvas-background (send the-color-database find-color "black"))
  (draw-ship ship-x ship-y ship-orientation-angle dc))

(define frame (new my-frame% [label "Lander"]))

(define canvas (new my-canvas%
                    [parent frame]
                    [paint-callback paint-scene]))
  
(send frame show #t)

