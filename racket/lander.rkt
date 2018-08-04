#lang racket/gui

(define frame-rate-hz 30)

(define min-rotation-angle (* 10 (/ pi 180)))

(define ship-x 100)
(define ship-y 100)

; Angles are measured in radians, with zero pointing east, then clockwise
; increasing. To make the ship point north, its angle is set to -pi / 2.
(define ship-orientation-angle (- (/ pi 2)))

; The ship's orientation and movement can be different, but initially the
; ship moves in the direction it is pointing.
(define ship-movement-angle ship-orientation-angle)

; Give the ship a bit of initial speed so as not to confront the player
; with a ship that is already dropping due to gravity.
(define ship-speed 0.5)

(define ship-trail-counter 0)
(define ship-trail-interval (exact-floor (/ frame-rate-hz 4)))
(define ship-trail-max-length 10)
(define ship-trail '())

(define turn-left? #f)   ; Will be #t when the user presses left.
(define turn-right? #f)  ; Will be #t when the user presses right.
(define thrust? #f)      ; Will be #t when the user presses shift.

(define (draw-ship x y angle dc)
  (let ([ship-height 30]
        [ship-width  20])
    (define (to-origin p)
      (list (- (first p)  (/ ship-height 2))
            (- (second p) (/ ship-width  2))))
    (define (from-origin p)
      (list (+ (first p)  (/ ship-height 2))
            (+ (second p) (/ ship-width  2))))
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

    ; The outline of the ship is specified in coordinates that make
    ; the ship point to an angle of zero radians (i.e. pointing right).
    ; It fits inside a 31x21 grid.
    (let ([outline (new dc-path%)]
          [points '((30 10)
                    (25 5) (20 5) (15 0) (0 0) (5 5)
                    (5 15)
                    (0 20) (15 20) (20 15) (25 15))])
      ; Move to the first point.
      (apply (lambda (x y)
               (send outline move-to x y))
             (transform (first points)))
      ; And draw the ship's outline as a path from there.
      (for [(point (in-list (rest points)))]
        (apply (lambda (x y)
                 (send outline line-to x y))
               (transform point)))
      (send outline close)
      (send dc set-pen "white" 1 'solid)
      (send dc set-brush "white" 'transparent)
      (send dc draw-path outline))))

(define (draw-trail trail dc)
  (send dc set-pen "white" 1 'solid)
  (send dc set-brush "white" 'transparent)
  (for [(p (in-list trail))]
    (send dc draw-point (car p) (cdr p))))
    
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
    (define/override (on-char event)
      (let ([key (send event get-key-code)])
        (set! turn-left? (eq? key 'left))
        (set! turn-right? (eq? key 'right))
        (set! thrust? (or (eq? key 'shift)
                          (eq? key 'rshift)))))))

(define (game-tick)
  ; Add the ship's old position to the trail when it's time.
  (set! ship-trail-counter (+ ship-trail-counter 1))
  (when (= ship-trail-counter ship-trail-interval)
    (set! ship-trail-counter 0)
    (set! ship-trail (append ship-trail (list (cons ship-x ship-y))))
    (when (> (length ship-trail) ship-trail-max-length)
      (set! ship-trail (rest ship-trail))))

  ; Calculate the ship's orientation angle.
  (when turn-left?
    (set! ship-orientation-angle (- ship-orientation-angle min-rotation-angle)))
  (when turn-right?
    (set! ship-orientation-angle (+ ship-orientation-angle min-rotation-angle)))

  ; Calculate the ship's movement angle and speed based on its orientation,
  ; thrust, gravity, and current movement.
  (let* ([gravity-dx 0]
         [gravity-dy 0.05]
         [movement-dx (* ship-speed (cos ship-movement-angle))]
         [movement-dy (* ship-speed (sin ship-movement-angle))]
         [thrust-dx (if thrust? (* 0.5 (cos ship-orientation-angle)) 0)]
         [thrust-dy (if thrust? (* 0.5 (sin ship-orientation-angle)) 0)]
         [dx (+ movement-dx thrust-dx gravity-dx)]
         [dy (+ movement-dy thrust-dy gravity-dy)])
    (set! ship-movement-angle (atan dy dx))
    (set! ship-speed (sqrt (+ (* dx dx) (* dy dy)))))
      
  ; Calculate the ship's new position.
  (set! ship-x (+ ship-x (* ship-speed (cos ship-movement-angle))))
  (set! ship-y (+ ship-y (* ship-speed (sin ship-movement-angle))))

  (send speed-message set-label (~r #:precision 3 ship-speed))
  (send direction-message set-label (~r #:precision 3 ship-movement-angle))
  
  (send canvas refresh-now))

(define (paint-scene canvas dc)
  (send canvas set-canvas-background (send the-color-database find-color "black"))
  (draw-ship ship-x ship-y ship-orientation-angle dc)
  (draw-trail ship-trail dc))

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

