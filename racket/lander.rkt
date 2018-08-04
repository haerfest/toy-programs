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

(define (draw-ship x y angle thrust? dc)
  (define (translate p)
    (cons (+ (car p) x)
          (+ (cdr p) y)))
  (define (rotate p)
    (let ([x (car p)]
          [y (cdr p)])
      (cons (- (* (cos angle) x) (* (sin angle) y))
            (+ (* (sin angle) x) (* (cos angle) y)))))
  (define (transform p)
    (translate (rotate p)))

  (send dc set-pen "white" 1 'solid)
  (send dc set-brush "white" 'transparent)

  ; The outline of the ship is specified in coordinates that make
  ; the ship point to an angle of zero radians (i.e. pointing right).
  ; It fits inside a 31x21 grid.
  (let ([outline (new dc-path%)]
        [points '((15 . 0)
                  (10 . -5) (5 . -5) (0 . -10) (-15 . -10) (-10 . -5)
                  (-10 . 5)
                  (-15 . 10) (0 . 10) (5 . 5) (10 . 5))])
    ; Move to the first point.
    (let ([p (transform (first points))])
      (send outline move-to (car p) (cdr p)))
    ; And draw the ship's outline as a path from there.
    (for [(point (in-list (rest points)))]
      (let [(p (transform point))]
        (send outline line-to (car p) (cdr p))))
    (send outline close)
    (send dc draw-path outline))

  ; Draw three horizontal lines when thrust is applied.
  (when thrust?
    (for [(y '(-4 0 4))]
      (let ([p1 (transform (cons -15 y))]
            [p2 (transform (cons -11 y))])
        (send dc draw-line (car p1) (cdr p1) (car p2) (cdr p2))))))

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
  (draw-ship ship-x ship-y ship-orientation-angle thrust? dc)
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

