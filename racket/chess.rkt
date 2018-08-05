#lang racket

(require racket/draw)

(define (trim bitmap)
  (let ([dc (new bitmap-dc% [bitmap bitmap])]
        [width (send bitmap get-width)]
        [height (send bitmap get-height)]
        [color (make-object color%)])
    (define (transparent-pixel? x y)
      (and (send dc get-pixel x y color)
           (< (send color alpha) 0.5)))
    (define (transparent-row? y [x 0])
      (or (= x width)
          (and (transparent-pixel? x y)
               (transparent-row? y (+ x 1)))))
    (define (transparent-column? x [y 0])
      (or (= y height)
          (and (transparent-pixel? x y)
               (transparent-column? x (+ y 1)))))
    (define (trim-columns x->xx [x 0])
      (let ([xx (x->xx width x)])
        (if (or (= xx width)
                (< xx 0)
                (not (transparent-column? xx)))
            xx
            (trim-columns x->xx (+ x 1)))))
    (define (trim-left)
      (trim-columns (lambda (_ x) x)))
    (define (trim-right)
      (trim-columns (lambda (width x) (- width x 1))))
    (define (trim-rows y->yy [y 0])
      (let ([yy (y->yy height y)])
        (if (or (= yy height)
                (< yy 0)
                (not (transparent-row? yy)))
            yy
            (trim-rows y->yy (+ y 1)))))
    (define (trim-top)
      (trim-rows (lambda (_ y) y)))
    (define (trim-bottom)
      (trim-rows (lambda (height y) (exact-floor (- height y 1)))))
    (let* ([x0 (trim-left)]
           [x1 (trim-right)]
           [y0 (trim-top)]
           [y1 (trim-bottom)]
           [width (+ (- x1 x0) 1)]
           [height (+ (- y1 y0) 1)]
           [pixels (make-bytes (* width height 4))]
           [trimmed (make-bitmap width height)])
      (when (and (send bitmap get-argb-pixels x0 y0 width height pixels)
                 (send trimmed set-argb-pixels 0 0 width height pixels))
        trimmed))))

; https://pixabay.com/en/chess-pieces-set-symbols-game-26774/
; (read-pieces "chess-26774_640.png")

(define (read-pieces filename rows columns)
  (let* ([all-pieces (read-bitmap filename)]
         [width  (send all-pieces get-width)]
         [height (send all-pieces get-height)]
         [piece-width  (exact-ceiling (/ width  columns))]
         [piece-height (exact-ceiling (/ height rows))])
    (define (cutout-piece row column)
      ; The last piece may be a tad smaller due to exact-ceiling above.
      (let* ([this-piece-width  (min piece-width  (- width  (* column piece-width)))]
             [this-piece-height (min piece-height (- height (* row piece-height)))]
             [piece (make-bitmap this-piece-width this-piece-height)]
             [pixels (make-bytes (* this-piece-width this-piece-height 4))])
        (when (and (send all-pieces
                         get-argb-pixels
                         (* column piece-width)
                         (* row piece-height)
                         this-piece-width
                         this-piece-height pixels)
                   (send piece set-argb-pixels 0 0 this-piece-width this-piece-height pixels))
          (trim piece))))
    (define (cutout-pieces [row 0] [column 0] [pieces empty])
      (cond [(= row rows) pieces]
            [(= column columns) (cutout-pieces (+ row 1) 0 pieces)]
            [else (cutout-pieces row
                                 (+ column 1)
                                 (append pieces (list (cutout-piece row column))))]))
    (cutout-pieces)))
 