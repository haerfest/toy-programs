#lang racket

(define grid-as-string
"08 02 22 97 38 15 00 40 00 75 04 05 07 78 52 12 50 77 91 08
49 49 99 40 17 81 18 57 60 87 17 40 98 43 69 48 04 56 62 00
81 49 31 73 55 79 14 29 93 71 40 67 53 88 30 03 49 13 36 65
52 70 95 23 04 60 11 42 69 24 68 56 01 32 56 71 37 02 36 91
22 31 16 71 51 67 63 89 41 92 36 54 22 40 40 28 66 33 13 80
24 47 32 60 99 03 45 02 44 75 33 53 78 36 84 20 35 17 12 50
32 98 81 28 64 23 67 10 26 38 40 67 59 54 70 66 18 38 64 70
67 26 20 68 02 62 12 20 95 63 94 39 63 08 40 91 66 49 94 21
24 55 58 05 66 73 99 26 97 17 78 78 96 83 14 88 34 89 63 72
21 36 23 09 75 00 76 44 20 45 35 14 00 61 33 97 34 31 33 95
78 17 53 28 22 75 31 67 15 94 03 80 04 62 16 14 09 53 56 92
16 39 05 42 96 35 31 47 55 58 88 24 00 17 54 24 36 29 85 57
86 56 00 48 35 71 89 07 05 44 44 37 44 60 21 58 51 54 17 58
19 80 81 68 05 94 47 69 28 73 92 13 86 52 17 77 04 89 55 40
04 52 08 83 97 35 99 16 07 97 57 32 16 26 26 79 33 27 98 66
88 36 68 87 57 62 20 72 03 46 33 67 46 55 12 32 63 93 53 69
04 42 16 73 38 25 39 11 24 94 72 18 08 46 29 32 40 62 76 36
20 69 36 41 72 30 23 88 34 62 99 69 82 67 59 85 74 04 36 16
20 73 35 29 78 31 90 01 74 31 49 71 48 86 81 16 23 57 05 54
01 70 54 71 83 51 54 69 16 92 33 48 61 43 52 01 89 19 67 48")

; Move a 4 x 4 window from left to right, then top to bottom:
;
; Starting at top-left:
;
;                24,468,444 (da)
;              /
;   08 02 22 97 --    34,144 (ha)
;   49 49 99 40 -- 9,507,960 (hb)
;   81 49 31 73 -- 8,981,847 (hc)
;   52 70 95 23 -- 7,953,400 (hd)
;   |  |  |  | \
;   |  |  |  |   279,496 (db)
;   |  |  |  6,514,520 (vd)
;   |  |  6,414,210 (vc)
;   |  336,140 (vb)
;   1,651,104 (va)
;
; Numbers are products, horizontal (ha..hd), vertical, and diagonal;
;
; Say we move right one number, then the first row in the window changes from:
;
; 08 02 22 97  to  02 22 97 38
;
; Its product can be easily changed from:
;
; (* 8 2 22 97)  to  (* 2 22 97 38)  via  (/ (* 8 2 22 97 38) 8)
;
; Which is:
;
; (/ (* prev-product new-digit) old-digit)
;
; And likewise for going down, and for going diagonal, but that can
; also be calculated on-the-fly when travelling horizontally and
; vertically.
;
; For every point in the resulting 17 x 17 matrix, we need to remember four values:
;
; 1.  Horizontal product (4 cells above).
; 2.  Vertical product (4 cells left).
; 3.  Diagonal product (top-left to bottom-right).
; 4.  Diagonal product (top-right to bottom-left).
;
; Then it's a matter of taking the maximum of all these values.

(define lines (string-split grid-as-string "\n"))
(define grid (map (λ (line)
                    (map (λ (number)
                           (string->number number))
                         (string-split line)))
                  lines))

(define row-count (length lines))
(define col-count (length (string-split (first lines))))

; build a row x col vector for quick access
(define grid+
  (build-vector row-count
                (λ (row)
                  (let [(vec (make-vector col-count))]
                    (for ([col (in-range col-count)])
                      (vector-set! vec col (list-ref (list-ref grid row) col)))
                    vec))))

(define (grid-ref grid row col)
  (vector-ref (vector-ref grid row) col))

(define computation-grid
  (build-vector row-count
                (λ (_) (make-vector col-count))))

#|
(define (group-by n items)
  (let loop ([groups '()]
             [remaining-items items])
    (if (> n (length remaining-items))
        (reverse groups)
        (loop (cons (take remaining-items n) groups) (rest remaining-items)))))

(define (max-4-seq-digit-product numbers)
  (define (product group)
    (foldl * 1 group))
  (let ([groups (group-by 4 numbers)])
    (apply max (map product groups))))

(define (grid-row row)
  (list-ref grid row))

(define (grid-column col)
  (foldl (λ (row result)
           (append result (list (list-ref row col))))
         '()
         grid))
|#
