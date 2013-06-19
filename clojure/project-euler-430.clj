(defn e [n m]
  (letfn [(turn-probability [disk]
            (/ (dec (*' 2 disk (inc (- n disk)))) n n))

          (expected-value [disk]
            (let [p (turn-probability disk)]
              (loop [i     m
                     w     1.0]
                (if (zero? i)
                  w
                  (recur (dec i) (+ (* w (- 1 p))
                                    (* (- 1 w) p)))))))]

    (let [middle-disk (quot n 2)]
      (loop [disk middle-disk
             sum  0]
        (if (zero? disk)
          (+ (* 2 sum) (if (odd? n)
                         (expected-value (inc middle-disk))
                         0))
          (recur (dec disk) (+ sum (expected-value disk))))))))

; ----------------------------------------------------------------------
; inspiration (not final solution) to this solution from:
; 
;  http://eulersolutions.fr.yuku.com/topic/237#.UcHDKBZUcy4
; 
; that person hints:
; 
; 1) the probability of a given disk at position x to be turned, is
; 
;           2 * x * (N - x + 1)
;    P(x) = -------------------
;                 N * N
;   
; 2) the probability of a piece being white is then:
; 
;    P( w(t+1, x) )= P( w(t) ) * (1 - P(x)) + (1 - P( w(t) )) * P(x)
; 
; right... let's start with the first one
; 
; 1) what is the probability that disk x gets turned?
; 
;   it gets turned whenever it falls in [A,B] or [B,A]
;   =
;   P( (A <= x ^ x <= B) v (B <= x ^ x <= A) )
;   =
;   P( A <= x ^ x <= B ) + P( B <= x ^ x <= A )
;   =
;   P( A <= x ) * P( x <= B) + P( B <= x ) * P( x <= A )
;   =
;   note that both cases are symmetrical:
;   2 * P( A <= x ) * P( x <= B )
;   =
;   note if x = N, then P( A <= 1 ) = 1 = N/N = x/N
;                   and P( B >= 1 ) = P( B = 1 ) = 1/N = (N - x + 1)/N
;   and     x = 1, then P( A <= 1 ) = P( A = 1 ) = 1/N = x/N
;                   and P( B >= 1 ) = 1 = N/N = (N - x + 1)/N
;   so P( A <= x ) = x/N
;      P( x <= B ) = (N - x + 1)/N and we get to:
;   2 * x/N * (N - x + 1)/N
;   =
;   2 * x * (N - x + 1) / N^2
; 
; QED
; 
; 2) so the probability that disk x is white at turn t+1 is:
; 
;    P( w(t+1) )
;    =
;    the chance that it was white and does NOT swap
;    +
;    the chance that it was black and DOES swap
;    =
;    P( w(t) ) * (1 - P(x))
;    +
;    (1 - P( w(t) )) * P(x)
; 
; QED
; 
; let's try E(3, 1)
; 
; P(1) = 2 * 1 * (3 - 1 + 1) / 3^2 = 6/9
; P(2) = 2 * 2 * (3 - 2 + 1) / 3^2 = 8/9
; P(3) = 2 * 3 * (3 - 3 + 1) / 3^2 = 6/9
; 
; so the middle disk is more likely to turn, which is intuitively right
; 
; disk 1
; 
; P( w(t=0) ) = 1
; P( w(t=1) ) = P( w(t=0) ) * (1 - P(1)) + (1 - P( w(t=0) )) * P(1)
;             =      1      * (1 - 6/9)  +        0
;             =      3/9
; 
; disk 2
; 
; P( w(t=0) ) = 1
; P( w(t=1) ) = P( w(t=0) ) * (1 - P(2)) + (1 - P( w(t=0) )) * P(2)
;             =      1      * (1 - 8/9)  +        0
;             =      1/9
; 
; disk 3
; 
; P( w(t=0) ) = 1
; P( w(t=1) ) = P( w(t=0) ) * (1 - P(3)) + (1 - P( w(t=0) )) * P(3)
;             =      1      * (1 - 6/9)  +        0
;             =      3/9
; 
; so after one turn
; - disk 1 has a probability of 3/9 = 1/3 to be white
; - disk 2 has a probability of 1/9 to be white
; - disk 3 has a probability of 3/9 = 1/3 to be white
; 
; for a total probability of white disks of 7/9
; 
; but E(3, 1) = 10/9, so what gives?
; 
; trivial case E(1, 1)
; 
; P(1) = 2 * 1 * (1 - 1 + 1) / 1^2
;      = 2
; 
; the probability of the sole disk to be turned is... 2?
; 
; we have A=1 and B=1 as the sole possibility, so perhaps this is
; contrived
; 
; let's analyse E(3,1)
; 
; we can throw:   A   B   disk 1   disk 2   disk 3
;                 1   1    flip      -        -
;                 1   2    flip     flip      -
;                 1   3    flip     flip     flip
;                 2   1    flip     flip      -
;                 2   2     -       flip      -
;                 2   3     -       flip     flip
;                 3   1    flip     flip     flip
;                 3   2     -       flip     flip
;                 3   3     -        -       flip
;                          ---- +   ---- +   ---- +
;                            5        7        5
;                            -        -        -
;                            9        9        9
; 
; so the P(x) formula is not quite right
; 
; - disk x flips if A <= x <= B  or  B <= x <= A
;                if (A <= x ^ x <= B) v (B <= x ^ x <= A)
; - P(x) = P((A <= x ^ x <= B) v (B <= x ^ x <= A))
;        = P(A <= x ^ x <= B) + P(B <= x ^ x <= A)
;        = P((A < x v A = x) ^ (x < B v x = B)) +
;          P((B < x v B = x) ^ (x < A v x = A))
;        = P(A < x v A = x) * P(x < B v x = B) +
;          P(B < x v B = x) * P(x < A v x = A)
;        = (P(A < x) + P(A = x)) * (P(x < B) + P(x = B)) +
;          (P(B < x) + P(B = x)) * (P(x < A) + P(x = A))
;        = ((x - 1)/N + 1/N) * ((N - x)/N + 1/N) +
;          ((x - 1)/N + 1/N) * ((N - x)/N + 1/N))
;        = x/N * (N - x + 1)/N + x/N * (N - x + 1)/N
;        = 2 * x/N^2 * (N - x + 1)/N^2
; 
; does this not count P(x = A) and P(x = B) each twice?
; 
; - subtract P(A = x ^ B = x)?
;            P(A = x) * P(B = x)
;            1/N * 1/N
; 
; we end up with:
; 
;          2 * x * (N - x + 1) - 1
; - P(x) = -----------------------
;                 N ^ 2
; ----------------------------------------------------------------------
