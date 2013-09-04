(defun sieve (upper-limit)
  (let ((numbers (make-array upper-limit :initial-element 'unmarked)))

    (labels
      ((sqr (x) (* x x))

       (mark-multiples-of (n)
         (loop for number from (sqr n) below upper-limit by n do
           (setf (elt numbers number) 'marked)))  ; number is not prime

       (find-next-number-to-mark (number)
         (labels ((iterate (candidate)
           (cond ((>= (sqr candidate) upper-limit) 0)  ; done with numbers
                 ((eq (elt numbers candidate) 'unmarked) candidate)
                 (t (iterate (1+ candidate))))))
                 (iterate number)))

       (sum-primes ()
         (loop for number from 2 below upper-limit
           when (eq (elt numbers number) 'unmarked)
           sum number)))

      (labels ((iterate (number)
        (if (zerop number)
          (sum-primes)
          (progn
            (mark-multiples-of number)
            (iterate (find-next-number-to-mark (1+ number)))))))
        (iterate 2)))))

(sieve 2000000)  ; => 142913828922
