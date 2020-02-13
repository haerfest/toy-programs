(defun paginate (&optional (page 5) (pages 10) (n 3))
  ;; (paginate  1) => (1 2 3 GAP 10 NEXT)
  ;; (paginate  3) => (PREV 1 2 3 GAP 10 NEXT)
  ;; (paginate  5) => (PREV 1 GAP 5 GAP 10 NEXT)
  ;; (paginate  8) => (PREV 1 GAP 8 9 10 NEXT)
  ;; (paginate 10) => (PREV 1 GAP 8 9 10)
  (let ((buttons (list)))
    (when (> page 1)
      (push 'prev buttons))
    (cond
      ((<= page n)                      ; page in first n
       (dotimes (i (min n pages))
         (push (1+ i) buttons))
       (when (> pages (1+ n))
         (push 'gap buttons))
       (when (> pages n)
         (push pages buttons)))
      ((> page (- pages n))             ; page in last n
       (let ((j (- pages n)))
         (when (> j 0)
           (push 1 buttons))
         (when (> j 1)
           (push 'gap buttons))
         (dotimes (i n) (push (+ j i 1) buttons))))
      (t (push 1 buttons)               ; page outside both
         (push 'gap buttons)
         (push page buttons)
         (push 'gap buttons)
         (push pages buttons)))
    (when (< page pages)
      (push 'next buttons))
    (nreverse buttons)))
     
    
    
  
