;; Adapted from Artificial Intelligence A Modern Approach, ch. 18.7.4.
;;
;; Example:
;; 
;; CL-USER> (train xor-samples xor-network :learning-rate 0.5 :epochs 1000000)
;; 4.3219616e-6
;; CL-USER> (infer xor-network #(0 0))
;; (0.0021414205)
;; CL-USER> (infer xor-network #(0 1))
;; (0.99683446)
;; CL-USER> (infer xor-network #(1 0))
;; (0.99683267)
;; CL-USER> (infer xor-network #(1 1))
;; (0.0029395432)

(defun sigmoid (x)
  (/ 1.0 (+ 1.0 (exp (- x)))))

(defun sigmoid-deriv (x)
  (let ((y (sigmoid x)))
    (* y (- 1.0 y))))

(defstruct neuron
  input
  output
  delta
  error
  (function #'sigmoid)
  (derivative #'sigmoid-deriv))

(defstruct layer
  neurons
  weights
  bias)

(defstruct network
  layers)

(defparameter matt-network
  (make-network :layers (vector (make-layer :neurons (vector (make-neuron) (make-neuron)))
                                (make-layer :neurons (vector (make-neuron) (make-neuron))
                                            :weights (make-array '(2 2)
                                                                 :initial-contents '((0.15 0.25)
                                                                                     (0.20 0.30)))
                                            :bias 0.35)
                                (make-layer :neurons (vector (make-neuron) (make-neuron))
                                            :weights (make-array '(2 2)
                                                                 :initial-contents '((0.40 0.50)
                                                                                     (0.45 0.55)))
                                            :bias 0.60)))
  "https://mattmazur.com/2015/03/17/a-step-by-step-backpropagation-example/")

(defparameter matt-samples '((#(0.05 0.10) #(0.01 0.99))))

(defparameter xor-network
  (make-network :layers (vector (make-layer :neurons (vector (make-neuron) (make-neuron)))
                                (make-layer :neurons (vector (make-neuron) (make-neuron) (make-neuron))
                                            :weights (make-array '(2 3))
                                            :bias 1.0)
                                (make-layer :neurons (vector (make-neuron))
                                            :weights (make-array '(3 1))
                                            :bias 1.0))))

(defparameter xor-samples '((#(0 0) #(0))
                            (#(0 1) #(1))
                            (#(1 0) #(1))
                            (#(1 1) #(0))))

(defun initialize-weights (network)
  (dotimes (index (length (network-layers network)))
    (when (> index 0)
      (let* ((layer (svref (network-layers network) index))
             (weights (layer-weights layer)))
        (dotimes (i (array-dimension weights 0))
          (dotimes (j (array-dimension weights 1))
            (setf (aref weights i j) (- (random 1.0) 0.5))))))))

(defun squared-error (network)
  (let* ((output-layer (svref (network-layers network)
                              (- (length (network-layers network)) 1)))
         (n (length (layer-neurons output-layer)))
         (squared-error 0))
    (dotimes (j n)
      (let ((neuron-j (svref (layer-neurons output-layer) j)))
        (incf squared-error (* 0.5
                               (neuron-error neuron-j)
                               (neuron-error neuron-j)))))
    squared-error))

(defun set-input (network input)
  (let* ((input-layer (svref (network-layers network) 0)))
    (dotimes (i (length (layer-neurons input-layer)))
      (let ((neuron-i (svref (layer-neurons input-layer) i)))
        (setf (neuron-output neuron-i) (svref input i))))))

(defun forward-pass (network)
  (do ((index 1 (1+ index)))
      ((= index (length (network-layers network))))
    (let ((curr-layer (svref (network-layers network) index))
          (prev-layer (svref (network-layers network) (- index 1))))
      (dotimes (j (length (layer-neurons curr-layer)))
        (let ((neuron-j (svref (layer-neurons curr-layer) j)))
          (setf (neuron-input neuron-j) (layer-bias curr-layer))
          (dotimes (i (length (layer-neurons prev-layer)))
            (let ((neuron-i (svref (layer-neurons prev-layer) i)))
              (incf (neuron-input neuron-j)
                    (* (aref (layer-weights curr-layer) i j)
                       (neuron-output neuron-i)))))
          (setf (neuron-output neuron-j)
                (funcall (neuron-function neuron-j)
                         (neuron-input neuron-j))))))))

(defun calculate-errors (network target)
  (let ((output-layer (svref (network-layers network)
                              (- (length (network-layers network)) 1))))
    (dotimes (j (length (layer-neurons output-layer)))
      (let* ((neuron-j (svref (layer-neurons output-layer) j)))
        (setf (neuron-error neuron-j)
              (- (svref target j)
                 (neuron-output neuron-j)))
        (setf (neuron-delta neuron-j)
              (* (funcall (neuron-derivative neuron-j)
                          (neuron-input neuron-j))
                 (neuron-error neuron-j)))))))

(defun back-propagation (network)
  (do ((index (- (length (network-layers network)) 2) (1- index)))
      ((zerop index))
    (let ((curr-layer (svref (network-layers network) index))
          (next-layer (svref (network-layers network) (+ index 1))))
      (dotimes (i (length (layer-neurons curr-layer)))
        (let ((neuron-i (svref (layer-neurons curr-layer) i))
              (sum 0))
          (dotimes (j (length (layer-neurons next-layer)))
            (let ((neuron-j (svref (layer-neurons next-layer) j)))
              (incf sum (* (aref (layer-weights next-layer) i j)
                           (neuron-delta neuron-j)))))
          (setf (neuron-delta neuron-i)
                (* (funcall (neuron-derivative neuron-i)
                            (neuron-input neuron-i))
                   sum)))))))

(defun update-weights (network learning-rate)
  (do ((index 1 (1+ index)))
      ((= index (length (network-layers network))))
    (let* ((curr-layer (svref (network-layers network) index))
           (prev-layer (svref (network-layers network) (- index 1)))
           (weights (layer-weights curr-layer)))
      (dotimes (i (length (layer-neurons prev-layer)))                
        (let ((neuron-i (svref (layer-neurons prev-layer) i)))
          (dotimes (j (length (layer-neurons curr-layer)))
            (let ((neuron-j (svref (layer-neurons curr-layer) j)))
              (incf (aref weights i j)
                    (* learning-rate
                       (neuron-output neuron-i)
                       (neuron-delta neuron-j))))))))))

(defun train (samples network &key (learning-rate 0.001) (epochs 100) (callback nil) (init-weights t))
  (when init-weights
    (initialize-weights network))
  (dotimes (epoch epochs)
    (dolist (sample samples)
      (set-input network (first sample))
      (forward-pass network)
      (calculate-errors network (second sample))
      (back-propagation network)
      (update-weights network learning-rate))
    (when callback
      (funcall callback (+ epoch 1) (squared-error network))))
  (squared-error network))

(defun collect-output (network)
   (let ((output nil)
        (output-layer (svref (network-layers network)
                              (- (length (network-layers network)) 1))))
    (dotimes (j (length (layer-neurons output-layer)))
      (let ((neuron-j (svref (layer-neurons output-layer) j)))
        (push (neuron-output neuron-j) output)))
    (reverse output)))

(defun infer (network input)
  (set-input network input)
  (forward-pass network)
  (collect-output network))

