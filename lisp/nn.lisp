;; Adapted from Artificial Intelligence A Modern Approach, ch. 18.7.4.
;;
;; Example:
;;
;; CL-USER> (train xor-samples xor-network :learning-rate 0.5 :epochs 100000)
;; 5.292373e-6
;; CL-USER> (infer xor-network #(0 0))
;; (0.010024827)
;; CL-USER> (infer xor-network #(0 1))
;; (0.99231297)
;; CL-USER> (infer xor-network #(1 0))
;; (0.992284)
;; CL-USER> (infer xor-network #(1 1))
;; (0.0032533524)

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
  (bias 1.0))

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
                                            :weights (make-array '(2 3)))
                                (make-layer :neurons (vector (make-neuron))
                                            :weights (make-array '(3 1))))))

(defparameter xor-samples '((#(0 0) #(0))
                            (#(0 1) #(1))
                            (#(1 0) #(1))
                            (#(1 1) #(0))))

(defun initialize-weights (network)
  (loop
     for index from 1 below (length (network-layers network))
     for layer = (svref (network-layers network) index)
     for weights = (layer-weights layer) do
       (loop for i below (array-dimension weights 0) do
            (loop for j below (array-dimension weights 1) do
                 (setf (aref weights i j) (- (random 1.0) 0.5))))))

(defun squared-error (network)
  (loop
     with output-layer = (svref (network-layers network)
                                (- (length (network-layers network)) 1))
     for j below (length (layer-neurons output-layer))
     for neuron-j = (svref (layer-neurons output-layer) j)
     for error = (neuron-error neuron-j)
     sum (/ (* error error) 2)))

(defun set-input (network input)
  (loop
     with input-layer = (svref (network-layers network) 0)
     for i below (length (layer-neurons input-layer))
     for neuron-i = (svref (layer-neurons input-layer) i) do
       (setf (neuron-output neuron-i) (svref input i))))

(defun forward-pass (network)
  (loop
     for index from 1 below (length (network-layers network))
     for curr-layer = (svref (network-layers network) index)
     for prev-layer = (svref (network-layers network) (- index 1)) do
       (loop
          for j below (length (layer-neurons curr-layer))
          for neuron-j = (svref (layer-neurons curr-layer) j) do
            (setf (neuron-input neuron-j)
                  (+ (layer-bias curr-layer)
                     (loop
                        for i below (length (layer-neurons prev-layer))
                        for neuron-i = (svref (layer-neurons prev-layer) i)
                        sum (* (aref (layer-weights curr-layer) i j)
                               (neuron-output neuron-i)))))
            (setf (neuron-output neuron-j)
                  (funcall (neuron-function neuron-j)
                           (neuron-input neuron-j))))))

(defun calculate-errors (network target)
  (loop
     with output-layer = (svref (network-layers network)
                                (- (length (network-layers network)) 1))
     for j below (length (layer-neurons output-layer))
     for neuron-j = (svref (layer-neurons output-layer) j) do
       (setf (neuron-error neuron-j)
             (- (svref target j) (neuron-output neuron-j)))
       (setf (neuron-delta neuron-j)
             (* (funcall (neuron-derivative neuron-j) (neuron-input neuron-j))
                (neuron-error neuron-j)))))

(defun back-propagation (network)
  (loop
     for index from (- (length (network-layers network)) 2) downto 1
     for curr-layer = (svref (network-layers network) index)
     for next-layer = (svref (network-layers network) (+ index 1)) do
       (loop
          for i below (length (layer-neurons curr-layer))
          for neuron-i = (svref (layer-neurons curr-layer) i) do
            (setf (neuron-delta neuron-i)
                  (* (funcall (neuron-derivative neuron-i)
                              (neuron-input neuron-i))
                     (loop
                        for j below (length (layer-neurons next-layer))
                        for neuron-j = (svref (layer-neurons next-layer) j)
                        sum (* (aref (layer-weights next-layer) i j)
                               (neuron-delta neuron-j))))))))


(defun update-weights (network learning-rate)
  (loop
     for index from 1 below (length (network-layers network))
     for prev-layer = (svref (network-layers network) (- index 1))
     for curr-layer = (svref (network-layers network) index)
     for weights = (layer-weights curr-layer) do
       (loop
          for i below (length (layer-neurons prev-layer))
          for neuron-i = (svref (layer-neurons prev-layer) i) do
            (loop
               for j below (length (layer-neurons curr-layer))
               for neuron-j = (svref (layer-neurons curr-layer) j) do
                 (incf (aref weights i j)
                       (* learning-rate
                          (neuron-output neuron-i)
                          (neuron-delta neuron-j)))))))

(defun train (samples network &key (learning-rate 0.001) (epochs 100))
  (initialize-weights network)
  (loop repeat epochs do
       (loop for (x y) in samples do
            (set-input network x)
            (forward-pass network)
            (calculate-errors network y)
            (back-propagation network)
            (update-weights network learning-rate)))
  (squared-error network))

(defun collect-output (network)
  (loop
     with output-layer = (svref (network-layers network)
                                (- (length (network-layers network)) 1))
     for j below (length (layer-neurons output-layer))
     for neuron-j = (svref (layer-neurons output-layer) j)
     collect (neuron-output neuron-j)))

(defun infer (network input)
  (set-input network input)
  (forward-pass network)
  (collect-output network))

