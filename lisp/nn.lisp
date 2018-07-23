(defun sigmoid (x)
  (/ 1 (+ 1 (exp (- x)))))

(defstruct neuron
  name
  inputs
  weights
  (bias 1.0)
  (activation-function #'sigmoid)
  output
  initialized?)

(defun activate (neuron)
  (flet ((dot (v1 v2)
           (do ((i 0 (1+ i))
                (sum 0 (+ sum (* (nth i v1) (nth i v2)))))
               ((= i (length v1)) sum))))
    (setf (neuron-output neuron)
          (funcall (neuron-activation-function neuron)
                   (+ (dot (mapcar #'neuron-output (neuron-inputs neuron))
                           (neuron-weights neuron))
                      (neuron-bias neuron))))))

(defparameter xor-network
  (list (list (make-neuron :name 'x1)
              (make-neuron :name 'x2))
        (list (make-neuron :name 'h1 :inputs '(x1 x2))
              (make-neuron :name 'h2 :inputs '(x1 x2)))
        (list (make-neuron :name 'y1 :inputs '(h1 h2)))))

(defun forward-pass (network inputs)
  (let ((neurons (make-hash-table)))
    ;; build a hash so we can quickly find each neuron by name
    (dolist (layer network)
      (dolist (neuron layer)
        (setf (gethash (neuron-name neuron) neurons) neuron)))

    ;; assign the inputs to the outputs of all input neurons
    (let ((input-layer (car network)))
      (dotimes (i (length input-layer))
        (let ((neuron (nth i input-layer))
              (input  (nth i inputs)))
          (setf (neuron-output neuron) input))))

    ;; build the connections between neurons and set initial weights
    (dolist (layer (cdr network))
      (dolist (neuron layer)
        (unless (neuron-initialized? neuron)
          (setf (neuron-inputs neuron) (mapcar #'(lambda (name)
                                                   (gethash name neurons))
                                               (neuron-inputs neuron))
                (neuron-weights neuron) (mapcar #'(lambda (input)
                                                    (declare (ignore input))
                                                    (- (random 2.0) 1.0))
                                                (neuron-inputs neuron))
                (neuron-initialized? neuron) t))))

    ;; activate the neurons from the first hidden layer on
    (dolist (layer (cdr network))
      (dolist (neuron layer)
        (activate neuron)))

    ;; return the outputs of the output neurons
    (mapcar #'neuron-output (car (reverse network)))))
  
