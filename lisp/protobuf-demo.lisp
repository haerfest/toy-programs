(ql:quickload "zeromq")

(defun zmq-listen (endpoint)
  "Subscribe to a ZMQ endpoint and print a count of all topics received."
  (zmq:with-context (ctx 1)
    (zmq:with-socket (s ctx zmq:sub)
      (zmq:connect s endpoint)
      (zmq:setsockopt s zmq:subscribe "")
      
      (let ((msg (make-instance 'zmq:msg))
            (counts '()))
        (loop do
          (zmq:recv s msg)
          
          (let ((topic (intern (zmq:msg-data-as-string msg))))
            (unless (member topic counts :key #'first)
              (setf counts (acons topic 0 counts)))
            (let ((counter (rest (assoc topic counts))))
              (setf (rest (assoc topic counts)) (1+ counter)))
            (format t "~{~a~^ ~}~C" counts #\return)
            (finish-output nil))
          
          (when (zmq:getsockopt s zmq:rcvmore)
            (zmq:recv s msg)))))))