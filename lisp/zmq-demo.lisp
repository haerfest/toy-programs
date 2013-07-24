(ql:quickload "zeromq")

(defun zmq-listen (endpoint fn)
  "Subscribe to a ZMQ endpoint and call fn for every topic + message received."
  (zmq:with-context (ctx 1)
    (zmq:with-socket (s ctx zmq:sub)
      (zmq:connect s endpoint)
      (zmq:setsockopt s zmq:subscribe "")
      
      (let ((msg (make-instance 'zmq:msg)))
        (loop do
          (zmq:recv s msg)
          (let ((topic (intern (zmq:msg-data-as-string msg)))
                (message (if (zmq:getsockopt s zmq:rcvmore)
                             (progn
                               (zmq:recv s msg)
                               (zmq:msg-data-as-is msg))
                             nil)))
            (funcall fn topic message)))))))

(defun zmq-count (endpoint)
  "Counts the number of messages received from a ZMQ endpoint."
  (let ((counts '()))
    (zmq-listen endpoint
                #'(lambda (topic message)
                   (unless (member topic counts :key #'first)
                     (setf counts (sort (acons topic 0 counts)
                                        #'string< :key #'first)))
                   (let ((item (assoc topic counts)))
                     (setf (rest item) (1+ (rest item))))
                   (format t "~{~a~^ ~}~C" counts #\return)
                   (finish-output nil)))))