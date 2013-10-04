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
                (message (when (zmq:getsockopt s zmq:rcvmore)
                           (zmq:recv s msg)
                           (zmq:msg-data-as-is msg))))
            (funcall fn topic message)))))))

(defun zmq-count (endpoint)
  "Counts the number of messages received from a ZMQ endpoint."
  (let ((counts '()))
    (zmq-listen endpoint
                (lambda (topic message)
                  (declare (ignore message))

                  (if (member topic counts :key #'first)
                      (let ((item (assoc topic counts)))
                        (incf (rest item)))
                      (setf counts (sort (copy-list (acons topic 1 counts))
                                         #'string< :key #'first)))

                  (format t "~C~{~a~^ ~}" #\return counts)
                  (finish-output nil)))))

(defun zmq-list (endpoint)
  "Lists the messages received from a ZMQ endpoint."
  (let ((time nil))
    (zmq-listen endpoint
                (lambda (topic message)
                  (declare (ignore message))
                  (multiple-value-bind (s m h) (get-decoded-time)
                    (let ((now (format nil "~2,'0d:~2,'0d:~2,'0d" h m s)))
                      (if (equal now time)
                          (format t "         ")
                          (progn
                            (setf time now)
                            (format t "~a " time)))
                      (format t "~a~%" topic)))))))