(ql:quickload "zeromq")

(defun zmq-listen (endpoint)
  "Subscribe to a ZMQ endpoint and print a count of all topics received."
  (zmq:with-context (ctx 1)
    (zmq:with-socket (s ctx zmq:sub)
      (zmq:connect s endpoint)
      (zmq:setsockopt s zmq:subscribe "")
      
      (let ((msg (make-instance 'zmq:msg)))
        (loop for topic = (progn
                            (zmq:recv s msg)
                            (let ((topic (intern (zmq:msg-data-as-string msg))))
                              (when (zmq:getsockopt s zmq:rcvmore)
                                (zmq:recv s msg))
                              topic))
                               
              for counts = '()
              then (let ((counter (if (member topic counts :key #'first)
                                      (rest (assoc topic counts))
                                      0)))
                     (acons topic (1+ counter)
                            (remove topic counts :key #'first)))
              do
                (format t "~{~a~^ ~}~C" (sort counts #'string< :key #'first)
                        #\return)
                (finish-output nil))))))