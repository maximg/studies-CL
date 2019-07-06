
(ql:quickload :usocket)


(defun crlf (&optional (stream *standard-output*))
  (write-char #\return stream)
  (write-char #\linefeed stream)
  (values))

;;; to test: (create-client "localhost" 22)

(defun create-client (host port)
  (let* ((socket (usocket:socket-connect host port :element-type 'character))
         (stream (usocket:socket-stream socket)))
    (unwind-protect 
     (progn
       (format t "Connected to ~A:~D~%" host port)
       (format stream "GET /")
       (crlf stream)
       (crlf stream)
       (usocket:wait-for-input socket)
       (format t "~A~%" (read-line stream)))
      (usocket:socket-close socket))))
