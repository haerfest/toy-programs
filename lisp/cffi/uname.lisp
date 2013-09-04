(ql:quickload "cffi")

(defpackage :cffi-demo
  (:use :common-lisp :cffi))

(in-package :cffi-demo)

(defcstruct uname
  (sysname  :char :count 256)
  (nodename :char :count 256)
  (release  :char :count 256)
  (version  :char :count 256)
  (machine  :char :count 256))

(defcfun ("uname" uname-c) :int
  (name :pointer))

(defun uname ()
  (with-foreign-object (uname '(:struct uname))
    (let ((rc (uname-c uname)))
      (values rc
              (when (zerop rc)
                (let ((plist (mem-ref uname '(:struct uname))))
                  (loop for (k v) on plist by #'cddr
                     collect (cons k (foreign-string-to-lisp v)))))))))

;; Example output:
;;
;; 0
;; ((MACHINE  . "i386")
;;  (VERSION  . "Darwin Kernel Version 11.4.2: Thu Aug 23 16:26:45 PDT 2012; root:xnu-1699.32.7~1/RELEASE_I386")
;;  (RELEASE  . "11.4.2")
;;  (NODENAME . "macbook.home")
;;  (SYSNAME  . "Darwin"))
