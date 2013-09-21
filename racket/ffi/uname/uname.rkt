(require ffi/unsafe
         ffi/unsafe/define)

;;; From the man page we learn that uname is defined in libc.

(define-ffi-definer define-uname (ffi-lib "libc"))

;;; The C structure that uname expects:
;;; 
;;; struct utsname {
;;;   char sysname[256];
;;;   char nodename[256];
;;;   char release[256];
;;;   char version[256];
;;;   char machine[256];
;;; };
;;;
;;; Note that this not only defines the _utsname type, but also
;;; _utsname-pointer and various accessor functions (see demo below).

(define-cstruct _utsname ([sysname  (_array _byte 256)]
                          [nodename (_array _byte 256)]
                          [release  (_array _byte 256)]
                          [version  (_array _byte 256)]
                          [machine  (_array _byte 256)]))

;;; The prototype of the uname C function:
;;; 
;;; int uname(struct utsname *name)

(define-uname uname (_fun _utsname-pointer -> _int))

;;; Call the uname() C function and return the structure fields
;;; as a list of strings.

(define (demo)
  ;; Allocate memory for the structure.  It is garbage-collected so we
  ;; don't have to (free ptr) it.  We then cast this generic pointer
  ;; to a pointer to the structure, so that we can pass it on to uname
  ;; and access its fields.
  (let* ([ptr  (malloc _utsname)]
         [name (cast ptr _pointer _utsname-pointer)])
    ;; Invoke the C function.
    (uname name)
    ;; Return a list of the filled-in fields.
    (map (lambda (get-field-fn)
           ;; Each field is a null-terminated byte (char) array, which
           ;; we cast to a Racket string.
           (cast (array-ptr (get-field-fn name))
                 _pointer
                 _string))
         ;; These functions are passed on to the lambda above and
         ;; retrieve the structure's fields.
         (list utsname-sysname
               utsname-nodename
               utsname-release
               utsname-version
               utsname-machine))))

;;; Example output:
;;
;; '("Darwin"
;;   "macbook.home"
;;   "11.4.2"
;;   "Darwin Kernel Version 11.4.2: Thu Aug 23 16:26:45 PDT 2012; ..."
;;   "i386")
