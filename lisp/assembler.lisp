;; https://en.wikipedia.org/wiki/MOS_Technology_6502

(defconstant +opcode-matrix+
  (make-array '(16 16)
              :initial-contents
              ;; 0        1        2        3        4 5        6        7 8   9        A       B C        D        E        F
              '(((brk)    (ora ix) -        -        - (ora zp) (asl zp) - (php) (ora im) (asl a) - -        (ora ab) (asl ab) -)    ; 0
                ((bpl re) (ora iy) -        -        - (ora zx) (asl zx) - (clc) (ora ay) -       - -        (ora ax) (asl ax) -)    ; 1
                ((jsr ab) (and ix) -        - (bit zp) (and zp) (rol zp) - (plp) (and im) (rol a) - (bit ab) (and ab) (rol ab) -)    ; 2
                ((bmi re) (and iy) -        -        - (and zx) (rol zx) - (sec) (and ay) -       - -        (and ax) (rol ax) -)    ;  3
                ((rti)    (eor ix) -        -        - (eor zp) (lsr zp) - (pha) (eor im) (lsr a) - (jmp ab) (eor ab) (lsr ab) -)    ; 4
                ((bvc re) (eor ix) -        -        - (eor zx) (lsr zx) - (cli) (eor ay) -       - -        (eor ax) (lsr ax) -)    ; 5
                ((rts)    (adc ix) -        -        - (adc zp) (ror zp) - (pla) (adc im) (ror a) - (jmp in) (adc ab) (ror ab) -)    ; 6
                ((bvs re) (adc iy) -        -        - (adc zx) (ror zx) - (sei) (adc ay) -       - -        (adc ax) (ror ax) -)    ; 7
                (-        (sta ix) -        - (sty zp) (sta zp) (stx zp) - (dey) -        (txa)   - (sty ab) (sta ab) (stx ab) -)    ; 8
                ((bcc re) (sta iy) -        - (sty zx) (sta zx) (stx zy) - (tya) (sta ay) (txs)   - -        (sta ax) -        -)    ; 9
                ((ldy im) (lda ix) (ldx im) - (ldy zp) (lda zp) (ldx zp) - (tay) (lda im) (tax)   - (ldy ab) (lda ab) (ldx ab) -)    ; A
                ((bcs re) (lda iy) -        - (ldy zx) (lda zx) (ldx zy) - (clv) (lda ay) (tsx)   - (ldy ax) (lda ax) (ldx ay) -)    ; B
                ((cpy im) (cmp ix) -        - (cpy zp) (cmp zp) (dec zp) - (iny) (cmp im) (dex)   - (cpy ab) (cmp ab) (dec ab) -)    ; C
                ((bne re) (cmp iy) -        - -        (cmp zx) (dec zx) - (cld) (cmp ay) -       - -        (cmp ax) (dec ax) -)    ; D
                ((cpx im) (sbc ix) -        - (cpx zp) (sbc zp) (inc zp) - (inx) (sbc im) (nop)   - (cpx ab) (sbc ab) (inc ab) -)    ; E
                ((beq re) (sbc iy) -        - -        (sbc zx) (inc zx) - (sed) (sbc ay) -       - -        (sbc ax) (inc ax) -)))) ; F

(defun find-opcode (mnem &optional (addr-mode nil))
  "Returns the opcode of a MNEMonic and ADDR-MODE."
  (flet ((match? (entry)
           (and (consp entry)
                (eq (car entry) mnem)
                (or (eq addr-mode (cdr entry)) ; both NIL
                    (eq addr-mode (cadr entry))))))
    (do ((row 0 (1+ row))) ((= row 16))
      (do ((col 0 (1+ col))) ((= col 16))
        (when (match? (aref +opcode-matrix+ row col))
          (return-from find-opcode (+ (* row 16) col)))))))

(addr-mode '(brk))                      ; NIL
(addr-mode '(lda #!0))                   ; im
(addr-mode '(lda 0))                    ; zp
(addr-mode '(lda 1000))                 ; abs
(addr-mode '(lda 0 x))                  ; zpx
(addr-mode '(lda 1000 x))               ; ax
(addr-mode '(lda (0 x)))                ; ix
(addr-mode '(lda (0) y))                ; iy
(addr-mode '(jmp (1000)))               ; in
(addr-mode '(rol a))                    ; a
(addr-mode '(bpl ^3))                   ; rel

(set-dispatch-macro-character #\# #\!
                              #'(lambda (stream char1 char2)
                                  (declare (ignore char1 char2))
                                  (list 'quote (list 'im (read stream t nil t)))))

(set-dispatch-macro-character #\# #\@
                              #'(lambda (stream char1 char2)
                                  (declare (ignore char1 char2))
                                  (list 'quote (list 're (read stream t nil t)))))

(defun addr-mode (sexp)
  (cl-match:match sexp
    ((list mnem) nil)                              ; (brk)
    ((list mnem 'a) 'a)                            ; (rol a)
    ((list mnem (type (unsigned-byte 8))) 'zp)     ; (lda 0)
    ((list mnem (type (unsigned-byte 8)) 'x) 'zx)  ; (lda 0 x)
    ((list mnem (type (unsigned-byte 8)) 'y) 'zy)  ; (lda 0 y)
    ((list mnem (type (unsigned-byte 16))) 'ab)    ; (lda 1000)
    ((list mnem (type (unsigned-byte 16)) 'x) 'ax) ; (lda 1000 x)
    ((list mnem (type (unsigned-byte 16)) 'y) 'ay) ; (lda 1000 y)
    ((list mnem (list (type (unsigned-byte 8)) 'x)) 'zx) ; (lda (0 x))
    ((list mnem (list (type (unsigned-byte 8))) 'y) 'zy) ; (lda (0) y)
    ((list mnem (list (type (unsigned-byte 8)))) 'in)    ; (jump (0))
    ((list mnem (list quote
                      (list 'im (type (unsigned-byte 8))))) 'im) ; (lda #!0)

    ((list mnem (list quote
                      (list 're (type (signed-byte 8))))) 're) ; (beq #@-10)
))
                  
    

