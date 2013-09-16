;;; Start Emacs and make sure Geiser is loaded.  Then hit C-c C-z to
;;; open a Racket REPL, followed by C-c C-b to evaluate this buffer.

(require racket/gui/base)

; Make a frame by instantiating the frame% class
(define frame (new frame% [label "Example"]))

; Make a static text message in the frame
(define msg (new message%
                 [parent frame]
                 [label "No events so far..."]))

; Make a button in the frame
(new button%
     [parent frame]
     [label "Click me"]
     [callback (lambda (button event)
                 (send msg set-label "Button click"))])

; Show the frame by calling its show method
(send frame show #t)
