(defun b (x)
  "Blinks the built-in LED."
  (pinmode 13 t)
  (digitalwrite 13 x)
  (delay 500)
  (b (not x)))
