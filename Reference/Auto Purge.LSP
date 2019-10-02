(defun c:atp()
  (command "-purge" "a" "*" "n" "qsave")
  (princ)
  )