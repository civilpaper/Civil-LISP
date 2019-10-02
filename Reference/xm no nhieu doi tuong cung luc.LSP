;;;;; No nhieu doi tuong duoc chon cung luc

(defun C:xm ()
  (setvar "qaflags" 1)
  (command "._explode" (ssget) "")
  (setvar "qaflags" 0)
  (princ)
)