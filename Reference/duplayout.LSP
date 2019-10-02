(defun c:duplayout (/ CTAB LAYOUT# LAYOUTNAME)
  (setvar "cmdecho" 0)
  (setq ctab (getvar "ctab"))
  (setq	layoutname
	 (getstring (strcat "\nLayout to duplicate <" ctab ">: "))
  )
  (if (= layoutname "")
    (setq layoutname ctab)
  )
  (initget 6)
  (setq layout# (getint "\nHow many copies ?<2>: "))
  (if (null layout#)
    (setq layout# 2)
  )
  (repeat layout#
    (command ".layout" "copy" layoutname "")
  )					;repeat
  (princ)
)					;defun