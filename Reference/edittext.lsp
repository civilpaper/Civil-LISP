;Tip1595:  MET.LSP  Edit Text  (c)2000, Jay Thomas

(defun C:MET (/ SS CNT NUM ENT ANS oldsnp)
  (command"undo""group")
  (setq oldsnp (getvar"snapmode"))
  (setvar"snapmode"0)
  (prompt "\n  Multi Edit Text, Attributes, or Dimensions. ")
  (if(>=(getvar"acadver")"13")
  (setq SS (ssget '((-4 . "<or")(0 . "ATTDEF")(0 . "DIMENSION")
                                (0 . "MTEXT")(0 . "TEXT")(0 . "TOLERANCE")
           (-4 . "<and")(0 . "INSERT")(66 . 1)(-4 . "and>")(-4 . "or>"))))
  (setq SS (ssget '((-4 . "<or")(0 . "ATTDEF")(0 . "TEXT")
           (-4 . "<and")(0 . "INSERT")(66 . 1)(-4 . "and>")(-4 . "or>")))))
  (if SS 
  (progn
  (setq NUM (sslength SS) CNT 0)
  (while(< CNT NUM)
    (princ(strcat"\r  Edit Text or Attribute: "(rtos(1+ CNT) 2 0)" of "(rtos NUM 2 0)": "))
    (setq ENT (ssname SS CNT))
    (setq ANS (cdr(assoc 0(entget ENT))))
  (cond
  ((or(eq ANS "ATTDEF")(eq ANS "DIMENSION")(eq ANS "MTEXT")
      (eq ANS "TEXT")(eq ANS "TOLERANCE"))
    (command "DDedit" ENT \))
  ((eq ANS "INSERT")
    (command "DDatte" ENT \))
    )
    (entupd ENT)
    (setq CNT (1+ CNT))
   )
  (princ"  Done... "))
  (princ"  Nothing selected. "))
  (setvar"snapmode"oldsnp)
  (command"undo""end")
  (princ)
) ;EOF

