(prompt "\n		CNO to +-*/ text entities with a number")
(defun c:CNO ( / No temp ed old new acc fac operator ssl )
  (Text_get)
  (setq operator (getstring "\nInput operator (+ - * /):"))
  (setq fac (getreal "\nInput a number:"))
  (setq acc (getint "\nAccuracy:"))
  (setq DZ (getvar "DIMZIN"))
  (setvar "DIMZIN" 0)
  (setq ssl (sslength #sset))

  (while (> ssl 0)
    (progn
      (setq temp (ssname #sset (setq ssl (1- ssl))))
      (setq ed (entget temp))
      (setq old (cons 1 (DXF 1 ed)))

      (setq No (atof (DXF 1 ed)))
      (cond
         ((= operator "+") (setq No (+ No fac)))
         ((= operator "-") (setq No (- No fac)))
         ((= operator "*") (setq No (* No fac)))
         ((= operator "/") (setq No (/ No fac)))
      )
	(setq sign (substr (DXF 1 ed) 1 1))
	(cond
           ((= sign "+")(setq new (cons 1 (strcat "+" (rtos No 2 acc)))))
    	   ((/= sign "+")(setq new (cons 1 (rtos No 2 acc))))
	)	
      (setq ed (subst new old ed))
      (entmod ed)  
    )
  )
(setvar "DIMZIN" DZ)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;