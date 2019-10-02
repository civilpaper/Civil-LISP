; cctl is stand for Convert Circle To Line
;        Design by  : Adesu <Ade Suharna>
;        Email      : mteybid@yuasabattery.co.id
;        Homepage   : http://www.yuasa-battery.co.id
;        Create     : 27 September 2006
;        Program no.: 0436/09/2006
;        Edit by    :

(defun c:cctl (/ cmt cnt len lst opt spt ss sse ssl ssn ssp)
  (setq ss (car (entsel "\nSelect a circle")))
  (setq opt (getreal "\nEnter number of segment <12>: "))
  (if
    (= opt nil)
    (setq opt 12)
    (setq opt (fix opt))                                    
    )                                                       ; repeat
  (command "_divide" ss opt "")
  (setq ssp (ssget "x" '((0 . "point"))))
  (setq ssl (sslength ssp))
  (setq cnt 0)
  (repeat
    ssl
    (setq ssn (ssname ssp cnt))
    (setq sse (entget ssn))
    (setq spt (cdr (assoc 10 sse)))
    (setq lst (append lst (list spt)))
    (command "_erase" ssn "")
    (setq cnt (1+ cnt))
    )                                                        ; repeat
  (command "_erase" ss "")
  (setq cmt 0)
  (repeat
    ssl
    (command "_line" (nth cmt lst)
	     (nth (1+ cmt) lst) "")
    (setq cmt (1+ cmt))
    )                                                       ; repeat
  (command "_line" (last lst)(car lst) "")
  (princ)
  )                                                         ; defun