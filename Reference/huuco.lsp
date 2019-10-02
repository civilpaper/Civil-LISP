(defun C:dHC ()
   (if (= Ty_le nil)  (setq Ty_le (getreal "\nDrawing scale : ")))
   (setq he_so (/ 1000 Ty_le))
   (setq he_so2 (* he_so he_so))
   (setq lalast (getvar "CLAYER"))
  (setq oslast (getvar "OSMODE"))
  (setq dy (* he_so 0.5))
  (command "layer" "s" "4" "")
  (setvar "OSMODE" 33)
  (setq dthc 0)
  (setq pt1 (getpoint "\nDiem bat dau ben trai: "))
     (if (/= pt1 nil) (progn
  (setq pt2 (getpoint "To : "))
  (while (/= pt2 nil)
    (setq y1 (- (cadr pt1) dy) x1 (car pt1))
    (setq y2 (- (cadr pt2) dy) x2 (car pt2))
    (setq dtt (abs (* dy (- x2 x1))))
    (setq dthc (+ dthc dtt))
    (setvar "OSMODE" 0)
    (command "line" (list x1 y1) (list x2 y2) "")
    (setq pt1 pt2)
    (setvar "OSMODE" 33)
    (setq pt2 (getpoint "To : "))
  )
       ))
  (setq pt1 (getpoint "\nDiem bat dau ben phai: "))
      (if (/= pt1 nil) (progn
  (setq pt2 (getpoint "To : "))
  (while (/= pt2 nil)
    (setq y1 (- (cadr pt1) dy) x1 (car pt1))
    (setq y2 (- (cadr pt2) dy) x2 (car pt2))
    (setq dtp (abs (* dy (- x2 x1))))
    (setq dthc (+ dthc dtp))
    (setvar "OSMODE" 0)
    (command "line" (list x1 y1) (list x2 y2) "")
    (setq pt1 pt2)
    (setvar "OSMODE" 33)
    (setq pt2 (getpoint "To : "))
  )
       ))
  (setq dthc (/ (/ dthc he_so2) 1))
  (setq en (car (entsel "Thay so dao dat huu co: ")))
  (setq elst (entget en))
  (setq elst (subst (cons 1  (rtos dthc 2 2)) (assoc 1 elst) elst))
  (entmod elst)
  (setq dtatb 0.00)
  (setvar "CLAYER" lalast)
 ;;(command "mm" "")
)