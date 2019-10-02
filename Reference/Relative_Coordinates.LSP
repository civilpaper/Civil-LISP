;;;RELATIVE COORDINATE === lay toa do tuong doi cua mot diem so voi diem truoc do

(defun C:RC ()
  (setvar "hpbound" 1)
  (setvar "cmdecho" 1)
  (setq pnt1 (getpoint "\nPick datum point: "))
;;;(setq ref1 (getpoint "\nEnter datum elevation of cross section:"))
;;; (setq ref1 (getreal "\nEnter datum elevation of cross section:")) ;_ it is a real

  (setq p1x (car pnt1))
  ;;x coord
  (setq p1y (cadr pnt1))
  ;;y coord
  (while
    (setq pnt2 (getpoint "\nPick coordinate point: "))
     (setq p2x (car pnt2))
     ;;x coord
     (setq p2y (cadr pnt2))
     ;;y coord
     ;;(setq dx (rtos (- (p1x p2x)) 2 2))
     ;; it shall be so
;;;(setq dx (rtos (- p1x p2x) 2 2)) dong comment nay cua file goc
(setq dx (rtos (- p2x p1x) 2 2))
    

     ;;(setq dy (rtos (+ ((- (p1y p2y)) ref1)) 2 2))
     ;; it shall be so

;;;     (setq dy (rtos (+ (- p1y p2y) ref1) 2 2))dong comment nay cua file goc
    
(setq dy (rtos (- p2y p1y) 2 2))



     ;;(setq STDZ (rtos P1z 2 2))
     (setq COORDN (strcat "Y " dy))
     (setq COORDE (strcat "X " dx))
					;;(setq COORDZ (strcat "Z " STDZ ))
     (setq PTXT (getpoint "\nPick text location: "))
     (command "LEADER" pnt2 PTXT "" COORDE COORDN "")
  )					;while
  (princ)
)