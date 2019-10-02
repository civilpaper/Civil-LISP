(defun DXF (code elist)
  (cdr (assoc code elist))
)

(defun deflt (str1 def)
   (strcat Str1 "<" (rtos def 2 3 ) ">: ")
);defun

(defun asin (rad)
 (atan (/ rad (sqrt (- 1 (expt rad 2)))))
; (atan (sqrt (- 1 (expt rad 2))) rad)
)

(defun acos (rad)
 (- (/ pi 2) (asin rad))
)

(defun CMDECH (mode)
  (if (= mode 1)
    (setvar "CMDECHO" 0)
    (setvar "CMDECHO" 1) 
  )
  (graphscr)
  (princ)
)

(defun C:DC(/ dist1 angl1 alfa DC ntl)
  (print)
  (print)
  (print)
 (cmdech 1)
 (setq last (getvar "OSMODE"))
; (command "setvar" "OSMODE" "4")
; (setq tl *scale)
  (if (= tl nil) (Setq tl (getreal "Nhap ty le ban ve: ")))
;  (setq tl *scale)
   (if (not  *R) (setq *R  300.0))
   (setq R  (getreal (deflt "\nNhap R : " *R)))
   (if (not R ) (setq R   *R) (setq *R   R))

  (setq PT1 (getpoint "Diem 1 : "))
  (setq PT2 (getpoint "Diem 2 : " pt1))

  (setq dist1 (distance pt1 pt2)
        angl1 (angtos (angle pt1 pt2) 1 4)
        deltaX (* (angle pt1 pt2) 1 4)

  )
  (setq ntl (/ 1000 tl))
  (setq dist (/ dist1 ntl)
        deltaX (rtos (* (cos (angle pt1 pt2)) dist) 2 4)
        deltaY (rtos (* (sin (angle pt1 pt2)) dist) 2 4)
  )
  (setq alfa (* 2 (asin (/ dist R 2)))
        DC (* alfa R)
   )
  (prompt (strcat "\nChieu dai thuc cua cung: " (rtos DC 2 4)))
  (command "setvar" "OSMODE" last)
 (princ)
)

(prompt "\nDC: Calculate length of ARC between 2 points")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun C:CC();/ dist R1 ntl)
  (print)
  (print)
  (print)
 (cmdech 1)
 (setq last (getvar "OSMODE"))
; (setq tl *scale)
  (if (= tl nil) (Setq (getreal "Nhap ty le ban ve: ")))
;  (setq tl *scale)
   (if (not  *R) (setq *R  300.0))
   (setq R  (getreal (deflt "\nNhap R : " *R)))
   (if (not R ) (setq R   *R) (setq *R   R))

  (setq PT1 (getpoint "Diem 1 : ")
        dist (getdist "Nhap chieu dai theo duong cong: ")
  )
  (setq R1 (* 2 R (sin (/ dist R 2)))
   )
 
  (setq ntl (/ 1000 tl))
  (setq R1 (/ R1 ntl))
  (prompt (strcat "\nChieu dai cung: " (rtos R1 2 4)))
  (setvar "OSMODE" 0)
  (command "circle" pt1 R1)
  (command "setvar" "OSMODE" last)
 (princ)
)

(prompt "\nCC: Divide the arc with a given curve length by the circle ")