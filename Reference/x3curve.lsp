;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setvar "CMDECHO" 0)
(prompt "\nX3 : Cam duong cong bac 3")
(prompt "\n Nguyen Quoc Cuong - 8/1999")
(princ)

(defun  *error* (msg)
   (setvar "OSMODE" oslast)
   (princ msg)
   (princ) 
);defun

(defun deflt (str1 def)
   (strcat Str1 "<" (rtos def 2 3 ) ">: ")
);defun

(defun DtR (deg) ; converts degree to radian
   (/ (* deg pi) 180)
);defun

(defun RtD (rad) ; converts radian to degree
   (/ (* rad 180) pi)
);defun

(defun tg (rad) ; tangent of an angle in radian
   (/ (sin rad) (cos rad))
);defun



(defun c:X3( / pt pt1 pt2 T1 T2 L1 L2 Lmin B tt2 K0 p al al2 phi1 phi2 ang1 ang2 dist1
                 pt11 pt31 ptmid angmid x y x01 y01 x02 y02 SS SC oslast)
   (setq oslast (getvar "OSMODE"))
   (command "UCS" "W")
   (if (not *SC) (setq *SC 3.0))
   (setvar "OSMODE" 33)
   (setq pt0 (getpoint "\nDinh duong cong :_end,int of " ))
   (setvar "OSMODE" 512)
   (setq pt1  (getpoint "\nDuong tiep tuyen tai dinh duong cong:_nearest of "))
   (setvar "OSMODE" 33)
   (setq pt2 (getpoint "\nDiem cuoi duong cong :_end,int of " ))
   (setvar "OSMODE" 0)
   (command "UCS" "3p" pt0 pt1 pt2)
   (setq pt2 (trans pt2 0 1))
   (setq Lx (car pt2)
          Ly (cadr pt2)
   )
  (setq a (/ Ly (expt Lx 3.0)))
  (setq SC (getreal (deflt (strcat "\nNhap khoang cach le (S<=" (rtos Lx 2 2) ") :" ) *SC)))
  (if (not SC) (setq SC *SC) (setq *SC SC))   
 

 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ;Draw spiral curve of the first spiral
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq SS SC)
(setvar "OSMODE" 0)
 (command "PLINE")      
 (command "0,0")
      (While (< SS Lx)
       (progn
       (setq fpt (list SS (* a (expt ss 3.0)))
         SS (+ SS SC)
      )
   (command fpt)      
    );progn  
   );while
   (command pt2)
   (command "")      
   (command "UCS" "W")
 
   (prompt (strcat "Phuong trinh duong cong : y = " (rtos a 2 15) " * x^3"))
   (setvar "OSMODE" oslast)
   (setvar "CMDECHO" 1)
   (princ)
);defun X3
(princ)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


