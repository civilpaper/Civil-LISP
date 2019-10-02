;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setvar "CMDECHO" 0)
(prompt "\nX2 : Cam duong cong bac 2 cho cap")
(prompt "\n Nguyen Quoc Cuong - 9/1999")
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



(defun c:X2( / pt pt1 pt2 T1 T2 L1 L2 Lmin B tt2 K0 p al al2 phi1 phi2 ang1 ang2 dist1
                 pt11 pt31 ptmid angmid x y x01 y01 x02 y02 SS SC oslast)
   (setq oslast (getvar "OSMODE"))
   (command "UCS" "W")
   (if (not *SC) (setq *SC 3.0))
   (if (not *L0) (setq *L0 100))
   (setvar "OSMODE" 33)
   (setq pt0 (getpoint "\nDiem giua dam :_end,int of " ))
   (setvar "OSMODE" 512)
   (setq pt1  (getpoint "\nDuong tiep tuyen tai dinh duong cong:_nearest of "))
   (setvar "OSMODE" 33)
   (setq pt2 (getpoint "\nDiem cuoi duong cap :_end,int of " ))
   (setq L0 (getreal (deflt "\nChieu dai doan cap thang dau dam :"  *L0)))
   (if (not L0) (setq L0 *L0) (setq *L0 L0))   
   (setvar "OSMODE" 0)
   (command "UCS" "3p" pt0 pt1 pt2)
   (setq pt2 (trans pt2 0 1))
   (setq x2 (car pt2)
         y2 (cadr pt2)
   )
   (setq a (/ y2 (- x2 L0) (+ x2 L0)))
   (setq x21 (- x2 L0)
         y21 (* a (expt x21 2.0))
         pt21 (list x21 y21)
   )

  (setq SC (getreal (deflt (strcat "\nNhap khoang cach le (S<=" (rtos x2 2 2) ") :" ) *SC)))
  (if (not SC) (setq SC *SC) (setq *SC SC))   
 

 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ;Draw spiral curve of the first spiral
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq SS SC)
(setvar "OSMODE" 0)
 (command "PLINE")      
 (command "0,0")
      (While (< SS x21)
       (progn
       (setq fpt (list SS (* a (expt ss 2.0)))
         SS (+ SS SC)
      )
   (command fpt)      
    );progn  
   );while
   (command pt21)
   (command pt2)
   (command "")      
   (command "UCS" "W")
 
   (prompt (strcat "Phuong trinh duong cong : y = " (rtos a 2 15) " * x^2"))
   (setvar "OSMODE" oslast)
   (setvar "CMDECHO" 1)
   (princ)
);defun X2
(princ)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


