;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(prompt "\n3TAM  : Cam duong cong 3 tam tai nut")
(prompt "\n   Nguyen Quoc Cuong - 04/1998")
(princ)

(defun  *error* (msg)
  (setvar "OSMODE" oslast)
  (princ msg)
 (princ) 
);defun

(defun deflt (str1 def)
  (strcat Str1 "<" (rtos def 2 3 ) ">: ")
)

(defun c:3TAM( / pt pt1 pt2 T1 T2 al1 al2 al3 R1 R2 R3 phi phi3 ang1 ang2 ang11 ang31
                pt11 pt31 s1 s3 x1 y1 x2 y2 x3 y3 x21 y21 x31 y31)
 (setq oslast (getvar "OSMODE"))
 (if (not *R1) (setq *R1 20.0))
 (if (not *R2) (setq *R2 10.0))
 (if (not *R3) (setq *R3 25.0))

 (setq pt  (getpoint "\nDinh duong cong : " )
      pt1  (getpoint "\nCanh tuyen 1 : ")
      pt2  (getpoint "\nCanh tuyen 2 : ")
      ang1 (angle  pt pt1)
      ang2 (angle  pt pt2)
      phi  (abs (- pi (abs (- ang1 ang2))))
      R1  (getreal (deflt "\nNhap R1: " *R1))
      R2  (getreal (deflt "\nNhap R2: " *R2))
      R3  (getreal (deflt "\nNhap R3: " *R3))
      )

 (if (not R1) (setq R1 *R1) (setq *R1 R1))
 (if (not R2) (setq R2 *R2) (setq *R2 R2))
 (if (not R3) (setq R3 *R3) (setq *R3 R3))


 (setq al1 (/ (* 15 pi) 180)
       al3 (/ (* 20 pi) 180)
       al2 (- phi al1 al3)
       phi3 (- phi al3)
  )
 (setq x1  (* (sin al1) R1)
       y1  (* (- 1 (cos al1)) R1)
       x21 (* (sin al2) R2)
       y21 (* (- 1 (cos al2)) R2)
       x2  (+ x1 (* x21 (cos al1)) (- (* y21 (sin al1))))
       y2  (+ y1 (* y21 (cos al1)) (* x21 (sin al1)))
       x31 (* (sin al3) R3)
       y31 (* (- 1 (cos al3)) R3)
       x3  (+ x2 (* x31 (cos phi3)) (- (* y31 (sin phi3))))
       y3  (+ y2 (* y31 (cos phi3)) (* x31 (sin phi3)))
       T1  (- x3 (* y3 (/ (cos phi) (sin phi))))
       T2  (/ y3 (sin phi))
 )
 (prompt (strcat "\nT1 : " (rtos T1 2 4) "  T2 :" (rtos T2 2 4)))

 (setq s1 (sqrt (+ (* y1 y1) (* (- T1 x1) (- T1 x1))))
       ang11 (atan (/ y1 (- T1 x1)))
 )

 (setq s3 (sqrt (+ (* y31 y31) (* (- T2 x31) (- T2 x31))))
       ang31 (atan (/ y31 (- T2 x31)))
 )

(if (and (< (min ang1 ang2) 1.5708) (> (max ang1 ang2) 4.71239))
 (if (> ang2 ang1)                            ;if2
   (setq pt11 (polar pt (- ang1 ang11) S1)
         pt31 (polar pt (+ ang2 ang31) S3))
   (setq pt11 (polar pt (+ ang1 ang11) S1)
         pt31 (polar pt (- ang2 ang31) S3))
  );if2

  (if (> ang2 ang1)                            ;if3
   (setq pt11 (polar pt (+ ang1 ang11) S1)
         pt31 (polar pt (- ang2 ang31) S3))
   (setq pt11 (polar pt (- ang1 ang11) S1)
         pt31 (polar pt (+ ang2 ang31) S3))
  );if3

);if1
 (setq pt1 (polar pt ang1 T1)      
       pt2 (polar pt ang2 T2)      
 )
 (setvar "OSMODE" 0)
 (command "ARC" pt1 "e" pt11 "D"  pt)
 (command "ARC" ""  pt31 )
 (command "ARC" ""  pt2)
 (setvar "OSMODE" oslast)
 (princ)
);defun 3TAM
(princ)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;