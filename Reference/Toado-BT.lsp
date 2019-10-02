
(defun DXF (code elist)
  (cdr (assoc code elist))
)

(defun deflt (str1 def)
   (strcat Str1 "<" (rtos def 2 3 ) ">: ")
);defun

(defun defltint (str1 int)
   (strcat Str1 "<" (itoa int) ">: ")
);defun

(defun defltstr (str1 def)
   (strcat Str1 "<"  def ">: ")
);defun



(defun DtR (deg) ; converts degree to radian
   (/ (* deg pi) 180)
);defun

(defun RtD (rad) ; converts radian to degree
   (/ (* rad 180) pi)
);defun

(defun  *error* (msg)
   (setvar "OSMODE" oslast)
   (princ msg)
   (princ) 
);defun


(defun c:td()

(setq file (getstring T "Ten file toa do (khong nhap phan mo rong) : "))
(setq tenf (strcat file ".tdo"))
(setq f (open tenf "a"))

(if (not *count) (setq *count 0.00))
(setq count (getreal (deflt "\nNhap y trinh dau tien : " *count)))
(if (not count) (setq count *count) (setq *count count))

(if (not *inclen) (setq *inclen 20.00))
(setq inclen (getreal (deflt "\nNhap so gia ly trinh : " *inclen)))
(if (not inclen) (setq inclen *inclen) (setq *inclen inclen))


(setq ST1 (getstring (defltstr "\nTen diem : " (strcat "Km" (itoa (Fix (/ count 1000))) "+" (rtos (- count (* 1000 (fix (/ count 1000)))) 2 2)) )))
;(setq ST1 (getstring (defltstr "\nTen diem : " (rtos count 2 2) )))
(if (= ST1 "") (setq ST1 (strcat "Km" (itoa (Fix (/ count 1000))) "+" (rtos (- count (* 1000 (fix (/ count 1000)))) 2 2))))
(setq pt1 (getpoint "Toa do diem : ")) 
(while (/= pt1 nil)
    (print)
    (setq x1 (car pt1) y1 (cadr pt1))
    (setq xx1 (rtos x1 2 4) yy1 (rtos y1 2 4))
    (write-line (strcat ST1  "      " xx1 "      " yy1) f)
    (setq count (+ Inclen count))
    (setq ST1 (getstring (defltstr "\nTen diem : " (strcat "Km" (itoa (Fix (/ count 1000))) "+" (rtos (- count (* 1000 (fix (/ count 1000)))) 2 2)) )))
;    (setq ST1 (getstring (defltstr "\nTen diem : " (rtos count 2 2) )))
    (if (= ST1 "") (setq ST1 (strcat "Km" (itoa (Fix (/ count 1000))) "+" (rtos (- count (* 1000 (fix (/ count 1000)))) 2 2))))
;    (if (= ST1 "") (setq ST1 (rtos count 2 2)))
    (setq pt1 (getpoint "Toa do diem : ")) 

)
(close f)
(princ)
)
(prompt "\nTD : Ghi toa do diem ra file")
