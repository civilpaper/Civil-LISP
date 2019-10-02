
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

(defun USTR (bit msg def spflag / inp nval)
  (if (and def (/= def ""))
    (setq msg (strcat "\n" msg " <" def ">: ")
          inp (getstring spflag msg)
          inp (if (= inp "") def inp)
    )
    (progn
      (if (= " " (substr msg (strlen msg) 1))
        (setq msg (strcat "\n" (substr msg 1 (1- (strlen msg))) ": "))
        (setq msg (strcat "\n" msg ": "))
      )
      (if (= bit 1)
        (while (= "" (setq inp (getstring spflag msg)))
           (prompt "\nInvalid String.")
        )
        (setq inp (getstring spflag msg))
      )
    )
  )
  inp
);defun USTR
;-----------------------------------------------------

(defun c:tdt()
(if (= scale nil) (c:TL))

(setq file (USTR 4 "Ten file toa do (khong nhap phan mo rong)" file T))
(setq tenf (strcat file ".tdo"))
(setq f (open tenf "a"))
(if (not *tentru) (setq *tentru ""))
(setq tentru (getstring (defltstr "\nNhap ten mo tru: " *tentru)))
(if (not tentru) 
  (setq tentru *tentru) 
  (progn
    (setq *tentru tentru)
    (write-line (strcat "\t" tentru) f)
  );pro
);if

(if (not *count) (setq *count 1))
(setq count (getint (defltint "\nNhap STT diem dau tien : " *count)))
(if (not count) (setq count *count) (setq *count count))
 (setq ST1 (getstring (defltstr "\nTen diem : " (itoa count) )))
  (if (= ST1 "") (setq ST1 (itoa count)))
  
(setq pt1 (getpoint "Toa do diem : ")) 
(while (/= pt1 nil)
    (print)
    (setq OS (getvar "OSMODE"))
    (setvar "OSMODE" 0)
    (setvar "DIMZIN" 0)
    (setq pt2 (getpoint "\nViet o dau : " pt1))
    (setq pt1 (trans pt1 1 0))
    (setq x1 (car pt1) y1 (cadr pt1))
    (setq xx1 (rtos x1 2 4) yy1 (rtos y1 2 4))
    (write-line (strcat ST1 "\t" xx1 "\t" yy1) f)
    (command "circle" pt2 (* 2.0 scale))
    (command "TEXT" "m" pt2 "0" ST1)
    (setq pt1 (trans pt1 0 1))
    (setq a1 (angle pt1 pt2))

 
    (setq pt1 (trans pt1 1 0))
    (setq pt2 (trans pt2 1 0))
    (setq L1 (- (distance pt1 pt2) (* 2.0 scale)))
    (setq pt1 (trans pt1 0 1))
    (command "donut" 0.0 (* 0.5 scale) pt1 "")
    (setq pt3 (polar pt1 a1 L1))
    ;(setq pt3 (trans pt3 0 1))

    (command "line" pt1 pt3 "")

;    (command "text" (list (car pt2) (+ (cadr pt2) (* 0.5 scale))) 0 (strcat "N=" yy1))
;    (command "text" (list (car pt2) (- (cadr pt2) (* 2.0 scale))) 0 (strcat "E= " xx1))    (setq pt1 (trans pt1 0 1))
;    (command "line" pt1 pt2 (strcat "@" (rtos (* 14 scale) 2 2)  "<0") "")

    (setvar "OSMODE" OS)
    (setq count (+ 1 count))
    (setq ST1 (getstring (defltstr "\nTen diem : " (itoa count) )))
    (if (= ST1 "") (setq ST1 (itoa count)))
    (setq pt1 (getpoint "Toa do diem : "))
    (setq *count count)
 )
(close f)
(princ)
)
(prompt "\nTDT : Ghi toa do cac diem mong mo-tru ra file va danh so tren ban ve")
(princ  "\n                  Nguyen Quoc Cuong - 12/11/2002")
(princ)
