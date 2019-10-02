
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


(defun c:STA(/ pt1 pt2 x1 chain1 km le pt0 ed)
   (if (or ( = x0 nil) (= chain0 nil))
      (Progn
         (setq pt0 (getpoint "\nPick the reference point : ")) 
         (setq ed (entget (car (entsel "\nPick on the chainage of the reference point: "))))
         (setq chain0 (read (DXF 1 ed)))
         (setq x0 (car pt0))
      );pro
   );if
   (if (not *tln) (setq *TLN 100000.0))
  (if  ( = TLN nil) 
     (progn
         (setq TLN (getreal (DEFLT "\nHorizontal scale: " *TLN)))
         (if (not TLN) (setq TLN  *TLN) (setq *TLN   TLN))
     );pro
  ); if
   (setq pt1 (getpoint "\nPick a point : ")) 
   (setq OS (getvar "OSMODE"))
   (while (/= pt1 nil)
      (print)
      (setvar "OSMODE" 0)
      (setq pt2 (getpoint "\nPoint to write : " pt1))
      ;    (setq pt1 (trans pt1 1 0))
      (setq x1 (car pt1)
         Chain1 (+ (* (- x1 x0) (/ 1000.0 TLN)) chain0)
         Km (fix (/ chain1 1000.0))
         le (rtos (- chain1 (* 1000.0 Km)) 2 2)
         Km (itoa km)
      )
      (command "text" (list (car pt2) (+ (cadr pt2) (* 0.5 scale))) 0 (strcat "Sta. Km" Km "+" le))
      ;    (setq pt1 (trans pt1 0 1))
      (command "line" pt1 pt2 (strcat "@" (rtos (* 14 scale) 2 2)  "<0") "")
      (setvar "OSMODE" OS)
      (setq pt1 (getpoint "\nPick a point : ")) 
   )
   (setvar "OSMODE" OS)
   (princ)
)
(prompt "\nSTA : Write the CHAINAGE to the screen.")
