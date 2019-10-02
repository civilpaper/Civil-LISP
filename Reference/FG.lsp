(vmon)
;*
;*
(defun deflt (str1 def)
   (strcat Str1 "<" (rtos def 2 3 ) ">: ")
);defun

(defun defltstr (str1 def)
   (strcat Str1 "<"  def ">: ")
);defun

(defun C:FG ()
   ; (IF (= TL NIL) (setq tl (getreal "Ty le ban ve :")))
   ;  (setq my 200.0)
   ;  (setq NTL (/ 1000.0 tl))
   (command "layer" "s" 0 "")
   (SETQ LAST (GETVAR "OSMODE"))
   (command "osnap" "NONE")
   (command "SETVAR" "TEXTSTYLE" "2" "")
   (command "SETVAR" "dimzin" "0")
   (SETVAR "OSMODE" 512)
   (setq pt (getpoint "\nNoi ghi cao do <nearest to>  : "))
   (print)
   (setq y2 (cadr pt))
   (setq y1 (- y2 100.0))
   (SETVAR "OSMODE" 64)
   (setq pt (getpoint "\nDiem < insert of > : "))
   (SETVAR "OSMODE" 0)
   (while (/= pt nil)
      (progn
         (setq x (car pt) y (cadr pt))
         (setq h (/ y 100.0))
         (command "layer" "s" 0 "")
         (command "text" "r" (list x y1) 90 (rtos h 2 3))
         (SETVAR "OSMODE" 64)
         (setq pt (getpoint "\nDiem < insert of > : "))
         (SETVAR "OSMODE" 0)
      )progn
   )
 (command "OSMODE" LAST)
 (princ)
);defun
(prompt "\n FG : ghi cao do THIET KE")
