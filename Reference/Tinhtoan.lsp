;------------------------------------------------------------------
(defun UKWORD (bit kwd msg def / inp)
     (if (and def (/= def ""))
          (setq msg (strcat "\n" msg "<" def ">: ")
               bit (* 2 (fix (/ bit 2)))
          )
          (if (= " " (substr msg (strlen msg) 1))
               (setq msg (strcat "\n" (substr msg 1 (1- (strlen msg))) ": "))
               (setq msg (strcat "\n" msg ": "))
          )
     )
     (initget bit kwd)
     (setq inp (getkword msg))
     (if inp inp def)
)
;------------------------------------------------------------------
(defun USTR (bit msg def spflag / inp nval)
     (if (and def (/= def ""))
          (setq msg (strcat "\n" msg "<" def ">: ")
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
)
;------------------------------------------------------------------
(defun ETOS (arg / file)
     (if (= 'STR (type arg)) (setq arg (strcat "\"" arg "\"")))
     (setq file (open "TL" "w"))
     (princ arg file)
     (close file)
     (setq file (open "TL" "r"))
     (setq arg (read-line file))
     (close file)
     (close (open "TL" "w"))
     arg
)  
;------------------------------------------------------------------
(defun DXF (code elist)
     (cdr (assoc code elist))
)
;-------------------------------------------------------------
(defun Text_get ( / ssl  nsset temp ed )
     (setq #sset (ssget))
     (setq ssl (sslength #sset) 
           nsset (ssadd)
     )
     (print ssl)
     (princ "entities found. ")  
     (princ "\nVerifying the selected entities -- please wait. ")
     (while (> ssl 0)
          (progn
               (setq temp (ssname #sset (setq ssl (1- ssl))))
               (setq ed (entget temp))
               (if (= (DXF 0 ed) "TEXT") (ssadd temp nsset))
          )
     )
     (setq ssl (sslength nsset)
          #sset nsset
     )
     (print ssl)
     (princ "TEXT entities found. ")
     (princ)
);defun Text_get
;-------------------------------------------------------------
;(load "ch_code.lsp")
;-------------------------------------------------------------------
(prompt "\n		ATT: Add something in front of text ")
(defun c:att ( / ssl ed No new temp old )
  (Text_get)
  (setq str (getstring "\nAdd what: "))
  (setq ssl (sslength #sset))

  (while (> ssl 0)
    (progn
      (setq temp (ssname #sset (setq ssl (1- ssl))))
      (setq ed (entget temp))
      (setq old (cons 1 (DXF 1 ed)))
      (setq No (DXF 1 ed))
      (setq No (strcat str No))
      (setq new (cons 1 No))
      (setq ed (subst new old ed))
      (entmod ed)  
    )
  )
)

;-------------------------------------------------------------------
(prompt "\n		ATS: Add something after text ")
(defun c:ats ( / ssl ed No new temp old )
  (Text_get)
  (setq str (getstring "\nAdd what: "))
  (setq ssl (sslength #sset))

  (while (> ssl 0)
    (progn
      (setq temp (ssname #sset (setq ssl (1- ssl))))
      (setq ed (entget temp))
      (setq old (cons 1 (DXF 1 ed)))
      (setq No (DXF 1 ed))
      (setq No (strcat No str))
      (setq new (cons 1 No))
      (setq ed (subst new old ed))
      (entmod ed)  
    )
  )
)
;-----------------------------------------------------------
(prompt "\n		CNO to +-*/ ++ ** text entities with a number")
(defun cno()
  (setq SS (ssget '((0 . "text"))))   
  (setq ham (getstring "\nInput operator (+ - * / ++ **):"))
  (if (/= "**" ham "++") (setq fac (getreal "\nInput a number:")))
  (setq acc (getint "\nAccuracy:"))
  (setq DZ (getvar "DIMZIN"))
  (setvar "DIMZIN" 0)
  (setq i 0 sum 0 mul 1)
  (while (< i (sslength ss))
      (setq ed (entget (ssname ss i))     
            old (assoc 1 ed)
            No (read (cdr (assoc 1 ed)))
      )
      (if (numberp No) 
        (progn
          (cond
            ((= ham "+") (setq No (+ No fac)))
            ((= ham "++") (setq Sum (+ Sum No)))
            ((= ham "-") (setq No (- No fac)))
            ((= ham "*") (setq No (* No fac)))
            ((= ham "**") (setq Mul (* Mul No)))
            ((= ham "/") (setq No (/ No fac)))
          )
          (if (/= "**" ham "++")           
            (progn
             (setq new (cons 1 (rtos No 2 acc)))
             (setq ed (subst new old ed))
             (entmod ed)
             ;(setq No 0) 
            );progn
          );if      
         );progn
       );if   
       (setq i (+ i 1))  
   );while
   (if (or (= ham "++") (= ham "**"))
     (progn
        (if (= ham "++") (setq Noch Sum) (setq Noch Mul))
        (setq ed (entget (car (entsel "Thay cho so : ")))
              old (assoc 1 ed)
              new (cons 1 (rtos Noch 2 acc))
              ed (subst new old ed)
        );setq
        (entmod ed)
     );progn
   );if
(setvar "DIMZIN" DZ)
(princ)
);defun 
;-----------------------------------------------------------
(defun VHC ()
  (setq DZ (getvar "DIMZIN"))
  (setvar "DIMZIN" 0)
  (setq s1 (read (cdr (assoc 1 (entget (car (entsel "\nDien tich day tren :"))))))
        s2 (read (cdr (assoc 1 (entget (car (entsel "\nDien tich day duoi :"))))))
        h  (read (cdr (assoc 1 (entget (car (entsel "\nChieu cao hinh chop :"))))))
        Vhc (/ (* h (+ s1 s2 (sqrt (* s1 s2)))) 3)
  )
  (if (setq pt1 (getpoint "\nPoint to write: "))
   (progn         
    (setq height (/ (getvar "VIEWSIZE") 30))
    (command "text" "style" "2" pt1 "0" (rtos Vhc 2 4))
    (setq elst (entget (entlast))
          elst (subst (cons 40 height) (assoc 40 elst) elst)
    )
    (entmod elst)
   );progn
   (progn 
    (setq en (car (entsel "Thay cho the tich : ")))
    (setq elst (entget en))
    (setq elst (subst (cons 1 (strcat (rtos Vhc 2 4) "")) (assoc 1 elst) elst))
    (entmod elst)
   );progn
  );if
  (setvar "DIMZIN" DZ)
  (print)  
);defun
;-----------------------------------------------------------
(defun VLT ()
   (initget 7)
   (setq hso (/ 1000 (getreal "\nTy le:")))
   (setq dz (getvar "dimzin")
         os (getvar "osmode")
   )
   (setvar "dimzin" 0)
   (setvar "osmode" 33)
   (setq V 0 y0 (cadr (getpoint "\nChon diem tren truc quay :")))
   (prompt "\nDinh nghia dien tich can quay:")
   (setq p1 (getpoint "\nFrom point")) 
   (While (setq p2 (getpoint "\nTo point"))
     (setq r1 (- (cadr p1) y0)
           r2 (- (cadr p2) y0)
           s1 (* pi r1 r1)
           s2 (* pi r2 r2)
           h  (- (car p2) (car p1))
           V (+ V (/ (* h (+ s1 s2 (sqrt (* s1 s2)))) 3))
           p1 p2
     );setq
   );while
   (setq V (/ V hso hso hso))
   (setq elst (entget (car (entsel "\nThay cho the tich :"))))
   (setq elst (subst (cons 1 (rtos V 2 4)) (assoc 1 elst) elst))
   (entmod elst)
   (setvar "dimzin" dz)
   (setvar "osmode" os)
   (princ)
);defun
;-----------------------------------------------------------
(prompt "\n		DA to write +- for a number")
(defun c:da()
  (Text_get)
  ;(if (= acc nil) (setq acc (getint "\nInput accuracy: ")))
  (setq DZ (getvar "DIMZIN"))
  (setvar "DIMZIN" 0)
  (setq ssl (sslength #sset))
  (while (> ssl 0)
    (progn
      (setq temp (ssname #sset (setq ssl (1- ssl))))
      (setq ed (entget temp))
      (setq old (cons 1 (DXF 1 ed)))
      (setq y (atof (DXF 1 ed)))
      (cond
          ((> y 0) (setq txt (strcat "+" (DXF 1 ed))))
          ((< y 0) (setq txt (DXF 1 ed)))
          ((= y 0) (setq txt "%%p0.00"))
      )
      (setq new (cons 1 txt))
      (setq ed (subst new old ed))
      (entmod ed)  
    )
  )
(setvar "DIMZIN" DZ)
)

;------------------------------------------------------------------------------------

(prompt "\n		REP: change text into the same new text ")
(defun c:rep ( / ssl ed No new temp old )
  (Text_get)
  (setq str (getstring "\nnew text: "))
  (setq ssl (sslength #sset))
  (while (> ssl 0)
    (progn
      (setq temp (ssname #sset (setq ssl (1- ssl))))
      (setq ed (entget temp))
      (setq old (cons 1 (DXF 1 ed)))
      (setq No (DXF 1 ed))
      (setq No (strcat str))
      (setq new (cons 1 No))
      (setq ed (subst new old ed))
      (entmod ed)  
    )
  )
(princ)
(princ)
)

;-------------------------------------------------------------------
(prompt "\n		REP: change text into the same new text ")
(defun c:r1 ( / ssl ed No new temp old )
    
(progn
(prompt "\n\n		NEW TEXT:")
  (Text_get)
  (setq ssl (sslength #sset))
      (setq temp (ssname #sset (setq ssl (1- ssl))))
      (setq ed (entget temp))
      (setq str (cons 1 (DXF 1 ed)))
      (setq str (DXF 1 ed))
)
  (progn
  (prompt "\n\n		SELECT OLD TEXT :")
  (Text_get)
  (setq ssl (sslength #sset))
  (while (> ssl 0)
    (progn
      (setq temp (ssname #sset (setq ssl (1- ssl))))
      (setq ed (entget temp))
      (setq old (cons 1 (DXF 1 ed)))
      (setq No (DXF 1 ed))
      (setq No (strcat str))
      (setq new (cons 1 No))
      (setq ed (subst new old ed))
      (entmod ed)  
    )
    );pro
  )
(progn
(command ".chprop" "p" "" "la" "3" "LT" "BYLAYER" "C" "BYLAYER" "")
)
(princ)
(princ)
)

;-------------------------------------------------------------------
(prompt "\n		STT: change text into the order of numbers  ")
(defun c:STT ( / ssl ed No new temp old )
  (Text_get)
  (setq fac (getreal "\nInput start number:"))
  (setq DZ (getvar "DIMZIN"))
  (setvar "DIMZIN" 0)
  (setq ssl (sslength #sset))
  (while (> ssl 0)
    (progn
      (setq temp (ssname #sset (setq ssl (1- ssl))))
      (setq ed (entget temp))
      (setq old (cons 1 (DXF 1 ed)))
      (setq No (atof (DXF 1 ed)))
      (setq No fac)
      (setq new (cons 1 (rtos No 2 0)))
      (setq ed (subst new old ed))
      (setq fac (+ 1 fac))
      (entmod ed) 
    )
  )
(setvar "DIMZIN" DZ)
)


;-----------------------------------------------------------------------------------------------------

(prompt "\n		sum to + text entities together")
(defun c:sum ( / No temp ed old new acc fac operator ssl )
  (Text_get)
  (if (= scale nil) (setq scale (getreal "\nInput current scale : ")))
  (setq DZ (getvar "DIMZIN"))
  (setvar "DIMZIN" 0)
  (setq ssl (sslength #sset))
  (setq sum 0)
  (while (> ssl 0)
    (progn
      (setq temp (ssname #sset (setq ssl (1- ssl))))
      (setq ed (entget temp))
      (setq old (cons 1 (DXF 1 ed)))
      (setq No (atof (DXF 1 ed)))
      (setq sum (+ sum No))
     )
  )
(setq acc (getint "\nAccuracy:"))
(setvar "DIMZIN" DZ)
(setq pt3 (getpoint "Where to write the result : "))
(setvar "TEXTSTYLE" "STANDARD")
(command "text" "m" (list (car pt3) (cadr pt3)) (* 1.6 scale) 0 (rtos sum 2 acc))

)


;-----------------------------------------------------------

(prompt "\n		mul to * text entities together")
(defun c:mul ( / No temp ed old new acc fac operator ssl )
  (Text_get)
  (if (= scale nil) (setq scale (getreal "\nInput current scale : ")))
  (setq DZ (getvar "DIMZIN"))
  (setvar "DIMZIN" 0)
  (setq ssl (sslength #sset))
  (setq mul 1)
  (while (> ssl 0)
    (progn
      (setq temp (ssname #sset (setq ssl (1- ssl))))
      (setq ed (entget temp))
      (setq old (cons 1 (DXF 1 ed)))
      (setq No (atof (DXF 1 ed)))
      (setq mul (* mul No))
     )
  )
(setq acc (getint "\nAccuracy:"))
(setvar "DIMZIN" DZ)
(setq pt3 (getpoint "Where to write the result : "))
(setvar "TEXTSTYLE" "STANDARD")
(command "text" "m" (list (car pt3) (cadr pt3)) (* 1.6 scale) 0 (rtos mul 2 acc))

)




;-----------------------------------------------------------

(prompt "\n\n		TT to +-*/ text entities with a number")
(defun c:TT ( / No temp ed old new acc fac operator ssl )
  
  (setq operator (getstring "\nInput operator (+ - * /):"))
  (setq acc (getint "\nAccuracy:"))
  (setq DZ (getvar "DIMZIN"))
  (setvar "DIMZIN" 0)

(progn
(prompt "\n\n		NHAP SO THU 1 :")
  (Text_get)
  (setq ssl (sslength #sset))
      (setq temp (ssname #sset (setq ssl (1- ssl))))
      (setq ed (entget temp))
      (setq old (cons 1 (DXF 1 ed)))
      (setq No1 (atof (DXF 1 ed)))
)

(while (/= ss1 0)
(progn
(prompt "\n\n		NHAP SO THU 2 :")
  (Text_get)
  (setq ssl (sslength #sset))
      (setq temp (ssname #sset (setq ssl (1- ssl))))
      (setq ed (entget temp))
      (setq old (cons 1 (DXF 1 ed)))
      (setq No2 (atof (DXF 1 ed)))
)

(progn
(prompt "\n\n		NHAP SO THU 3 :")
  (Text_get)
  (setq ssl (sslength #sset))
      (setq temp (ssname #sset (setq ssl (1- ssl))))
      (setq ed (entget temp))
      (setq old (cons 1 (DXF 1 ed)))
)
      (cond
         ((= operator "+") (setq No3 (+ No1 No2)))
         ((= operator "-") (setq No3 (- No1 No2)))
         ((= operator "*") (setq No3 (* No1 No2)))
         ((= operator "/") (setq No3 (/ No1 No2)))
      )
	(setq sign (substr (DXF 1 ed) 1 1))
	(cond
           ((= sign "+")(setq new (cons 1 (strcat "+" (rtos No3 2 acc)))))
    	   ((/= sign "+")(setq new (cons 1 (rtos No3 2 acc))))
	)	

(setq ed (subst new old ed))
(entmod ed)
(if (= scale nil) (setq scale (getreal "\nInput current scale : ")))
(setvar "DIMZIN" DZ)
;(setq pt3 (getpoint "Where to write the result : "))
;(command "text" "m" (list (car pt3) (cadr pt3)) (* 1.6 scale) 0 (rtos No3 2 acc))
(prompt (strcat "\n The result : " (rtos No3 2 acc)))

(progn
(prompt "\n\n		NHAP SO THU 1 :")
  (Text_get)
  (setq ssl (sslength #sset))
      (setq temp (ssname #sset (setq ssl (1- ssl))))
      (setq ed (entget temp))
      (setq old (cons 1 (DXF 1 ed)))
      (setq No1 (atof (DXF 1 ed)))
)

);while
)


;-----------------------------------------------------------------------------------------------------

(prompt "\n		to : tinh tong trong bang thong ke")
(defun c:TO ( / No temp ed old new acc fac operator ssl )
  (if (= scale nil) (setq scale (getreal "\nInput current scale : ")))
  (if (= aaa nil) (setq aaa (getint "\nAccuracy:")))
  (setq DZ (getvar "DIMZIN"))
  (setvar "DIMZIN" 0)
(progn
(prompt "\n\n		NHAP CAC SO HANG :")
  (Text_get)
  (setq ssl (sslength #sset))
  (setq sum 0)
  (while (> ssl 0)
    (progn
      (setq temp (ssname #sset (setq ssl (1- ssl))))
      (setq ed (entget temp))
      (setq old (cons 1 (DXF 1 ed)))
      (setq No (atof (DXF 1 ed)))
      (setq sum (+ sum No))
     )
  )
);pro
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(while (/= ss1 0)
(progn
(prompt "\n\n		NHAP SO KET QUA :")
  (Text_get)
  (setq ssl (sslength #sset))
      (setq temp (ssname #sset (setq ssl (1- ssl))))
      (setq ed (entget temp))
      (setq old (cons 1 (DXF 1 ed)))
      (setq new (cons 1 (strcat (rtos sum 2 aaa))))
)	
(setq ed (subst new old ed))
(entmod ed)
(if (= scale nil) (setq scale (getreal "\nInput current scale : ")))
(setvar "DIMZIN" DZ)
(progn
(prompt "\n\n		NHAP CAC SO HANG :")
  (Text_get)
  (setq ssl (sslength #sset))
  (setq sum 0)
  (while (> ssl 0)
    (progn
      (setq temp (ssname #sset (setq ssl (1- ssl))))
      (setq ed (entget temp))
      (setq old (cons 1 (DXF 1 ed)))
      (setq No (atof (DXF 1 ed)))
      (setq sum (+ sum No))
     )
  )
);pro

);while

);end


;-----------------------------------------------------------

(prompt "\n		ti : tinh tich trong bang thong ke")
(defun c:ti ()
  (if (= scale nil) (setq scale (getreal "\nInput current scale : ")))
  (if (= bbb nil) (setq bbb (getint "\nAccuracy:")))
  (setq DZ (getvar "DIMZIN"))
  (setvar "DIMZIN" 0)
(progn
(prompt "\n\n		NHAP CAC THUA SO :")
  (Text_get)
  (setq ssl (sslength #sset))
  (setq sum 1)
  (while (> ssl 0)
    (progn
      (setq temp (ssname #sset (setq ssl (1- ssl))))
      (setq ed (entget temp))
      (setq old (cons 1 (DXF 1 ed)))
      (setq No (atof (DXF 1 ed)))
      (setq sum (* sum No))
     )
  )
);pro
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(while (/= ss1 0)
(progn
(prompt "\n\n		NHAP SO KET QUA :")
  (Text_get)
  (command ".chprop" "p" "" "la" "3" "LT" "BYLAYER" "C" "BYLAYER" "")
  (setq ssl (sslength #sset))
      (setq temp (ssname #sset (setq ssl (1- ssl))))
      (setq ed (entget temp))
      (setq old (cons 1 (DXF 1 ed)))
      (setq sum (/ sum 1000))
      (setq new (cons 1 (strcat (rtos sum 2 bbb))))
)	
(setq ed (subst new old ed))
(entmod ed)
(if (= scale nil) (setq scale (getreal "\nInput current scale : ")))
(setvar "DIMZIN" DZ)
(progn
(prompt "\n\n		NHAP CAC THUA SO :")
  (Text_get)
  (setq ssl (sslength #sset))
  (setq sum 1)
  (while (> ssl 0)
    (progn
      (setq temp (ssname #sset (setq ssl (1- ssl))))
      (setq ed (entget temp))
      (setq old (cons 1 (DXF 1 ed)))
      (setq No (atof (DXF 1 ed)))
      (setq sum (* sum No))
     )
  )
);pro

);while

);end


;------------------------------------------------------------------------------------
(prompt "\n\n This lisp file is written by Nobody")











