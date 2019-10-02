(prompt "\n\nL O A D I N G    T E X T _ U T I . L S P")

(defun DXF (code elist)
   (cdr (assoc code elist))
)
(defun CDXF (tt elist)
   (car (nth tt elist))
)
;CDXF - xac dinh thu tu cua tieu danh sach
(defun xdtt (code elist)
   (setq i 0)
   (while (/= code (cdxf i elist))
      ;(progn
      (setq i (+ 1 i))
   );while
)
(defun RAD (degree)
   (setq radian (/ (* degree pi) 180))
   radian
)
(defun DEG (Rad)
   (setq degree (/ (* rad 180) pi))
   degree
)
(defun tangent (Rad)
   (setq tang (/ (sin rad) (cos rad)))
   tang
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun text_u_ssget ( / ssl  nsset temp ed )
   (setq sset (ssget))
   (setq ssl (sslength sset) 
      nsset (ssadd)
   )
   (print ssl)
   (princ "entities found. ")  
   (princ "\nVerifying the selected entities ... please wait. ")
   (while (> ssl 0)
      (progn
         (setq temp (ssname sset (setq ssl (1- ssl))))
         (setq ed (entget temp))
         (if (= (DXF 0 ed) "TEXT") (ssadd temp nsset))
      )
   )
   (setq ssl (sslength nsset)
      sset nsset
   )
   (print ssl)
   (princ "TEXT entities found. ")
   (princ)
   (print)
);defun u_ssget
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun block_u_ssget ( / ssl  nsset temp ed )
   (setq sset (ssget))
   (setq ssl (sslength sset) 
      nsset (ssadd)
   )
   (print ssl)
   (princ "entities found. ")  
   (princ "\nVerifying the selected entities -- please wait. ")
   (while (> ssl 0)
      (progn
         (setq temp (ssname sset (setq ssl (1- ssl))))
         (setq ed (entget temp))
         (if (= (DXF 0 ed) "INSERT") (ssadd temp nsset))
      )
   )
   (setq ssl (sslength nsset)
      sset nsset
   )
   (print ssl)
   (princ "BLOCK entities found. ")
   (princ)
   (print)
);defun block_u_ssget
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(princ "\n		TO  : To oblique or straighten up TEXT entities")
(princ)
(defun c:TO( / sset ssl temp ed old new )
   (text_u_ssget)
   (setq ssl (sslength sset))
   (if (> ssl 0) 
      (setq ob_angle (getreal "New oblique angle of the selected texts : "))
   )
   (while (> ssl 0)
      (progn
         (setq temp (ssname sset (setq ssl (1- ssl))))
         (setq ed (entget temp))
         
         (setq old (cons 51 (DXF 51 ed))
            new (cons 51 (tangent (rad ob_angle)))
            ed (subst new old ed)
         )
         (entmod ed)  
      )  
   )
   (princ)
);defun
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(princ "\n		CT2 : Thay ®æi ch÷ vÒ CT2 vµ theo tû lÖ hiÖn hµnh")
(princ)
(defun c:CT2( / sset ssl temp ed old new )
   (text_u_ssget)
   (if (= scale nil) (setq scale (getreal "\nChän lùa tû lÖ : ")))
   (setq ssl (sslength sset))
   (while (> ssl 0)
      (progn
         (setq temp (ssname sset (setq ssl (1- ssl))))
         
         (setq ed (entget temp))
         (progn
            (setq old (cons 7 (DXF 7 ed))
               sty "2"
               new (cons 7 sty)
               ed (subst new old ed)
            )
            (setq old (cons 40 (DXF 40 ed))
               size (* 1.6 scale)
               new (cons 40 size)
               ed (subst new old ed)
            )
            (setq old (cons 41 (DXF 41 ed))
               width  0.7
               new (cons 41 width)
               ed (subst new old ed)
            )
         )  
         
         (entmod ed)  
      )
   )
   (princ)
);defun TT2
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(princ "\n		CT2d : To update TEXT to style 2d and current size")
(princ)
(defun c:CT2D( / sset ssl temp ed old new )
   (text_u_ssget)
   (if (= scale nil) (setq scale (getreal "\nChän lùa tû lÖ : ")))
   (setq ssl (sslength sset))
   (while (> ssl 0)
      (progn
         (setq temp (ssname sset (setq ssl (1- ssl))))
         
         (setq ed (entget temp))
         (progn
            (setq old (cons 7 (DXF 7 ed))
               sty "2d"
               new (cons 7 sty)
               ed (subst new old ed)
            )
            (setq old (cons 40 (DXF 40 ed))
               size (* 2.0 scale)
               new (cons 40 size)
               ed (subst new old ed)
            )
            (setq old (cons 41 (DXF 41 ed))
               width  1.0
               new (cons 41 width)
               ed (subst new old ed)
            )
         )  
         
         (entmod ed)  
      )
   )
   (princ)
);defun TT2
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(princ "\n		CT5 : To update TEXT to style 5 and current size")
(princ)
(defun c:CT5( / sset ssl temp ed old new )
   (text_u_ssget)
   (if (= scale nil) (setq scale (getreal "\nChän lùa tû lÖ : ")))
   (setq ssl (sslength sset))
   (while (> ssl 0)
      (progn
         (setq temp (ssname sset (setq ssl (1- ssl))))
         
         (setq ed (entget temp))
         (progn
            (setq old (cons 7 (DXF 7 ed))
               sty "5"
               new (cons 7 sty)
               ed (subst new old ed)
            )
            (setq old (cons 40 (DXF 40 ed))
               size (* 3.5 scale)
               new (cons 40 size)
               ed (subst new old ed)
            )
            (setq old (cons 41 (DXF 41 ed))
               width  1.0
               new (cons 41 width)
               ed (subst new old ed)
            )
         )  
         (entmod ed)  
      )
   )
   (princ)
);defun CT5
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(princ "\n		CT3 : To update TEXT to style 3 and current size")
(princ)
(defun c:CT3( / sset ssl temp ed old new )
   (text_u_ssget)
   (if (= scale nil) (setq scale (getreal "\nChän lùa tû lÖ : ")))
   (setq ssl (sslength sset))
   (while (> ssl 0)
      (progn
         (setq temp (ssname sset (setq ssl (1- ssl))))
         
         (setq ed (entget temp))
         (progn
            (setq old (cons 7 (DXF 7 ed))
               sty "3"
               new (cons 7 sty)
               ed (subst new old ed)
            )
            (setq old (cons 40 (DXF 40 ed))
               size (* 2.5 scale)
               new (cons 40 size)
               ed (subst new old ed)
            )
            (setq old (cons 41 (DXF 41 ed))
               width  1.0
               new (cons 41 width)
               ed (subst new old ed)
            )
         )  
         (entmod ed)  
      )
   )
   (princ)
);defun CT3
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(princ "\n		VA  : To align TEXT entities horizontally")
(princ)
(defun c:VA( / sset ssl temp ed old new )
   (setq sset (ssget))
   (setq ssl (sslength sset) 
      nsset (ssadd)
   )
   (print ssl)
   (princ "entities found. ")  
   (print)
   ;(text_u_ssget)
   (setq ssl (sslength sset))
   (if (> ssl 0) 
   (setq pt1 (getpoint "New aligned point of the selected entities : ")))
   (while (> ssl 0)
      (progn
         (setq temp (ssname sset (setq ssl (1- ssl))))
         (setq ed (entget temp))
         ;;;TEXT
         (if (= (DXF 0 ed) "TEXT")
            (progn
               (if (or (/= (DXF 72 ed) 0) (/= (DXF 73 ed) 0))
                  (setq old (nth (xdtt 10 ed) ed)
                     new (list (nth 0 old) (nth 1 old) (cadr pt1) (nth 3 old))
                     ed (subst new old ed)
                     old (nth (xdtt 11 ed) ed)
                     new (list (nth 0 old) (nth 1 old) (cadr pt1) (nth 3 old))
                     ed (subst new old ed)
                  )     
                  (setq old (nth (xdtt 10 ed) ed)
                     new (list (nth 0 old) (nth 1 old) (cadr pt1) (nth 3 old))
                     ed (subst new old ed)
                  )
               )
               (entmod ed)  
            )  
         );if TEXT
         
         ;;;BLOCK
         (if (= (DXF 0 ed) "INSERT")
            (progn
               (setq old (nth (XDTT 10 ED) ed))
               (setq new (list (nth 0 old) (nth 1 old) (cadr pt1) (nth 3 old)))
               (setq y1 (nth 2 old))
               (setq ed (subst new old ed))
               (entmod ed)  
               (setq en (entnext (DXF -1 ed)))
               (while (/= (DXF 0 (entget en)) "SEQEND")
                  (progn
                     (setq ed (entget en))
                     (setq old (nth (XDTT 10 ED) ed))
                     (setq y2 (nth 2 old))
                     (setq dy (- y2 y1))
                     (setq new (list (nth 0 old) (nth 1 old) (+ (cadr pt1) dy) (nth 3 old)))
                     (setq ed (subst new old ed))
                     (entmod ed)  
                     (setq en (entnext en))
                  );progn
               );while
               (entupd en)
            );progn
         );if BLOCK
      )
   )
   
   (setq pt1 nil)
   (princ)
);defun
;;;;;end of old
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(princ "\n		CA  : To switch between UPCASE and LOWCASE of TEXT entities")
(princ)
(defun c:CA( / sset ssl temp ed old new )
   (text_u_ssget)
   (initget 1 "U u L l")
   (setq ssl (sslength sset))
   (if (> ssl 0) 
   (setq sw (getkword "New case of the selected texts [U/L] : ")))
   (while (> ssl 0)
      ( if (or (= sw "U") (= sw "u"))
         (progn
            (setq temp (ssname sset (setq ssl (1- ssl))))
            (setq ed (entget temp))
            
            (setq old (cons 1 (DXF 1 ed))
               new (cons 1 (strcase (DXF 1 ed)))
               ed (subst new old ed)
            )
            (entmod ed)  
         )  
      );if
      ( if (or (= sw "l") (= sw "L"))
         (progn
            (setq temp (ssname sset (setq ssl (1- ssl))))
            (setq ed (entget temp))
            
            (setq old (cons 1 (DXF 1 ed))
               new (cons 1 (strcase (DXF 1 ed) T))
               ed (subst new old ed)
            )
            (entmod ed)  
         )  
      );if
   );while
   (princ)
);defun CA
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(princ "\n		TC  : To change layer of TEXT entities")
(princ)

(defun c:TC( / sset ssl temp ed old new )
   (text_u_ssget)
   (setq ssl (sslength sset))
   (if (> ssl 0) 
      (setq new_layer (getstring "New layer of the selected texts : "))
   )
   (while (> ssl 0)
      (progn
         (setq temp (ssname sset (setq ssl (1- ssl))))
         (setq ed (entget temp))
         
         (setq old (cons 8 (DXF 8 ed))
            new (cons 8 new_layer)
            ed (subst new old ed)
         )
         (entmod ed)  
      )  
   )
   
   (princ)
);defun
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(princ)
(prompt "\n		MUL : To multiply the selected text entities to a given value")
(defun c:mul(); / sset ssl temp ed oldval newval old new subval oldlay newlay)
   (text_u_ssget)
   (print)
   (print)
   ;(setvar "DIMZIN" 0)
   
   (if (not *MUL) (setq *Mul 1.0))
   (setq subval (getreal (deflt "Input the operator value : " *MUL)))
   (if (not subval ) (setq subval  *mul) (setq *mul   subval))
   
   (if (not *ACC) (setq *ACC 3))
   (setq ACC (getint (defltint "Input the accuracy level: " *ACC)))
   
   (if (not ACC ) (setq ACC  *ACC) (setq *ACC   ACC))
   
   (setq ssl (sslength sset))
   (while (> ssl 0)
      (progn
         (setq temp (ssname sset (setq ssl (1- ssl))))
         
         (setq ed (entget temp))
         (setq oldval (read (DXF 1 ed)))
         (setq newval (rtos (* oldval subval) 2 ACC))
         (setq old (cons 1 (DXF 1 ed))
            new (cons 1 newval)
            ed (subst new old ed)
         );setq
         
         (setq oldlay (cons 8 (DXF 8 ed))
            newlay (cons 8 "2")
            ed (subst newlay oldlay ed)
         );setq
         
         (entmod ed) 
      );progn
   );while
   ; (princ)
);defun Text-mul
;------------------------------------------------
(princ)
(prompt "\n		TS  : To subtract the selected text entities to a given value")
(defun c:TS()
   ; sset ssl temp ed oldval newval old new subval oldlay newlay)
   (text_u_ssget)
   (print)
   
   (print)
   (setvar "DIMZIN" 0)
   (if (= #ACC nil) (setq #ACC (getint "Input the nuber of fraction: ")))
   (setq subval (getreal "Input the subtract value : "))
   (setq ssl (sslength sset))
   (while (> ssl 0)
      (progn
         (setq temp (ssname sset (setq ssl (1- ssl))))
         
         (setq ed (entget temp))
         (PROGN
            (setq oldval (read (DXF 1 ed)))
            (setq newval (rtos (- oldval subval) 2 #ACC))
            (setq old (cons 1 (DXF 1 ed))
               new (cons 1 newval)
               ed (subst new old ed)
            );setq
            (setq oldlay (cons 8 (DXF 8 ed))
               newlay (cons 8 "4")
               ed (subst newlay oldlay ed)
            );setq
            
         );PROGN
         
         (entmod ed)  
         
      );progn
   );while
   (princ)
);defun TS()

(princ)
(prompt "\n		PRE : To insert PREFIX to the selected text entities")
(defun c:pre()
   ; sset ssl temp ed oldval newval old new subval oldlay newlay)
   (text_u_ssget)
   (print)
   (print)
   (setvar "DIMZIN" 0)
   (setq pre (getstring "Input the prefix : "))
   (setq ssl (sslength sset))
   (while (> ssl 0)
      (progn
         (setq temp (ssname sset (setq ssl (1- ssl))))
         
         (setq ed (entget temp))
         (PROGN
            (setq oldval (DXF 1 ed))
            (setq newval (strcat pre oldval))
            (setq old (cons 1 (DXF 1 ed))
               new (cons 1 newval)
               ed (subst new old ed)
            );setq
            (setq oldlay (cons 8 (DXF 8 ed))
               newlay (cons 8 "2")
               ed (subst newlay oldlay ed)
            );setq
            
         );PROGN
         
         (entmod ed)  
         
      );progn
   );while
   (princ)
);defun PRE()
;--------------------------------------------------------
(princ)
(prompt "\n		TT  : To calculate total value of selected text entities")
(defun c:TT ( / sset ssl no temp ed val totalval )
   (text_u_ssget)
   (print)
   (print)
   (setvar "DIMZIN" 0)
   (setq totalval 0.0)
   
   (setq ssl (sslength sset)
      no ssl
   )
   (while (> ssl 0)
      (progn
         (setq temp (ssname sset (setq ssl (1- ssl))))
         
         (setq ed (entget temp))
         (setq val 0.0)
         (setq val (read (DXF 1 ed)))
         (setq totalval (+ totalval val))
         
      );progn
   );while
   (if (/= no 0.0)
      (prompt (strcat "\nTotal value of " (rtos no 2 0) " numerical text entities : " (rtos totalval 2 4)))
   );if
   (princ)
);defun TT()
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(prompt "\n		TUP : Thay ®æi Text theo Tû lÖ hiÖn hµnh (^-^)")
(princ)
(defun c:TUP( / sset ssl temp ed old new )
   (text_u_ssget)
   (if (= scale nil) (setq scale (getreal "\nChän lùa tû lÖ : ")))
   (setq ssl (sslength sset))
   (while (> ssl 0)
      (progn
         (setq temp (ssname sset (setq ssl (1- ssl))))
         
         (setq ed (entget temp))
         (setq STY (DXF 7 ed))
         
         (if (= STY "5")
            (progn
               (setq old (cons 40 (DXF 40 ed))
                  size (* 3.5 scale)
                  new (cons 40 size)
                  ed (subst new old ed)
               )
            )
         )  

         (if (= STY "3")
            (progn
               (setq old (cons 40 (DXF 40 ed))
                  size (* 2.5 scale)
                  new (cons 40 size)
                  ed (subst new old ed)
               )
            )
         )  
         (if (= STY "2D")
            (progn
               (setq old (cons 40 (DXF 40 ed))
                  size (* 2.0 scale)
                  new (cons 40 size)
                  ed (subst new old ed)
               )
            )
         )  
         (if (= STY "2")
            (progn
               (setq old (cons 40 (DXF 40 ed))
                  size (* 1.6 scale)
                  new (cons 40 size)
                  ed (subst new old ed)
               )
            )
         )
         (entmod ed)  
         
      )
   )
   (princ)
);defun

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(princ "\n		TCC : To change layer of numeric TEXT entities")
(princ)

(defun c:TCC( / sset ssl temp ed old new )
   (textnum_u_ssget)
   (setq ssl (sslength sset))
   (if (> ssl 0) 
      (setq new_layer (getstring "New layer of the selected texts : "))
   )
   (while (> ssl 0)
      (progn
         (setq temp (ssname sset (setq ssl (1- ssl))))
         (setq ed (entget temp))
;         (setq val (read (DXF 1 ed)))
;         If (= (numberp val) T) then 
         (Progn         
            (setq old (cons 8 (DXF 8 ed))
               new (cons 8 new_layer)
               ed (subst new old ed)
            )
            (entmod ed)  
         )
      )  
   )
   
   (princ)
);defun
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun textnum_u_ssget ( / ssl  nsset temp ed )
   (setq sset (ssget))
   (setq ssl (sslength sset) 
      nsset (ssadd)
   )
   (print ssl)
   (princ "entities found. ")  
   (princ "\nVerifying the selected entities ... please wait. ")
   (while (> ssl 0)
      (progn
         (setq temp (ssname sset (setq ssl (1- ssl))))
         (setq ed (entget temp))
         (if (and (= (DXF 0 ed) "TEXT") (= (numberp (read (DXF 1 ed))) T)) (ssadd temp nsset))
      )
   )
   (setq ssl (sslength nsset)
      sset nsset
   )
   (print ssl)
   (princ "Numeric TEXT entities found. ")
   (princ)
   (print)
);defun u_ssget
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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

;-----------------------------------------------------------

(prompt "\n		CHNO: To +-*/ text entities with a number")
(defun c:CHNO ( / No temp ed old new acc fac operator ssl )
  (Text_get)
  (setq operator (getstring "\nInput operator (+ - * /):"))
  (setq fac (getreal "\nInput a number:"))
  (setq acc (getint "\nAccuracy:"))
  (setq DZ (getvar "DIMZIN"))
  (setvar "DIMZIN" 0)
  (setq ssl (sslength #sset))

  (while (> ssl 0)
    (progn
      (setq temp (ssname #sset (setq ssl (1- ssl))))
      (setq ed (entget temp))
      (setq old (cons 1 (DXF 1 ed)))

      (setq No (atof (DXF 1 ed)))
      (cond
         ((= operator "+") (setq No (+ No fac)))
         ((= operator "-") (setq No (- No fac)))
         ((= operator "*") (setq No (* No fac)))
         ((= operator "/") (setq No (/ No fac)))
      )
      (setq new (cons 1 (rtos No 2 acc)))
     
      (setq ed (subst new old ed))
      (entmod ed)  
    )
  )
(setvar "DIMZIN" DZ)
)
;-----------------------------------------------------------

(defun Cir_Text_get ( / ssl temp ed )
     (setq #sset (ssget))
     (setq ssl (sslength #sset) 
           #t_sset (ssadd)
           #c_sset (ssadd)
     )
     (print ssl)
     (princ "entities found. ")  
     (princ "\nVerifying the selected entities -- please wait. ")
     (while (> ssl 0)
          (progn
               (setq temp (ssname #sset (setq ssl (1- ssl))))
               (setq ed (entget temp))
               (if (= (DXF 0 ed) "TEXT") (ssadd temp #t_sset))
               (if (= (DXF 0 ed) "CIRCLE") (ssadd temp #c_sset))
          )
     )
     (setq t_ssl (sslength #t_sset)
           c_ssl (sslength #c_sset)
     )
     (print t_ssl)
     (princ "TEXT entities found. ")
     (print c_ssl)
     (princ "CIRTCLE entities found. ")
     (princ)
);defun Cir_Text_get

;-------------------------------------------------------------------

(prompt "\n		MJ  : Middle justify Text")
(defun c:MJ ( / code ssl)
     (Text_get)
     (setq pt (getpoint "\nInput Middle point"))
     (setq ssl (sslength #sset))
     
     (while (> ssl 0)
         (progn
               (setq temp (ssname #sset (setq ssl (1- ssl)))
                    ed (entget temp)

		    old (cons 72 (DXF 72 ed))
		    new (cons 72 1)
		    ed (subst new old ed)

		    old (cons 73 (DXF 73 ed))
		    new (cons 73 1)
		    ed (subst new old ed)


		    old (cons 10 (DXF 10 ed))
		    new (cons 10 (list (car pt) (cadr (dxf 10 ed)) 0.0)) 
		    ed (subst new old ed)

		    old (cons 11 (DXF 11 ed))
		    new (cons 11 (DXF 10 ed))
		    ed (subst new old ed)
		)
          (entmod ed)  
          )

     )
     (princ)
);defun
;-------------------------------------------------------------------

(defun Select_Sort( / ssl temp_name temp_y y_max name_max )

     (Text_get)

     (setq  Newsset (ssadd))
     (setq ssl (sslength #sset))

     (while (> ssl 0)
       (progn
         (setq ssl (sslength #sset))
         (setq temp_name (ssname #sset 0))
         (setq temp_y (cadr (dxf 10 (entget temp_name))))
         (setq y_max temp_y)
         (setq name_max temp_name)
         (prompt "\n") (princ ssl)
  
         (while (> ssl 1)
           (progn
              (setq temp_name (ssname #sset (setq ssl (1- ssl))))
	      (setq temp_y (cadr (dxf 10 (entget temp_name))))
	      (if (< temp_y y_max)
                (progn
                   (setq y_max temp_y)
	           (setq name_max temp_name)
	        )
	      );if
           );progn
         );while

         (ssadd name_max NewSset)

         (ssdel name_max #sset)
         (setq ssl (sslength #sset))
       
      );progn 
    ); while #sset
);defun Select_Sort

;---------------------
(prompt "\n		CTT : Lay SL cat ngang - TEXT")
(defun c:CTT ( / )

  (setq fn (getfiled "Input data file" "" "txt" 1))
  (setq f (open fn "w"))

  (setq TnName (car (entsel "\n\nPick CS Name")))
    
  (while TnName
     (progn
        (Select_Sort)
        (setq line (dxf 1 (entget TnName)))
	(setq ssln (sslength NewSset))
        (while (> ssln 0)
 	   (progn
              (setq temp (ssname Newsset (setq ssln (1- ssln))))
              (setq ed (entget temp))
              (setq value (dxf 1 ed))
	      (setq line (strcat line "\t" value))
	   );progn
        );while
	(write-line line f)
        (princ line)	  
	(setq TNname (car (entsel "\nPick SC Name")))     
    )
  );while TnName

  (close f) 
  
);defun
;-------------------------------------------------------------------
(prompt "\n		CE : ")
(DEFUN C:CE ( / t_ssl c_ssl )
	(Cir_Text_get)
	(setq t_ssl (sslength #t_sset)
              c_ssl (sslength #c_sset)
	      ssl t_ssl	
	)

	(while (> c_ssl 0)
    	(progn
      		(setq temp (ssname #c_sset (setq c_ssl (1- c_ssl))))
      		(setq ed (entget temp))
		(setq cen (DXF 10 ed))
	  	(setq radius (DXF 40 ed))

		(setq ssl t_ssl)
	  
		(while (> ssl 0)
    		(progn
      			(setq t_temp (ssname #t_sset (setq ssl (1- ssl))))
      			(setq ed1 (entget t_temp))
			(setq in_point (DXF 10 ed1))
		  	(setq dist (distance in_point cen))
		  	(if (< dist radius)
			(progn
				(setq 	old (cons 10 (DXF 10 ed1))
        	              		new (cons 10 cen)
	                      		ed1 (subst new old ed1)
					old (cons 72 (DXF 72 ed1))
        	              		new (cons 72 4)
	                      		ed1 (subst new old ed1)

					old (cons 11 (DXF 11 ed1))
        	              		new (cons 11 cen)
	                      		ed1 (subst new old ed1)
				)
	        		(entmod ed1)
			  	(ssdel t_temp #t_sset)
			  	(setq t_ssl (- t_ssl 1))
			);progn
			);if
		); progn
  		);while
	);progn
	);while
  ) ;defun

;==============================================================================
(prompt "\n		AST : Add a string to prefix or suffix of Text or Mtext")
(defun C:AST ( / #sset ssl ed No new temp old chon)
  (setq TextPre "" TextSuf "")
  (while (and (eq TextPre "") (eq TextSuf ""))
     (setq dcl_id (load_dialog "Text-Uti.dcl"))
     (if (< dcl_id 0) (exit))
     (if (not (new_dialog "Add_string_to_text" dcl_id "" dlg_post)) (exit))
     (action_tile "Pretxt"   "(setq TextPre $value)")
     (action_tile "Suftxt"   "(setq TextSuf $value)")
     (action_tile "ChapNhan"  "(done_dialog 1)")
     (action_tile "HuyBo"     "(done_dialog 2)")
     (setq dlgnum (start_dialog))
     (unload_dialog dcl_id)
     (if (= dlgnum 2) (exit))
  );while
  
  (text_u_ssget)
  (while (> ssl 0)
    (progn
      (setq temp (ssname sset (setq ssl (1- ssl)))
	    ed  (entget temp)
	    old (assoc 1 ed)
	    No  (DXF 1 ed)
      )
      (if (/= TextPre nil) (setq No (strcat TextPre No)) )
      (if (/= TextSuf nil) (setq No (strcat No TextSuf)) )
      (setq new (cons 1 No)
	    ed (subst new old ed)
      )
      (entmod ed)  
    );progn
  );while
  (princ)
);defun c:ast

(defun c:as() (c:ast))
;===========================================================================================
(princ "\n		WF  : Change width factor of Text")
(defun c:WF( / sset ssl temp ed wfac curwf)
  (command "_undo" "e" "_undo" "be")
  (text_u_ssget)
;  (setq ssl (sslength sset))
  (setq temp (ssname sset 0)
	ed (entget temp)
	curwf (dxf 41 ed)
  )
  (if (> ssl 0) (setq wfac (getreal (strcat "\nEnter width factor of text <" (rtos curwf 2 2) "> : "))) )
  (if (null wfac) (setq wfac curwf) )
  (while (> ssl 0)
    (setq temp (ssname sset (setq ssl (1- ssl)))
	  ed (entget temp)
	  ed (subst (cons 41 wfac) (assoc 41 ed) ed)
    )
    (entmod ed)  
  );while
  (command "_undo" "e")
  (princ)
);defun


