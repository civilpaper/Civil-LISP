(princ "\n\nL O A D I N G    T E X T _ U T I . L S P")


;============================================================================
(princ "\n		CT2 : Update TEXT to style \"2\"")
(defun c:CT2( / sset ssl temp ed old new )
  (text_u_ssget)
  (if (= scale nil) (c:TL))
  (if sset
    (progn
      (command "_undo" "e" "_undo" "be")
      (while (> ssl 0)
	(setq temp (ssname sset (setq ssl (1- ssl)))
	      ed (entget temp)
	      ed (subst (cons 7 "2") (assoc 7 ed) ed)
	      ed (subst (cons 40 (* T2_H scale)) (assoc 40 ed) ed)
	      ed (subst (cons 41 T2_W) (assoc 41 ed) ed)
        );setq
        (entmod ed)
      );while
      (command "_undo" "e")
    );progn
  );if
  (princ)
);defun CT2



;============================================================================
(defun c:CT22( / sset ssl temp ed old new )
  (text_u_ssget)
  (if (= scale nil) (c:TL))
  (if sset
    (progn
      (command "_undo" "e" "_undo" "be")
      (while (> ssl 0)
	(setq temp (ssname sset (setq ssl (1- ssl)))
	      ed (entget temp)
	      ed (subst (cons 7 "2") (assoc 7 ed) ed)
	      ed (subst (cons 8 "2") (assoc 8 ed) ed)
	      ed (subst (cons 40 (* T2_H scale)) (assoc 40 ed) ed)
	      ed (subst (cons 41 T2_W) (assoc 41 ed) ed)
        );setq
;	(if (/= (substr (dxf 1 ed) 1 3) "%%U")
;	  (setq ed (subst (cons 1 (strcat "%%U" (dxf 1 ed))) (assoc 1 ed) ed))
;	)
        (entmod ed)
      );while
      (command "_undo" "e")
    );progn
  );if
  (princ)
);defun CT22



;===========================================================================================
(princ "\n		CT2d: Update TEXT to style \"2d\"")
(defun c:CT2D( / sset ssl temp ed old new )
  (text_u_ssget)
  (if (= scale nil) (c:TL))
  (if sset
    (progn
      (command "_undo" "e" "_undo" "be")
      (while (> ssl 0)
	(setq temp (ssname sset (setq ssl (1- ssl)))
	      ed (entget temp)
	      ed (subst (cons 7 "2d") (assoc 7 ed) ed)
	      ed (subst (cons 8 "0") (assoc 8 ed) ed)
	      ed (subst (cons 40 (* T2_H scale)) (assoc 40 ed) ed)
	      ed (subst (cons 41 0.85) (assoc 41 ed) ed)
        );setq
;     (if (/= (substr (dxf 1 ed) 1 3) "%%U")
;       (setq ed (subst (cons 1 (strcat "%%U" (dxf 1 ed))) (assoc 1 ed) ed))
;     )
	(entmod ed)
      );while
      (command "_undo" "e")
    );progn
  );if
  (princ)
);defun CT2D



;===========================================================================================
(princ "\n		CT3 : Update TEXT to style \"3\"")
(defun c:CT3( / sset ssl temp ed old new )
  (text_u_ssget)
  (if (= scale nil) (c:TL))
  (if sset
    (progn
      (command "_undo" "e" "_undo" "be")
      (while (> ssl 0)
	(setq temp (ssname sset (setq ssl (1- ssl))))
;	(command "_justifytext" temp "" "mc")
	(setq ed (entget temp)
	      ed (subst (cons 7 "3") (assoc 7 ed) ed)
	      ed (subst (cons 8 "0") (assoc 8 ed) ed)
	      ed (subst (cons 40 (* T3_H scale)) (assoc 40 ed) ed)
	      ed (subst (cons 41 T3_W) (assoc 41 ed) ed)
;	      ed (subst (cons 72 1) (assoc 72 ed) ed)
	);setq
;     (if (/= (substr (vl-string-left-trim " " (dxf 1 ed)) 1 3) "%%U")
;       (setq ed (subst (cons 1 (strcat "%%U" (dxf 1 ed))) (assoc 1 ed) ed))
;     )
	(entmod ed)
      );while
      (command "_undo" "e")
    );progn
  );if  
  (princ)
);defun CT3



;===========================================================================================
(princ "\n		CT5 : Update TEXT to style \"5\"")
(defun c:CT5( / sset ssl temp ed old new )
  (text_u_ssget)
  (if (= scale nil) (c:TL))
  (if sset
    (progn
      (command "_undo" "e" "_undo" "be")
      (while (> ssl 0)
	(setq temp (ssname sset (setq ssl (1- ssl))))
;	(command "_justifytext" temp "" "mc")
	(setq ed (entget temp)
	      ed (subst (cons 7 "5") (assoc 7 ed) ed)
	      ed (subst (cons 8 "0") (assoc 8 ed) ed)
	      ed (subst (cons 40 (* T5_H scale)) (assoc 40 ed) ed)
	      ed (subst (cons 41 T5_W) (assoc 41 ed) ed)
;	      ed (subst (cons 72 1) (assoc 72 ed) ed)
        );setq
;     (if (/= (substr (vl-string-left-trim " " (dxf 1 ed)) 1 3) "%%U")
;       (setq ed (subst (cons 1 (strcat "%%U" (dxf 1 ed))) (assoc 1 ed) ed))
;     )
	(entmod ed)
      );while
      (command "_undo" "e")
    );progn
  );if
  (princ)
);defun CT5



;===========================================================================================
(princ "\n		TUP : Update TEXT to current scale")
(defun c:TUP( / sset ssl temp ed old new )
  (command "_undo" "e" "_undo" "be")
  (text_u_ssget)
  (if (= scale nil) (c:TL))
;  (setq ssl (sslength sset))
  (while (> ssl 0)
    (setq temp (ssname sset (setq ssl (1- ssl)))
	  ed (entget temp)
	  STY (DXF 7 ed)
    )
    (cond
      ((= STY "5")
;       (command "_justifytext" temp "" "mc")
       (setq ed (subst (cons 40 (* T5_H scale)) (assoc 40 ed) ed)
	     ed (subst (cons 41 T5_W) (assoc 41 ed) ed)
	     ed (subst (cons 8 "0") (assoc 8 ed) ed)
       )
;       (if (/= (substr (vl-string-left-trim " " (dxf 1 ed)) 1 3) "%%U")
;	 (setq ed (subst (cons 1 (strcat "%%U" (dxf 1 ed))) (assoc 1 ed) ed))
;       )
      );"5"
      ((= STY "3")
;       (command "_justifytext" temp "" "mc")
       (setq ed (subst (cons 40 (* T3_H scale)) (assoc 40 ed) ed)
	     ed (subst (cons 41 T3_W) (assoc 41 ed) ed)
	     ed (subst (cons 8 "0") (assoc 8 ed) ed)
       )
;       (if (/= (substr (vl-string-left-trim " " (dxf 1 ed)) 1 3) "%%U")
;	 (setq ed (subst (cons 1 (strcat "%%U" (dxf 1 ed))) (assoc 1 ed) ed))
;       )
      );"3"
      ((= STY "2d")
       (setq ed (subst (cons 40 (* T2_H scale)) (assoc 40 ed) ed)
	     ed (subst (cons 41 0.85) (assoc 41 ed) ed)
	     ed (subst (cons 8 "0") (assoc 8 ed) ed)
       )
      );"2d"
      ((= STY "2")
       (setq ed (subst (cons 40 (* T2_H scale)) (assoc 40 ed) ed)
	     ed (subst (cons 41 T2_W) (assoc 41 ed) ed)
;	     ed (subst (cons 8 "2") (assoc 8 ed) ed)
       )
      );"2"
    );cond
    (entmod ed)  
   );while
   (command "_undo" "e")
   (princ)
);defun c:TUP



;===========================================================================================
(princ "\n		TC  : Change layer of TEXT entities")
(defun c:TC( / sset ssl temp ed old new )
   (command "_undo" "e" "_undo" "be")
   (text_u_ssget)
;   (setq ssl (sslength sset))
   (if (> ssl 0) 
      (setq new_layer (getstring "\nNew layer of the selected text : "))
   )
   (while (> ssl 0)
     (setq temp (ssname sset (setq ssl (1- ssl))))
     (setq ed (entget temp))
     (setq old (cons 8 (DXF 8 ed))
	   new (cons 8 new_layer)
           ed (subst new old ed)
     )
     (entmod ed)  
   );while
   (command "_undo" "e")
   (princ)
);defun



;============================================================================
(prompt "\n		T2  : Change style-\"2\"-Text to layer \"2\"")
(defun c:T2( / sset ssl temp ed old new )
   (command "_undo" "e" "_undo" "be")
   (text_u_ssget)
;   (setq ssl (sslength sset))
   (while (> ssl 0)
      (progn
         (setq temp (ssname sset (setq ssl (1- ssl)))
               ed (entget temp)
	 )
         (if (eq (dxf 7 ed) "2")
	   (setq old (assoc 8 ed)
	       new (cons 8 "2")
	       ed (subst new old ed)
           )
	 )
         (entmod ed)  
      )  
   )
   (command "_undo" "e")
   (princ)
);defun



;===========================================================================================
(princ "\n		TO  : Oblique or straighten up TEXT entities")
(defun c:TO( / sset ssl temp ed old new )
   (command "_undo" "e" "_undo" "be")
   (text_u_ssget)
;   (setq ssl (sslength sset))
   (if (> ssl 0) 
      (setq ob_angle (getreal "\nNew oblique angle of the selected text : "))
   )
   (while (> ssl 0)
     (setq temp (ssname sset (setq ssl (1- ssl)))
	   ed (entget temp)
	   ed (subst (cons 51 (tangent (rad ob_angle))) (assoc 51 ed) ed)
     )
     (entmod ed)  
   );while
   (command "_undo" "e")
   (princ)
);defun



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



;====================================================================================
(princ "\n		CA  : Switch between UPCASE and LOWCASE of TEXT entities")
(setq Chon_CA "L")
(defun c:CA( / sset ssl temp ed old new str)
(if (eq (substr (ver) 1 16) "Visual LISP 2004")
 (progn
   (command "_undo" "e" "_undo" "be")
   (text_u_ssget)
;   (setq ssl (sslength sset))
   (initget "U u L l")
   (if (> ssl 0) (setq sw (getkword (strcat "\nNew case of the selected text [U/L] <" Chon_CA "> : "))))
   (if (null sw)
     (setq sw Chon_CA)
     (setq Chon_CA (strcase sw))
   );if
   
   (while (> ssl 0)
     (if (or (= sw "U") (= sw "u"))
       (progn
	 (setq temp (ssname sset (setq ssl (1- ssl)))
	       ed (entget temp)
	       ed (subst (cons 1 (strcase (DXF 1 ed))) (assoc 1 ed) ed)
         )
	 (entmod ed)  
       );progn  
     );if
     (if (or (= sw "l") (= sw "L"))
       (progn
	 (setq temp (ssname sset (setq ssl (1- ssl)))
	       ed (entget temp)
	       str (DXF 1 ed)
	       str1 (vl-string-left-trim " " str)
	       Char_pos (- (strlen str) (strlen str1))
	 )
	 (if (or (= (substr str1 1 1) "_") (= (substr str1 1 1) "-") )
	   (setq str2 (vl-string-left-trim " " (substr str1 2 (- (strlen str1) 2)) )
		 Char_pos (+ (- Char_pos 1) (- (strlen str1) (strlen str2)) )
	   )
	 );if
	 (setq ed (subst (cons 1 (strcat (substr str 1 (+ Char_pos 1)) (strcase (substr str (+ Char_pos 2) (- (strlen str) Char_pos)) T))) (assoc 1 ed) ed)
	       ;ed (subst (cons 1 (strcase (DXF 1 ed) T)) (assoc 1 ed) ed)
	 )
	 (entmod ed)
       );progn
     );if
   );while
   (command "_undo" "e")
 );progn
 (c:Tcase)
);if
(princ)
);defun CA



;----------------------------------------------------------
(prompt "\n   CDT : Update a text with elevation of a point")
(defun c:CDT ( / DZ pt y ptside ang OT)
  (if (= scale nil)  (setq scale (getreal "\nInput current scale: ")))
  (if (setq #Bacc (getint (strcat"\nInput accuracy <" (getvar "LUPREC") "> : ")))
    (setvar "LUPREC" #Bacc)
  )
  (setq pt (getpoint "\nSpecify a point to determine elevation : "))
  (setq y (/ (cadr pt) 100.0))
  (cond
    ((> y 0) (setq y (rtos y 2 #Bacc)))
    ((< y 0) (setq y (rtos y 2 #Bacc)))
    ((= y 0) (setq y "%%p0.00"))
  )
  (setq #sset (entsel "\nChoose a text : ")
	ed (entget (car #sset))
	old (assoc 1 ed)
	new (cons 1 y)
	ed (subst new old ed)
  )
  (entmod ed)  
  (princ)
);defun c:CDT



;===========================================================================================
(prompt "\n		ET  : Erase entities at same position in horizontal alignment drawings")
(defun c:ET ( / pt1 pt2 pt3 Dist i Num sset ssl Direction OldVC OldVS)
  (defun LocalError (msg)			; defines function LocalError
    (command "_undo" "e")
    (setq *error* existError)		; restores *error* function
    (princ errormsg)
    (princ)
  )
  (setq existError *error*		; stores current error function
	*error* LocalError		; resets *error* to LocalError
	errormsg "\nQuiting command."
	OldVC (getvar "VIEWCTR")
	OldVS (getvar "VIEWSIZE")
  )
  (command "_undo" "e" "_undo" "be")
  (initget (+ 1 2 4))
  (setq pt1 (getpoint "\nSpecify range enclosing entities to erase : "))
  (initget (+ 1 2 4))
  (setq pt2 (getcorner pt1 "\nSpecify other corner point : "))

  (setq Direction "Diep")
  (while (or (and (null Direction_Save) (null Direction)) (null (wcmatch Direction "v,V,h,H,")) )
    (if (null Direction_Save)
      (setq Direction (strcase (getstring "\nDirection (Vertical/Horizontal) : ")))
      (setq Direction (strcase (getstring (strcat "\nDirection (Vertical/Horizontal) <" Direction_Save "> : " ))) )
    )
  );while
  (if (eq Direction "")
    (setq Direction Direction_Save)
    (if (wcmatch Direction "v,V,h,H") (setq Direction_Save Direction))
  )
    
  (setq Dist 0)
  (while (or (eq Dist 0) (and (null Dist_Save) (null Dist)))
    (if (null Dist_Save)
      (setq Dist (getdist "\nSpecify distance between consecutive drawings : "))
      (setq Dist (getdist (strcat "\nSpecify distance between consecutive drawings <" (rtos Dist_Save 2 2) "> : " )))
    )
    (if (and (null Dist) (/= Dist_Save nil) )
      (setq Dist Dist_Save)
      (if (/= Dist 0) (setq Dist_Save Dist))
    )
  );while
  
  (setq Num nil)
  (while (or (<= Num 0) (null Num))
    (if (null Num_Save)
      (setq Num (getint "\nEnter number of drawings : "))
      (setq Num (getdist (strcat "\nEnter number of drawings <" (rtos Num_Save 2 2) "> : " )))
    )
    (if (and (null Num) (/= Num_Save nil) )
      (setq Num Num_Save)
      (if (> Num 0) (setq Num_Save Num))
    )
  );while

;  (if (eq Direction "H")
;    (setq pt3 (list (+ (car pt2) (* (1- Num) Dist)) (cadr pt2) ))
;    (setq pt3 (list (car pt2) (+ (cadr pt2) (* (1- Num) Dist)) ))
;  )
;  (command "_zoom" "w" pt1 pt3)

  (setq sset (ssget "_W"  pt1 pt2)
	ssl  (sslength sset)
  )
  (while (> ssl 0)
    (setq temp (ssname sset (setq ssl (1- ssl))) )
    (entdel temp)
  )
  (setq errormsg "\nDone !"
	i 1
  )
  (while (< i Num)
    (if (eq Direction "H")
      (setq pt1 (list (+ (car pt1) Dist) (cadr pt1) )
	    pt2 (list (+ (car pt2) Dist) (cadr pt2) )
      )
      (setq pt1 (list (car pt1) (+ (cadr pt1) Dist) )
	    pt2 (list (car pt2) (+ (cadr pt2) Dist) )
      )
    )
    (command "_zoom" "w" pt1 pt2)
    (setq sset (ssget "_W"  pt1 pt2)
          ssl  (sslength sset)
	  i (1+ i)
    )
    (while (> ssl 0)
      (setq temp (ssname sset (setq ssl (1- ssl))) )
      (entdel temp)
    )
  );while
  (command "_undo" "e")
  (setq *error* existError)		; restores prior *error* function
  (command "_zoom" "c" OldVC OldVS)
  (princ)
);defun c:ET



;=========================================================================
(prompt "\n		RT  : Rotate texts with the same angle")
(defun c:RT ( / ssl ed No new temp old ang)
  (text_u_ssget)
  (if #AngRT
    (setq ang (getreal (strcat "\nInput angle (degrees) <" (rtos #AngRT) "> : ")))
    (while (not ang) (setq ang (getreal "\nInput angle (degrees) : ")))
  )
  (if ang (setq #AngRT ang))
  (setq ang (/ (* #AngRT pi) 180))
  (setq ssl (sslength sset))
  (command "_justifytext" sset "" "MC")
  (setq sset (entlast))
  (while (> ssl 0)
    (progn
      (setq temp (ssname sset (setq ssl (1- ssl)))
	    ed (entget temp)
	    old (assoc 50 ed)
	    OldAng (cdr old)
	    NewAng (+ OldAng ang)
      )
      (if (>= NewAng (* 2 pi)) (setq NewAng (- NewAng (* 2 pi))) )
      (if (<  NewAng 0       ) (setq NewAng (+ NewAng (* 2 pi))) )

      (setq new (cons 50 NewAng))
      (setq ed (subst new old ed))
      (entmod ed)  
    );progn
  );while
  (princ)
);c:RT



;-------------------------------------------------------------------
;(prompt "\n		ATT : Add a string at the beginning of Text(s)")
(defun c:att ( / ssl ed No new temp old chon)
  (text_u_ssget)
  (setq chon (car (entsel "\nChoose a text containing prefix-string : ")))
  (while (/= "TEXT" (cdr (assoc 0 (entget chon))))
    (setq chon (car (entsel "\nPlease choose a text ...")))
  )
  (setq str (cdr (assoc 1 (entget chon))))
  (setq ssl (sslength sset))

  (while (> ssl 0)
    (progn
      (setq temp (ssname sset (setq ssl (1- ssl))))
      (setq ed (entget temp))
      (setq old (cons 1 (DXF 1 ed)))
      (setq No (DXF 1 ed))
      (setq No (strcat str No))
      (setq new (cons 1 No))
      (setq ed (subst new old ed))
      (entmod ed)  
    )
  )
  (princ)
);defun c:att



;-------------------------------------------------------------------
;(prompt "\n		ATS : Add a string at the end of Text(s)")
(defun c:ats ( / ssl ed No new temp old chon )
  (text_u_ssget)
  (setq chon (car (entsel "\nChoose a text containing suffix-string : ")))
  (while (/= "TEXT" (cdr (assoc 0 (entget chon))))
    (setq chon (car (entsel "\nPlease choose a text ...")))
  )
  (setq str (cdr (assoc 1 (entget chon))))
  (setq ssl (sslength sset))

  (while (> ssl 0)
    (progn
      (setq temp (ssname sset (setq ssl (1- ssl))))
      (setq ed (entget temp))
      (setq old (cons 1 (DXF 1 ed)))
      (setq No (DXF 1 ed))
      (setq No (strcat No str))
      (setq new (cons 1 No))
      (setq ed (subst new old ed))
      (entmod ed)  
    )
  )
);defun c:ats



;------------------------------------------------------------------------------------
(prompt "\n		REP : Replace content of text(s) to the same new one")
(defun c:REP ( / ssl ed No new temp old )
  (text_u_ssget)
  (if (or (null New_str) (= New_str ""))
    (setq str (getstring "\nInput new content : "))
    (setq str (getstring (strcat "\nInput new content <\"" New_str "\"> : ")))
  );if
  (if (/= str "") (setq New_str str))
  (setq ssl (sslength sset))
  (while (> ssl 0)
    (progn
      (setq temp (ssname sset (setq ssl (1- ssl)))
	    ed (entget temp)
	    old (assoc 1 ed)
	    new (cons 1 New_str)
	    ed (subst new old ed)
      )
      (entmod ed)  
    );progn
  );while
  (princ)
);defun c:rep



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
