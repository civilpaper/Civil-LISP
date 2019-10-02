(prompt "\n   ARR : S¾p xÕp c¸c TEXT (Copy from Van Diep, Modified by  Lª TuÊn Anh) ")
(setq Phuong_ARR "D"
      Le_ARR "T"
      Kc_le 0
)

(defun X2T (elist)   
   (car (cadr (textbox elist )))
)
;----------------------------------------
(defun DXF (code elist)
   (cdr (assoc code elist))
)

;-------------------------------------------------------------
(defun ChonText (checksx / ssl nsset temp ed  edmin namemin)
   (setq sset (ssget))
   (setq ssl (sslength sset) 
	 nsset (ssadd)
   )
   (while (> ssl 0)
      (progn
         (setq temp (ssname sset (setq ssl (1- ssl))))
         (setq ed (entget temp))
         (if (= (DXF 0 ed) "TEXT")
	   (progn
	     (if (eq checksx 1)
	       (progn
		 (setq ed (subst (cons 72 0) (assoc 72 ed) ed))
		 (setq ed (subst (cons 73 0) (assoc 73 ed) ed))
		 (entmod ed)
	       )
	     )
	     (setq temp (cdr (assoc -1 ed)))
	     (ssadd temp nsset)
	   )
	 )
      )
   )
   (setq ssl (sslength nsset)
	 sset nsset
	 nsset (ssadd)
	 ssl (sslength sset)
   )
   (while (/= ssl 0)                 ;S¾p xÕp c¸c ®èi t­îng
      (setq temp (ssname sset 0))
      (setq namemin temp )
      (setq edmin (entget namemin))
      (while (> ssl 0)
         (progn
            (setq temp (ssname sset (setq ssl (1- ssl))))
            (setq ed (entget temp))
            (if (<= (cadr (DXF 10 ed)) (cadr (DXF 10 edmin )) )
               (progn
                  (setq namemin temp)
                  (setq edmin (entget namemin))          
               )
            ) 
         )
      )
      (ssadd namemin nsset)
      (ssdel namemin sset)
      (setq ssl (sslength sset))
   )
   (setq ssl (sslength nsset)
	 sset nsset
	 nsset (ssadd)
   )
);defun ChonText


;--------------------------S¾p xÕp ch÷ theo lÒ tr¸i ------------------------------
(defun ARR1 (check / sset ssl temp new old p ck)
   (ChonText 1)
   (setq p (getpoint "\nChän ®iÓm chuÈn bªn tr¸i TEXT : " ) )
   (if (eq Kc_le nil) (setq Kc_le 0))
   (if (eq check 1) (setq ck (getdist (strcat "\nNhËp kho¶ng c¸ch gi÷a c¸c dßng TEXT (" (rtos Kc_le) ") : " )) ))
   (if (eq ck nil)
       (setq ck Kc_le)
       (setq Kc_le ck)
   )
   (setq ssl (sslength sset))
   (while (> ssl 0)
      (progn
         (setq temp (ssname sset (setq ssl (1- ssl)))
	       ed   (entget temp)
	       old  (assoc 10 ed)
         )
	 (if (eq check 1) (setq new (cons 10 p))
			  (setq new (cons 10 (list (car p) (cadr (cdr old)))) )
	 )
         (setq ed (subst new old ed))
         (if (eq check 1) (setq p (list (car p) (- (cadr p) ck) 0.0 )) )
         (entmod ed)
      )
   )
   (princ)
);defun ARR1


;---------------------------S¾p xÕp ch÷ theo lÒ gi÷a ------------------------------
(defun ARR2 (check / sset ssl temp new old p ck lst)
   (ChonText 1)
   (setq p (getpoint "\nChän ®iÓm chuÈn gi÷a c¸c TEXT : " ) )
   (if (eq Kc_le nil) (setq Kc_le 0))
   (if (eq check 1) (setq ck (getdist (strcat "\nNhËp kho¶ng c¸ch gi÷a c¸c dßng TEXT (" (rtos Kc_le) ") : " )) ))
   (if (eq ck nil)
       (setq ck Kc_le)
       (setq Kc_le ck)
   )
   (setq ssl (sslength sset))
   (while (> ssl 0)
      (progn
         (setq temp (ssname sset (setq ssl (1- ssl)))
	       ed   (entget temp)
         )
	 (if (/= check 1) (setq new (assoc 10 ed)))
         (setq ed   (subst (cons 72 1) (assoc 72 ed) ed))
         (entmod ed)
	 (if (eq check 1)
	   (progn
	     (setq old  (assoc 11 ed))
	     (setq new (cons 11 p))
	   )
	   (progn
	     (setq old  (assoc 11 ed))
	     (setq new (cons 11 (list (car p) (cadr (cdr new)) )) )
	   )
	 )
         (setq ed (subst new old ed))
         (if (eq check 1) (setq p (list (car p) (- (cadr p) ck) 0.0 )) )
         (entmod ed)
      )
   )
   (princ)
);defun ARR2


;--------------------------- S¾p xÕp ch÷ theo lÒ ph¶i ------------------------------
(defun ARR3 (check / sset ssl temp new old p ck lst)
   (ChonText 1)
   (setq p (getpoint "\nCho ®iÓm chuÈn bªn ph¶i TEXT : " ) )
   (if (eq Kc_le nil) (setq Kc_le 0))
   (if (eq check 1) (setq ck (getdist (strcat "\nNhËp kho¶ng c¸ch gi÷a c¸c dßng TEXT (" (rtos Kc_le) ") : " )) ))
   (if (eq ck nil)
       (setq ck Kc_le)
       (setq Kc_le ck)
   )
   (setq ssl (sslength sset))       
   (while (> ssl 0)
      (progn 
         (setq temp (ssname sset (setq ssl (1- ssl)))
	       ed   (entget temp)
         )
	 (if (/= check 1) (setq new (assoc 10 ed)))
         (setq ed   (subst (cons 72 2) (assoc 72 ed) ed))
         (entmod ed)
	 (if (eq check 1)
	   (progn
	     (setq old  (assoc 11 ed))
	     (setq new (cons 11 p))
	   )
	   (progn
	     (setq old  (assoc 11 ed))
	     (setq new (cons 11 (list (car p) (cadr (cdr new)) )) )
	   )
	 )
         (setq ed (subst new old ed))
         (if (eq check 1) (setq p (list (car p) (- (cadr p) ck) 0.0 )) )
         (entmod ed)
      )
   )
   (princ)
);defun ARR3


;--------------------------- S¾p xÕp ch÷ theo ph­¬ng ngang ------------------------------
(defun PNGANG ( / sset ssl temp new old  lst)
   (ChonText 1)
   (setq p (getpoint "\nChän mét ®iÓm trªn ph­¬ng ngang: " ) )
   (setq ssl (sslength sset))       
   (while (> ssl 0)
      (progn 
         (setq temp (ssname sset (setq ssl (1- ssl)))
	       ed   (entget temp)
	       old  (assoc 10 ed)
	       p    (list (car (cdr old)) (cadr p))
	       new  (cons 10 p)
	       ed   (subst new old ed)
	 )
         (entmod ed)
      )
   )
   (princ)
);defun PNGANG

(defun PDUNG ( / kieu)
   (setq kieu (getstring (strcat "\nQuý ngµi thÝch s¾p xÕp theo bªn Tr¸i, Gi÷a, Ph¶i ? T/G/P (" Le_ARR ") : ")))
   (while (AND (/= (strcase kieu) "T") (/= (strcase kieu) "G") (/= (strcase kieu) "P") (/= kieu ""))
      (prompt "\nPh¶i chän T, G hoÆc lµ P chø ... !")
      (setq kieu (getstring (strcat "\nQuý ngµi muèn s¾p xÕp theo bªn Tr¸i, Gi÷a hay Ph¶i ? T/G/P (" Le_ARR ") : ")))
   )
   (if (= kieu "")
       (setq kieu Le_ARR)
       (setq Le_ARR (strcase kieu))
   )
   (cond
     ( (= (strcase kieu) "T") (ARR1 0) )
     ( (= (strcase kieu) "G") (ARR2 0) )
     ( (= (strcase kieu) "P") (ARR3 0) )
   )
);defun PDUNG


;;-------------------------------------------------------------
(defun LE ( / kieu)
   (setq kieu (getstring (strcat "\nS¾p xÕp theo Tr¸i, Gi÷a hay Ph¶i ? T/G/P (" Le_ARR ") : ")))
   (while (AND (/= (strcase kieu) "T") (/= (strcase kieu) "G") (/= (strcase kieu) "P") (/= kieu ""))
      (prompt "\nSai råi, lùa chän l¹i tõ kho¸ ®i... !")
      (setq kieu (getstring (strcat "\nS¾p xÕp theo bªn Tr¸i, Gi÷a hay Ph¶i ? T/G/P (" Le_ARR ") : ")))
   )
   (if (= kieu "")
       (setq kieu Le_ARR)
       (setq Le_ARR (strcase kieu))
   )
   (cond
     ( (= (strcase kieu) "T") (ARR1 1) )
     ( (= (strcase kieu) "G") (ARR2 1) )
     ( (= (strcase kieu) "P") (ARR3 1) )
   )
);defun LE


;;-------------------------------------------------------------
(defun c:ARR ( / loai str)
   (setq loai (getstring (strcat "\nS¾p xÕp theo Ph­¬ng Ngang, Ph­¬ng §øng hay LÒ ? N/D/L (" Phuong_ARR ") : ")))
   (while (AND (/= (strcase loai) "N") (/= (strcase loai) "D") (/= (strcase loai) "L") (/= loai ""))
      (prompt "\nPh¶i chän N, D hoÆc lµ L chø, chän l¹i ®i ... (^-^) ")
      (setq loai (getstring (strcat "\nS¾p xÕp theo Ph­¬ng Ngang, Ph­¬ng §øng hay LÒ ? N/D/L (" Phuong_ARR ") : ")))
   )
   (if (= loai "")
       (setq loai Phuong_ARR)
       (setq Phuong_ARR (strcase loai))
   )
   (cond
     ( (= (strcase loai) "N") (PNGANG) )
     ( (= (strcase loai) "D") (PDUNG) )
     ( (= (strcase loai) "L") (LE) )
   )
);defun ARR


;;-------------------------------------------------------------
(defun c:FT ( / sset ssl temp new old )   
   (ChonText 1)
   (setq ssl (sslength sset)
      ssl1 ssl  
   )     
   (setq temp (ssname sset (- ssl 1) )
      ed (entget temp)
      point (DXF 10 ed)  
   )   
   (setq newlength (getdist  point "\nNhËp chiÒu dµi míi cña TEXT  : " ) )     
   (while (> ssl 0)
      (progn 
         (setq temp (ssname sset (setq ssl (1- ssl)))
            ed (entget temp)
            oldlength (/ (X2T ed) (DXF 41 ed))
            width (/ newlength oldlength)
            old (cons 41 (DXF 41 ed))     
         )
         (if (or (<= width  0.8 ) (>= width 1.2 ))              
            (progn
               (setq new old)
               (print " ChiÒu dµi míi qu¸ dµi hay ng¾n : ")
               (princ)
               (princ (- ssl1 ssl))
            )
            (setq new (cons 41 width))            
         )
         (setq ed (subst new old ed))         
         (entmod ed)  
      )
   )
   (princ)
);defun FT

;;-----------------------------------------
(prompt "\n   CM  : Copy entities along one direction with same distance")
(defun c:CM (/ oldmode dt p1 p2 sl index kc goc)
  (setq dt (ssget)
	p1 (getpoint "\nFrom : ")
	p2 (getpoint p1 "\nTo : ")
  )

  (if #NumCM
    (setq sl (getint (strcat "\nQuantity <" (rtos #NumCM)  "> : ")))
    (while (not sl)
      (setq sl (getint "\nQuantity : "))
    )
  );if
  (if sl (setq #NumCM sl))
  (setq goc (angle p1 p2)
	kc (distance p1 p2)
	index 0
  )
  (command "_undo" "e" "_undo" "be")
  (setq oldmode (getvar "osmode"))
  (setvar "osmode" 0)
  (repeat #NumCM
    (setq index (1+ index))
    (command ".copy" dt "" p1 (polar p1 goc (* kc index)))
  )
  (setvar "osmode" oldmode)
  (command "_undo" "e")
  (princ)
);defun c:CM

;------------------------------------------------------------------
(prompt "\n   CMM : Copy entities along one direction with quantity")
(defun c:CMM (/ oldmode dt p1 p2 sl index kc goc)
  (setq dt (ssget)
	p1 (getpoint "\nFrom : ")
	p2 (getpoint p1 "\nTo : ")
  )

  (if #NumCM
    (setq sl (getint (strcat "\nQuantity <" (rtos #NumCM)  "> : ")))
    (while (not sl)
      (setq sl (getint "\nQuantity : "))
    )
  );if
  (if sl (setq #NumCM sl))
  (setq goc (angle p1 p2)
	kc (/ (distance p1 p2) #NumCM)
	index 0
  )
  (command "_undo" "e" "_undo" "be")
  (setq oldmode (getvar "osmode"))
  (setvar "osmode" 0)
  (repeat #NumCM
    (setq index (1+ index))
    (command ".copy" dt "" p1 (polar p1 goc (* kc index)))
  )
  (setvar "osmode" oldmode)
  (command "_undo" "e")
  (princ)
);defun c:CMM



