; MODULE_ID ACAD2007_LSP_
;;;    ACAD2007.LSP Version 1.0 for AutoCAD 2007
;;;
;;;    Copyright (C) 1994-2006 by Autodesk, Inc.
;;;
;;;    Permission to use, copy, modify, and distribute this software
;;;    for any purpose and without fee is hereby granted, provided
;;;    that the above copyright notice appears in all copies and
;;;    that both that copyright notice and the limited warranty and
;;;    restricted rights notice below appear in all supporting
;;;    documentation.
;;;
;;;    AUTODESK PROVIDES THIS PROGRAM "AS IS" AND WITH ALL FAULTS.
;;;    AUTODESK SPECIFICALLY DISCLAIMS ANY IMPLIED WARRANTY OF
;;;    MERCHANTABILITY OR FITNESS FOR A PARTICULAR USE.  AUTODESK, INC.
;;;    DOES NOT WARRANT THAT THE OPERATION OF THE PROGRAM WILL BE
;;;    UNINTERRUPTED OR ERROR FREE.
;;;
;;;    Use, duplication, or disclosure by the U.S. Government is subject to
;;;    restrictions set forth in FAR 52.227-19 (Commercial Computer
;;;    Software - Restricted Rights) and DFAR 252.227-7013(c)(1)(ii) 
;;;    (Rights in Technical Data and Computer Software), as applicable.
;;;
;;;.
;;;
;;;    Note:
;;;            This file is normally loaded only once per AutoCAD session.
;;;            If you wish to have LISP code loaded into every document,
;;;            you should add your code to acaddoc.lsp.

;;;
;;;    Globalization Note:   
;;;            We do not support autoloading applications by the native 
;;;            language command call (e.g. with the leading underscore
;;;            mechanism.)

(if (not (=  (substr (ver) 1 11) "Visual LISP")) (load "acad2007doc.lsp"))

;; Silent load.
(princ)

;***************************************************************
;*		COLLECTED AND MODIFYED BY THE REAPER           *
;*			CTCTDS - XNTVTKCT		       *
;*		        NOVEMBER  12 2004		       *
;***************************************************************


(defun RAD (degree)
  (setq radian (/ (* degree 3.141592654) 180))
  radian
)

(defun DEG (Rad)
  (setq degree (/ (* rad 180) 3.141592654))
  degree
)

(defun tangent (Rad)
  (setq tang (/ (sin rad) (cos rad)))
  tang
)
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




(defun c:ss( )
 (command "layer" "new" "1" "c" "1" "1" "")
 (command "layer" "new" "2" "c" "2" "2" "")
 (command "layer" "new" "3" "c" "3" "3" "")
 (command "layer" "new" "4" "c" "4" "4" "")
 (command "layer" "new" "2" "c" "2" "2" "")
 (command "layer" "new" "5" "c" "1" "5" "")
 (command "layer" "new" "8" "c" "8" "8" "")
 (command "layer" "c" "7" "0" "")
 (command "style" "IN" ".VnArialH" "" "" "" "" "")
 (command "style" "IN1" ".VnArial NarrowH" "" "" "" "" "")
 (command "style" "Thuong" ".VnArial Narrow" "" "" "" "" "")
)

(defun c:1 (/ a) (setq a (ssget))(command "change" a "" "p" "la" "1" ""))
(defun c:2 (/ a) (setq a (ssget))(command "change" a "" "p" "la" "2" ""))
(defun c:3 (/ a) (setq a (ssget))(command "change" a "" "p" "la" "3" ""))
(defun c:4 (/ a) (setq a (ssget))(command "change" a "" "p" "la" "4" ""))
(defun c:5 (/ a) (setq a (ssget))(command "change" a "" "p" "la" "5" ""))
(defun c:6 (/ a) (setq a (ssget))(command "change" a "" "p" "la" "6" ""))
(defun c:7 (/ a) (setq a (ssget))(command "change" a "" "p" "la" "7" ""))
(defun c:8 (/ a) (setq a (ssget))(command "change" a "" "p" "la" "8" ""))
(defun c:9 (/ a) (setq a (ssget))(command "change" a "" "p" "la" "9" ""))
(defun c:11 (/ a) (setq a (ssget))(command "change" a "" "p" "la" "0" ""))
(defun c:a () (command ".line" ))
(defun c:ac() (command ".arc"))
(defun c:c () (command ".copy"))
(defun c:c () (command ".copy"))
(defun c:cc () (command ".circle"))
(defun c:ci () (command ".circle"))
(defun c:du () (command "dim1" "update"))
(defun c:dx () (command ".dimaligned"))
(defun c:dc () (command "dim1" "continue"))
(defun c:dr () (command ".dimradius"))
(defun c:df () (command ".dimlinear"))
(defun c:dd () (command ".dimlinear"))
(defun c:dn () (command ".dim1" "n"))
(defun c:n () (command ".dim1" "n"))
(defun c:de () (command "dim1" "l"))
(defun c:da () (command ".dimangular"))
(defun c:a () (command ".line"))
(defun c:g () (command ".matchprop"))
(defun c:q () (command ".offset"))
(defun c:s () (command ".stretch"))
(defun c:t () (command ".text"))
(defun c:v () (command ".move"))
(defun c:m () (command ".move"))
(defun c:x () (command ".explode"))
(defun c:z () (command ".zoom"))
(defun c:zz () (command ".zoom" "p"))
(defun c:ze () (command ".zoom" "e"))  
(defun c:zd () (command ".zoom" "d"))  
(defun c:za () (command ".zoom" "all"))
(defun c:ed () (command ".ddedit"))
(defun c:ff (/ gp) (setq gp (ssget))(command "mirror" gp "" pause pause "n"))
(defun c:fff (/ gp) (setq gp (ssget))(command "mirror" gp "" pause pause "y"))
(defun c:d () (command ".dist" ))

;-------------------------------------------Cat dim-----------------------------------
(DEFUN C:cd (/ CMD SS LTH DEM PT DS KDL N70 GOCX GOCY PT13 PT14 PTI PT13I PT14I
                PT13N PT14N O13 O14 N13 N14 OSM OLDERR PT10 PT11)
(SETQ CMD (GETVAR "CMDECHO"))
(SETQ OSM (GETVAR "OSMODE"))
(SETQ OLDERR *error*
      *error* myerror)
(PRINC "Choose dimesions:")
(SETQ SS (SSGET))
(SETVAR "CMDECHO" 0)
(SETQ PT (GETPOINT "Select point: "))
(SETQ PT (TRANS PT 1 0))
(COMMAND "UCS" "W")
(SETQ LTH (SSLENGTH SS))
(SETQ DEM 0)
(WHILE (< DEM LTH)
    (PROGN
	(SETQ DS (ENTGET (SSNAME SS DEM)))
	(SETQ KDL (CDR (ASSOC 0 DS)))
	(IF (= "DIMENSION" KDL)
	   (PROGN
		(SETQ PT10 (CDR (ASSOC 10 DS)))
		(SETQ PT11 (CDR (ASSOC 11 DS)))
		(SETQ PT13 (CDR (ASSOC 13 DS)))
		(SETQ PT14 (CDR (ASSOC 14 DS)))
		(SETQ N70 (CDR (ASSOC 70 DS)))
		(IF (OR (= N70 32) (= N70 33) (= N70 160) (= N70 161))
		   (PROGN
			(SETQ GOCY (ANGLE PT10 PT14))
			(SETQ GOCX (+ GOCY (/ PI 2)))
		   )
		)
		(SETVAR "OSMODE" 0)
		(SETQ PTI (POLAR PT GOCX 2))
		(SETQ PT13I (POLAR PT13 GOCY 2))
		(SETQ PT14I (POLAR PT14 GOCY 2))
		(SETQ PT13N (INTERS PT PTI PT13 PT13I NIL))
		(SETQ PT14N (INTERS PT PTI PT14 PT14I NIL))
		(SETQ O13 (ASSOC 13 DS))
		(SETQ O14 (ASSOC 14 DS))
		(SETQ N13 (CONS 13 PT13N))
		(SETQ N14 (CONS 14 PT14N))
		(SETQ DS (SUBST N13 O13 DS))
		(SETQ DS (SUBST N14 O14 DS))
		(ENTMOD DS)
	   )
	)
	(SETQ DEM (+ DEM 1))
    )
)
(COMMAND "UCS" "P")
(SETVAR "CMDECHO" CMD)
(SETVAR "OSMODE" OSM)
(setq *error* OLDERR)               ; Restore old *error* handler
(PRINC)
)

;--------------------------------------dim possition------------------------------------
(DEFUN C:bd (/ CMD SS LTH DEM PT DS KDL N70 GOCX GOCY PT13 PT14 PTI
                PT10 PT10I PT10N O10 N10 PT11 PT11N O11 N11 KC OSM OLDERR)
(SETQ CMD (GETVAR "CMDECHO"))
(SETQ OSM (GETVAR "OSMODE"))
(SETQ OLDERR *error*
      *error* myerror)
(PRINC "Choose dimesions:")
(SETQ SS (SSGET))
(SETVAR "CMDECHO" 0)
(SETQ PT (GETPOINT "Select point: "))
(SETQ PT (TRANS PT 1 0))
(COMMAND "UCS" "W")
(SETQ LTH (SSLENGTH SS))
(SETQ DEM 0)
(WHILE (< DEM LTH)
    (PROGN
	(SETQ DS (ENTGET (SSNAME SS DEM)))
	(SETQ KDL (CDR (ASSOC 0 DS)))
	(IF (= "DIMENSION" KDL)
	   (PROGN
		(SETQ PT13 (CDR (ASSOC 13 DS)))
		(SETQ PT14 (CDR (ASSOC 14 DS)))
		(SETQ PT10 (CDR (ASSOC 10 DS)))
		(SETQ PT11 (CDR (ASSOC 11 DS)))
		(SETQ N70 (CDR (ASSOC 70 DS)))
		(IF (OR (= N70 32) (= N70 33) (= N70 160) (= N70 161))
		   (PROGN
			(SETQ GOCY (ANGLE PT10 PT14))
			(SETQ GOCX (+ GOCY (/ PI 2)))
		   )
		)
		(SETVAR "OSMODE" 0)
		(SETQ PTI (POLAR PT GOCX 2))
		(SETQ PT10I (POLAR PT10 GOCY 2))
		(SETQ PT10N (INTERS PT PTI PT10 PT10I NIL))
		(SETQ KC (DISTANCE PT10 PT10N))
		(SETQ O10 (ASSOC 10 DS))
		(SETQ N10 (CONS 10 PT10N))
		(SETQ DS (SUBST N10 O10 DS))
		(SETQ PT11N (POLAR PT11 (ANGLE PT10 PT10N) KC))
		(SETQ O11 (ASSOC 11 DS))
		(SETQ N11 (CONS 11 PT11N))
		(SETQ DS (SUBST N11 O11 DS))
		(ENTMOD DS)
	   )
	)
	(SETQ DEM (+ DEM 1))
    )
)
(COMMAND "UCS" "P")
(SETVAR "CMDECHO" CMD)
(SETVAR "OSMODE" OSM)
(setq *error* OLDERR)
(PRINC)
)

(DEFUN C:OO () (COMMAND "OSNAP" "CEN,END,INT,INS,MID,NOD,NEAR,NONE,PERP,QUAD"))

;--------------------------------------Can chinh chu theo phuong dung ben phai-----------------------------------
(defun c:hp ( / sset ssl temp ed old new )
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
         (setq 
             old2(nth (xdtt 10 ed) ed)
             old1 (nth (xdtt 72 ed) ed)
             new1 (cons 72 2)
             ed (subst new1 old1 ed)
                     old3 (nth (xdtt 73 ed) ed)
                     new3 (cons 73 0)
                     ed (subst new3 old3 ed)
             old (nth (xdtt 11 ed) ed)
             new (list (nth 0 old) (car pt1) (nth 2 old2) (nth 3 old))
             ed (subst new old ed)
             ed (subst new1 old1 ed)
                     
          )
      (entmod ed)  
      )  
  )

;;;MTEXT
      (if (= (DXF 0 ed) "MTEXT")
      (progn
         (setq 
             old1 (nth (xdtt 71 ed) ed)
             new1 (cons 71 3)
             ed (subst new1 old1 ed)           
             old (nth (xdtt 10 ed) ed)
             new (list (nth 0 old) (car pt1) (nth 2 old) (nth 3 old))
             ed (subst new old ed)                            
          )
      (entmod ed)  
      )  
  )      
 )
)


(setq pt1 nil)
 (princ)
);defun
;--------------------------------------Can chinh chu theo phuong dung ben trai-----------------------------------
(defun c:ht( / sset ssl temp ed old new )
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
     (if  (= (DXF 0 ed) "TEXT")
      (progn
       (setq 
             old1 (nth (xdtt 72 ed) ed)
             new1 (cons 72 0)
             old (nth (xdtt 10 ed) ed)
                     old3 (nth (xdtt 73 ed) ed)
                     new3 (cons 73 0)
                     ed (subst new3 old3 ed)
             new (list (nth 0 old) (car pt1) (nth 2 old) (nth 3 old))
             ed (subst new old ed)
             ed (subst new1 old1 ed)   
           )
      (entmod ed)  
      )  
  )

;;;MTEXT
      (if (= (DXF 0 ed) "MTEXT")
      (progn
         (setq 
             old1 (nth (xdtt 71 ed) ed)
             new1 (cons 71 1)
             ed (subst new1 old1 ed)           
             old (nth (xdtt 10 ed) ed)
             new (list (nth 0 old) (car pt1) (nth 2 old) (nth 3 old))
             ed (subst new old ed)                            
          )
       (entmod ed)  
      )  
  )   
)
)
(setq pt1 nil)
 (princ)
);defun

;--------------------------------------Can chinh chu theo phuong ngang----------------------------------
(defun c:vt( / sset ssl temp ed old new )
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
     (if  (= (DXF 0 ed) "TEXT")
      (progn
       (setq 
             old1 (nth (xdtt 72 ed) ed)
             new1 (cons 72 0)
             old (nth (xdtt 10 ed) ed)
                     old3 (nth (xdtt 73 ed) ed)
                     new3 (cons 73 0)
                     ed (subst new3 old3 ed)
             new (list (nth 0 old) (nth 1 old) (cadr pt1) (nth 3 old))
             ed (subst new old ed)
             ed (subst new1 old1 ed)   
           )
      (entmod ed)  
      )  
  )
;;;MTEXT
      (if (= (DXF 0 ed) "MTEXT")
      (progn
         (setq 
             old1 (nth (xdtt 71 ed) ed)
             new1 (cons 71 7)
             ed (subst new1 old1 ed)           
             old (nth (xdtt 10 ed) ed)
             new (list (nth 0 old) (nth 1 old) (cadr pt1) (nth 3 old))
             ed (subst new old ed)                            
          )
       (entmod ed)  
      );progn  
      );if
 );while
);if

(setq pt1 nil)
 (princ)
);defun


;--------------------------------------Tao chu in nghieng cho text-------------------------------------------------

(defun c:TN( / sset ssl temp ed old new )
(text_uu_ssget)
(setq ssl (sslength sset))
(if (> ssl 0) 
  (setq ob_angle (getreal "Cho goc nghieng moi cua text : "))
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
);defun TN


;-------------------------------------Chuyen text  -> kieu "Thuong" ---------------------------------------------
(defun c:cth ( / sset ssl temp ed old new )
  (setq sset (ssget))
  (setq ssl (sslength sset) 
        nsset (ssadd)
  )
  (print ssl)
  (princ "entities found. ")  
(print)
;(text_u_ssget)
(setq ssl (sslength sset))
(while (> ssl 0)
    (progn
      (setq temp (ssname sset (setq ssl (1- ssl))))
      (setq ed (entget temp))
     (if (or (= (DXF 0 ed) "TEXT") (= (DXF 0 ed) "MTEXT"))
         (setq 
             old (nth (xdtt 7 ed) ed)
             new (cons 7 "Thuong")
             ed (subst new old ed)  
          ) 
      );if              
      (entmod ed)  
      );progn
  );while
 (princ)
);defun

;-------------------------------------Chuyen text  -> kieu "IN" ---------------------------------------------
(defun c:cin ( / sset ssl temp ed old new )
  (setq sset (ssget))
  (setq ssl (sslength sset) 
        nsset (ssadd)
  )
  (print ssl)
  (princ "entities found. ")  
(print)
;(text_u_ssget)
(setq ssl (sslength sset))
(while (> ssl 0)
    (progn
      (setq temp (ssname sset (setq ssl (1- ssl))))
      (setq ed (entget temp))
     (if (or (= (DXF 0 ed) "TEXT") (= (DXF 0 ed) "MTEXT"))
         (setq 
             old (nth (xdtt 7 ed) ed)
             new (cons 7 "IN")
             ed (subst new old ed)  
          ) 
      );if              
      (entmod ed)  
      );progn
  );while
 (princ)
);defun

;--------------------------------------Tao under Line--------------------------------------------------------
(defun c:UU( / sset ssl temp ed old new )
(text_uu_ssget)
(setq ssl (sslength sset))
(while (> ssl 0)
	      (progn
	      (setq temp (ssname sset (setq ssl (1- ssl))))
	      (setq ed (entget temp))

	       (setq old (cons 1 (DXF 1 ed))
	             new (cons 1 (strcat "%%u" (DXF 1 ed)))
	             ed (subst new old ed)
	           )
	      (entmod ed)
	      )
 	(princ)
  )
  
);defun UU

(defun text_uu_ssget ( / ssl  nsset temp ed )
	  (setq sset (ssget))
	  (setq ssl (sslength sset) 
	        nsset (ssadd)
	  )
	  (print ssl)
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
	  (princ)
	  (print)
);defun u_ssget

;--------------------------------------Bo under Line--------------------------------------------------------
(defun c:NU( / sset ssl temp ed old new )
(text_uu_ssget)
(setq ssl (sslength sset))
(while (> ssl 0)
	      (progn
	      (setq temp (ssname sset (setq ssl (1- ssl))))
	      (setq ed (entget temp))

	       (setq old (cons 1 (DXF 1 ed))
	             new (cons 1 (substr (DXF 1 ed) 4))
	             ed (subst new old ed)
	           )
	      (entmod ed)
	      )
 	(princ)
  )
  
);defun UU

(defun text_uu_ssget ( / ssl  nsset temp ed )
	  (setq sset (ssget))
	  (setq ssl (sslength sset) 
	        nsset (ssadd)
	  )
	  (print ssl)
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
	  (princ)
	  (print)
);defun u_ssget

;;--------XOAY TEXT THEO MOT DUONG THANG CHON-----------
(defun c:xt ()
  (setq sset (ssget))
  (setq ssl (sslength sset) 
        nsset (ssadd)
  )
  (print ssl)
  (princ "entities found. ")  
  (print)
  (setq l1 (entget (car (entsel "\nPick Line: "))))
  (if (and (> ssl 0) (= (DXF 0 l1) "LINE")) 
   (while (> ssl 0)
    (progn
     (setq temp (ssname sset (setq ssl (1- ssl))))
     (setq ed (entget temp))
     (if (or (= (DXF 0 ed) "TEXT") (= (DXF 0 ed) "MTEXT")) 
      (progn
       (setq 
             pt1 (cdr (nth (xdtt 10 l1) l1))
	     pt2 (cdr (nth (xdtt 11 l1) l1))
	     ag (angle pt1 pt2)
       )
       (if (and (> ag (* pi 0.5)) (< ag (* pi 1.5))) (setq ag (- ag pi)))
       (setq              
	     old (nth (xdtt 50 ed) ed)
             new (cons 50 ag)
             ed (subst new old ed)
       )
       (entmod ed)  
       (princ '\n)
       (princ ag)
       (princ "\n") 
       (princ ed)
      );progn  
     );if
    );progn
  );while
  );if
); defun c:xt



;--------------------------------------Them tiep dau ngu-----------------------------------------------------
(defun c:TD( / sset ssl temp ed old new )
(text_uu_ssget)
(setq tdd (getstring "Tiep dau can them:"))
(setq ssl (sslength sset))
(while (> ssl 0)
	      (progn
	      (setq temp (ssname sset (setq ssl (1- ssl))))
	      (setq ed (entget temp))
	      (if (or (= (DXF 0 ed) "TEXT") (= (DXF 0 ed) "MTEXT"))
	       (setq old (cons 1 (DXF 1 ed))
	             new (cons 1 (strcat tdd (DXF 1 ed)))
	             ed (subst new old ed)
	       )
	      )	
	      (entmod ed)
	      )
 	(princ)
  )
  
);defun TD

;--------------------------------------Them tiep cuoi ngu-----------------------------------------------------
(defun c:TC( / sset ssl temp ed old new )
(text_uu_ssget)
(setq tcd (getstring "Tiep cuoi can them:"))
(setq ssl (sslength sset))
(while (> ssl 0)
	      (progn
	      (setq temp (ssname sset (setq ssl (1- ssl))))
	      (setq ed (entget temp))
	      (if (or (= (DXF 0 ed) "TEXT") (= (DXF 0 ed) "MTEXT"))
	       (setq old (cons 1 (DXF 1 ed))
	             new (cons 1 (strcat (DXF 1 ed) tcd))
	             ed (subst new old ed)
	       )
 	      )
	      (entmod ed)
	      )
 	(princ)
  )
  
);defun TC


;---------------------------------------Nhan so voi mot so can nhan them---------------------

(defun c:ns()
  (setvar "CMDECHO" 0)        
	 (setq co (getreal "\nGia tri can so nhan:"))
	 (setq co1 (tphan (rtos co)))
         (SETQ SET (SSGET))
	 (SETQ QUANT (SSLENGTH SET))
	 (SETQ INDEX 0)
       	 (WHILE (< INDEX QUANT)
	   (if	
             (OR (= "TEXT" (CDR (ASSOC 0 (SETQ s (ENTGET (SSNAME SET INDEX)))))) (= (DXF 0 s) "MTEXT"))
		  (PROGN			   
			 (setq otext (assoc 1 s))
			 (setq ot (cdr otext))
			 (command "luprec" (+ (tphan ot) co1))
			 (setq ot (atof ot))
			 (setq nt (cons 1 (rtos (* ot co))))  
			 (setq s (subst nt otext s))
			 (entmod s)
        	       )
                  )
     		  (setq index (+ index 1))
                 )
(command "luprec" "4")  
)

;---------------------------------------Cong so voi mot so can cong them---------------------

(defun c:cs()
  (setvar "CMDECHO" 0)
  (setq co (getreal "\nGia tri can cong them:"))
  (SETQ SET (SSGET))
  (SETQ QUANT (SSLENGTH SET))
  (SETQ INDEX 0)
  (WHILE (< INDEX QUANT)
   (if	
    (OR (= "TEXT" (CDR (ASSOC 0 (SETQ s (ENTGET (SSNAME SET INDEX)))))) (= (DXF 0 s) "MTEXT"))
     (PROGN
	(setq otext (assoc 1 s))
	(setq ot (cdr otext))
	(command "luprec" (max (tphan ot) co1))
    	(setq ot (read ot))
	(setq nt (cons 1 (rtos (+ ot co))))  
	(setq s (subst nt otext s))
	(entmod s)
     )
   );if
   (setq index (+ index 1))
  );while
 (command "luprec" "4")  
);defun


;---------------------------------------Bo chu so sau dau phay---------------------
(defun c:bdp()
  (setq cmd (getvar "cmdecho"))
  (setvar "cmdecho" 0) 
  (setq sset  (ssget))
  (setq ssl   (sslength sset) 
        nsset (ssadd)
  )
  (print ssl)
  (princ "entities found.")  
  (print)
  ;(text_u_ssget)
(setq ssl (sslength sset))
(setq m (getint "\n So chu so sau dau phay:"))
(while (> ssl 0)
  (progn
      (setq temp (ssname sset (setq ssl (1- ssl))))
      (setq ed (entget temp))
      (if (= (DXF 0 ed) "TEXT")
       (progn
         (setq textold(nth (xdtt 1 ed) ed))
	 (setq t1(cdr textold)) 	 
	 (setq k1 (tphan t1))
	 (if (> k1 m) (setq t1 (substr t1 1 (- (strlen t1) (- k1 m)))))	
	 (setq textnew( cons 1 t1)) 
         (setq   ed (subst textnew textold ed))
         (entmod ed)  
      );progn  
     );if
  );progn
);while
 (setvar "cmdecho" cmd)
);defun
;---------------------------------------Bo chu so sau dau phay---------------------
(defun bdp1(text m)
  (setq cmd (getvar "cmdecho"))
  (setvar "cmdecho" 0) 
  (setq t1 text)
  (setq k1 (tphan text))
  (if (> k1 m) (setq t1 (substr t1 1 (- (strlen t1) (- k1 m)))))
  t1		
  (setvar "cmdecho" cmd)
);defun
;-------------------------------------So chu so sau dau phay--------------------------
(defun tphan (a)
 (setq l1 (strlen a))
 (setq index1 1)
 (setq k l1)
 (while (< index1 (+ l1 1))
  (progn
   (if (= (substr a index1 1) ".")
    (setq k index1)
   );if
   (setq index1 (+ index1 1))
  );progn
 );while 
 (setq kq (- l1 k))
 kq
)
;---------------------------------------Cong cac text-------------------------------------

(defun c:ct()
  (setvar "CMDECHO" 0)
  (command "luprec" "0") 
  (SETQ SET (SSGET))
  (SETQ QUANT (SSLENGTH SET))
  (SETQ INDEX 0)
  (setq lu1 0)
  (setq tong 0)
  (WHILE (< INDEX QUANT)
   (if	
    (OR (= "TEXT" (CDR (ASSOC 0 (SETQ s (ENTGET (SSNAME SET INDEX)))))) (= (DXF 0 s) "MTEXT"))
     (PROGN
	(setq otext (assoc 1 s))
	(setq ot (cdr otext))
	(setq lu1 (max lu1 (tphan ot)))
	(setq ot (atof ot))
	(setq tong (+ tong ot))  
	(entmod s)
     )
   );if
   (setq index (+ index 1))
  );while
 (command "luprec" lu1)
 (setq p1 (getpoint "\n Pick a point:"))
 (command ".text" p1 "" "" (rtos tong))
 (command "luprec" "4")  
);defun

;-------------------TAO MOT CHUOI SO TANG DAN THEO THU TU--------------------
(Defun C:stt ()
  (setvar "CMDECHO" 0)
  (setvar "BLIPMODE" 0)
  (princ "\nGhi so Theo thu tu tu 1 - n.")
  (setq CT  (getreal "\nChieu cao text: ")
    	PT1 (getpoint "\nDiem bat dau ghi so: ")
	DST (getdist "\nKhoang cach giua hai so :")
	DIR (getorient "\nGoc ghi so :")
	NN 0
	HN (getint "\nSo can ghi den la: ")
  )
  (repeat HN
    (setq PT1 (polar PT1 DIR DST))
    (command ".Text" "C" PT1 CT "" (itoa (setq NN (+ NN 1))))
  )
 )

;--------------------------------------Sap xep text----------------------------------
(defun c:sxt( / sset ssl temp ed old new )
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
(setq pt1 (getpoint "Diem bat dau: ")))
(setq kc (getdist "Khoang cach giua cac text:"))
(setq p1 (cadr pt1))
  (while (> ssl 0)
    (progn
      (setq temp (ssname sset (setq ssl (1- ssl))))
      (setq ed (entget temp))
;;;TEXT
     (if  (= (DXF 0 ed) "TEXT")
      (progn
       (setq 
             old1 (nth (xdtt 72 ed) ed)
             new1 (cons 72 0)
             old (nth (xdtt 10 ed) ed)
                     old3 (nth (xdtt 73 ed) ed)
                     new3 (cons 73 0)
                     ed (subst new3 old3 ed)
             new (list (nth 0 old) (nth 1 old) p1 (nth 3 old))
	     p1 (- p1 kc)
             ed (subst new old ed)
             ed (subst new1 old1 ed)   
           );setq
      (entmod ed)  
      ) ;progn
  );if
);progn
);while

(setq pt1 nil)
 (princ)
);defun

(defun c:90 (/ ss1) (setq ss1 (ssget))(command "rotate" ss1 "" pause "90"))
(defun c:09 (/ ss1) (setq ss1 (ssget))(command "rotate" ss1 "" pause "-90"))
(defun c:15 (/ ss1) (setq ss1 (ssget))(command "rotate" ss1 "" pause "15"))
(defun c:51 (/ ss1) (setq ss1 (ssget))(command "rotate" ss1 "" pause "-15"))
(defun c:25 (/ ss1) (setq ss1 (ssget))(command "rotate" ss1 "" pause "225"))
(defun c:52 (/ ss1) (setq ss1 (ssget))(command "rotate" ss1 "" pause "-225"))
(defun c:27 (/ ss1) (setq ss1 (ssget))(command "rotate" ss1 "" pause "270"))
(defun c:72 (/ ss1) (setq ss1 (ssget))(command "rotate" ss1 "" pause "-270"))
(defun c:35 (/ ss1) (setq ss1 (ssget))(command "rotate" ss1 "" pause "135"))
(defun c:53 (/ ss1) (setq ss1 (ssget))(command "rotate" ss1 "" pause "-135"))
(defun c:45 (/ ss1) (setq ss1 (ssget))(command "rotate" ss1 "" pause "45"))
(defun c:54 (/ ss1) (setq ss1 (ssget))(command "rotate" ss1 "" pause "-45")) 
(defun c:18 (/ ss1) (setq ss1 (ssget))(command "rotate" ss1 "" pause "180"))
(defun c:81 (/ ss1) (setq ss1 (ssget))(command "rotate" ss1 "" pause "-180"))
(defun c:30 (/ ss1) (setq ss1 (ssget))(command "rotate" ss1 "" pause "30"))
(defun c:03 (/ ss1) (setq ss1 (ssget))(command "rotate" ss1 "" pause "-30"))
(defun c:60 (/ ss1) (setq ss1 (ssget))(command "rotate" ss1 "" pause "60"))
(defun c:06 (/ ss1) (setq ss1 (ssget))(command "rotate" ss1 "" pause "-60"))

(defun c:s1 (/ a1) (setq a1 (ssget))(command "scale" a1 "" pause "1.1"))
(defun c:s2 (/ a1) (setq a1 (ssget))(command "scale" a1 "" pause "2"))
(defun c:s5 (/ a1) (setq a1 (ssget))(command "scale" a1 "" pause "0.5"))
(defun c:s7 (/ a1) (setq a1 (ssget))(command "scale" a1 "" pause "0.75"))
(defun c:s9 (/ a1) (setq a1 (ssget))(command "scale" a1 "" pause "0.9"))
(defun c:s12 (/ a1) (setq a1 (ssget))(command "scale" a1 "" pause "1.25"))
(defun c:s15 (/ a1) (setq a1 (ssget))(command "scale" a1 "" pause "1.5"))
(defun c:s17 (/ a1) (setq a1 (ssget))(command "scale" a1 "" pause "1.75"))


(defun c:ls (/ ee23xx23)
(setvar "cmdecho" 0)
(setq ee23 (car(entsel"\nPick Object on desired layer:")))
(if ee23(progn
        (setq ee23(entget ee23))
        (setq xx23(cdr(assoc 8 ee23)))
        (command "layer" "s" xx23 "")
       )
     )
     (prompt (strcat "\Current Layer is now..."xx23))
     (setvar "cmdecho" 1)
     (princ)
  )

(defun c:dh () (command "dimedit" "home"))


;-------------------------------------------chuyen dim-----------------------------------
(DEFUN C:CDIM(/ CMD SS LTH DEM PT DS KDL N70 GOCX GOCY PT13 PT14 PTI PT13I PT14I
                PT13N PT14N O13 O14 N13 N14 OSM OLDERR PT10 PT11)
(SETQ CMD (GETVAR "CMDECHO"))
(SETQ OSM (GETVAR "OSMODE"))
(SETQ OLDERR *error*
      *error* myerror)
(PRINC "Choose dimesions:")
(SETQ SS (SSGET))
(SETVAR "CMDECHO" 0)
(COMMAND "UCS" "W")
(SETQ LTH (SSLENGTH SS))
(SETQ DEM 0)
(WHILE (< DEM LTH)
    (PROGN
	(SETQ ED (ENTGET (SSNAME SS DEM)))
	(SETQ KDL (CDR (ASSOC 0 ED)))
	(IF (= "DIMENSION" KDL)
	   (PROGN
		(sETQ   PT10 (nth (xdtt 10 ed) ed)
			PT11 (nth (xdtt 11 ed) ed)
			PT13 (nth (xdtt 13 ed) ed)
			PT14 (nth (xdtt 14 ed) ed)
		)
		(setq PT10N (list (nth 1 pt10) (nth 2 pt10) 0))
            	(setq PT11N (list (nth 1 pt11) (nth 2 pt11) 0))
		(setq PT13N (list (nth 1 pt13) (nth 2 pt13) 0))
		(setq PT14N (list (nth 1 pt14) (nth 2 pt14) 0))
		(SETQ	N10 (CONS 10 PT10N)
			N11 (CONS 11 PT11N)
			N13 (CONS 13 PT13N)
			N14 (CONS 14 PT14N)
		)	
		(SETQ ED (SUBST N10 PT10 ED))
		(SETQ ED (SUBST N11 PT11 ED))
		(SETQ ED (SUBST N13 PT13 ED))
		(SETQ ED (SUBST N14 PT14 ED))
		(ENTMOD ED)
	   )
	)
	(SETQ DEM (+ DEM 1))
    )
)
(COMMAND "UCS" "P")
(SETVAR "CMDECHO" CMD)
(SETVAR "OSMODE" OSM)
(setq *error* OLDERR)               ; Restore old *error* handler
(PRINC)
)

;--------------------------------------Can chinh chu theo phuong dung giua-----------------------------------
(defun c:hg ( / sset ssl temp ed old new )
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
         (setq 
             old2(nth (xdtt 10 ed) ed)
             old1 (nth (xdtt 72 ed) ed)
             new1 (cons 72 1)
             ed (subst new1 old1 ed)
                     old3 (nth (xdtt 73 ed) ed)
                     new3 (cons 73 0)
                     ed (subst new3 old3 ed)
             old (nth (xdtt 11 ed) ed)
             new (list (nth 0 old) (car pt1) (nth 2 old2) (nth 3 old))
             ed (subst new old ed)
             ed (subst new1 old1 ed)
                     
          )
      (entmod ed)  
      )  
  )

;;;MTEXT
      (if (= (DXF 0 ed) "MTEXT")
      (progn
         (setq 
             old1 (nth (xdtt 71 ed) ed)
             new1 (cons 71 2)
             ed (subst new1 old1 ed)           
             old (nth (xdtt 10 ed) ed)
             new (list (nth 0 old) (car pt1) (nth 2 old) (nth 3 old))
             ed (subst new old ed)                            
          )
      (entmod ed)  
      )  
  )      
 )
)


(setq pt1 nil)
 (princ)
);defun

(defun c:cat ()
 (setq cdm (getvar "cmdecho"))
 (setq osm (getvar "osmode"))
 (setvar "cmdecho" 0)
 (setq pt1(getpoint "Diem chen:"))
 (setvar "osmode" 0)
 (setq x (car pt1))
 (setq y (+ (cadr pt1) 65))
 (command "line" (list x (+ y 65)) (list x (+ y 15)) (list (+ x 10) (+ y 15)) (list (- x 10) (- y 15)) (list x (- y 15)) (list x (- y 65)) "")
 (setvar "cmdecho" cdm)
 (setvar "osmode" osm)
) 

(defun c:caodo1()
 (setq cdm (getvar "cmdecho"))
 (setq osm (getvar "osmode"))
 (setvar "cmdecho" 0)
 (setvar "osmode" 0)
 (setq sset (ssget))
 (setq ssl (sslength sset))
 (princ "\n")
 (if (> ssl 0) 
  (while (> ssl 0)
    (progn
      (setq temp (ssname sset (setq ssl (1- ssl))))
      (setq ed (entget temp))
      (if (= (DXF 0 ed) "TEXT")
       (progn
         (setq p1 (nth (xdtt 11 ed) ed))
	 (setq t1 (nth (xdtt 1 ed) ed))
	 (setq x (cadr p1))
	 (setq y (caddr p1))
 	 (setq t2 (cdr t1))
	 (princ x)
	 (princ "	")
	 (princ y)
	 (princ "	")
	 (princ t2)
	 (princ "\n") 
       );progn  
      );if	    
    );progn
  );while
 );if
 (setvar "cmdecho" cdm)
 (setvar "osmode" osm)
) 

(defun c:ucc() (command "ucsicon" "off"))
(defun c:cf() (command "close"))


;-----------------------------------------------------------------------------------------------------------
(defun block_u_ssget ( / ssl  nsset temp ed )
  (setq sset (ssget))
  (setq ssl (sslength sset) 
        nsset (ssadd)
  )
  (print ssl)
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
  (princ)
  (print)
);defun block_u_ssget

;-------------------------------------------Thay doi ty le chen cua block-----------------------------------

(defun c:bsca()
(block_u_ssget)
(setq ssl (sslength sset))
(if(> ssl 0)
 (progn
   (setq xsca(getreal "\nTy le chen phuong x:"))
   (setq ysca(getreal "\nTy le chen phuong y:"))
 );progn
);if
(while (> ssl 0)
  (progn
      (setq temp (ssname sset (setq ssl (1- ssl))))
      (setq ed (entget temp))
      (princ ed)
      (setq xold (nth (xdtt 41 ed) ed)
	    yold (nth (xdtt 42 ed) ed)
	    xnew (cons 41 xsca)
	    ynew (cons 42 ysca)
	    ed (subst xnew xold ed)
	    ed (subst ynew yold ed)
      )
      (entmod ed)    
      (princ xold)	
  );progn
 );while
);defun