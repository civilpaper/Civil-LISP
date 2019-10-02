;;;-------------------------------------------------------------------------------------;;;
;;;				Function    : Tong hop cac lisp hay dung		;;;
;;;				Collected by: CivilPaper.com 17/11/2007			;;;
;;;-------------------------------------------------------------------------------------;;;
;;;					VERSION HISTORY					;;;
;;; - Chinh sua ngay 21/06/2014 dua tren ma DXF cua phien ban CAD 2015			;;;
;;; ------------------------------------------------------------------------------------;;;

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
    (setq i (+ 1 i))
    );while
  )
;Set some variables:
(setq ly_scale 1)
;Set Insert scale unit to: milli
(setvar "INSUNITS" 4)
;**************************************************************************
;*                   Some commands about layer                            *
;**************************************************************************
;************************ Change selected objects to the color of their layer ****************************
(defun c:CBL () (c:ColorByLayer))
(defun c:ColorByLayer  (/ *error*)
  (princ "\rCOLORBYLAYER ")
  (defun *error*  (msg)
    (cond ((not msg) (if acDoc (vla-endundomark acDoc)))           ; Normal exit
          ((member msg '("Function cancelled" "quit / exit abort")))    ; <esc> or (quit)
          ((princ (strcat "\n** Error: " msg " ** "))))                 ; Fatal error, display it
    (princ))

  ((lambda (acDoc / ss oLayer oLayers oLayersList)
     (if (setq ss (ssget "_:L"))
       (progn
         (vla-startundomark acDoc)
         (vlax-for x (setq ss (vla-get-activeselectionset acDoc))
           (if (= :vlax-true
                  (vla-get-lock
                    (setq oLayer (vla-item (cond (oLayers)
                                                 ((setq oLayers
                                                         (vla-get-layers acDoc))))
                                           (vla-get-layer x)))))
             (progn
               (setq oLayersList (cons oLayer oLayersList))
               (vla-put-lock oLayer :vlax-false)))
           (vl-catch-all-apply
             'vla-put-color
             (list x (vla-get-color oLayer))))
         (vla-delete ss)
         (foreach oLayer  oLayersList
           (vla-put-lock oLayer :vlax-true))
         (*error* nil))
       (*error* "Nothing selected")))
    (vla-get-activedocument (vlax-get-acad-object))))
;************************ Tao Layer ****************************
(defun c:mla()
	(command "_layer" "N" "1" "C" 1 "1" "L" "continuous" "1" "") ; color of dim 0.01 for A3 drawing
	(command "_layer" "N" "2" "C" 2 "2" "L" "continuous" "2" "") ; color of normal text 1.5 for A3 drawing	     
	(command "_layer" "N" "3" "C" 3 "3" "L" "continuous" "3" "") ; color of concrete 0.02 for A3 drawing
 	(command "_layer" "N" "4" "C" 4 "4" "L" "continuous" "4" "") ; color of horizon steel 0.03 for A3 drawing 
 	(command "_layer" "N" "5" "C" 5 "5" "L" "continuous" "5" "") ; color of vertical steel 0.03 for A3 drawing 
	(command "_layer" "N" "6" "C" 6 "6" "L" "center" "6" "")     ; color of center line 
 	(command "_layer" "N" "7" "C" 7 "7" "L" "dashed" "7" "")
	(command "_layer" "N" "8" "C" 8 "8" "L" "continuous" "8" "")
	(command "_layer" "N" "9" "C" 9 "9" "L" "continuous" "9" "")
(princ)
)
(defun c:lof () (command "layer" "off" ))
(defun c:lo () (command ".layon"))

;*************************Shortcut to change layer**************************
(defun c:1 (/ a) (setq a (ssget))(command "change" a "" "p" "la" "1" ""))
(defun c:2 (/ a) (setq a (ssget))(command "change" a "" "p" "la" "2" ""))
(defun c:3 (/ a) (setq a (ssget))(command "change" a "" "p" "la" "3" ""))
(defun c:4 (/ a) (setq a (ssget))(command "change" a "" "p" "la" "4" ""))
(defun c:5 (/ a) (setq a (ssget))(command "change" a "" "p" "la" "5" ""))
(defun c:6 (/ a) (setq a (ssget))(command "change" a "" "p" "la" "6" ""))
(defun c:7 (/ a) (setq a (ssget))(command "change" a "" "p" "la" "7" ""))
(defun c:8 (/ a) (setq a (ssget))(command "change" a "" "p" "la" "8" ""))
(defun c:9 (/ a) (setq a (ssget))(command "change" a "" "p" "la" "9" ""))
(defun c:0 (/ a) (setq a (ssget))(command "change" a "" "p" "la" "0" ""))
;*************************   Turnoff layer   *******************************
(Defun c:o1()  (command "_layer"  "off"  "1"  "" ))
(Defun c:o2()  (command "_layer"  "off"  "2"  "" ))
(Defun c:o3()  (command "_layer"  "off"  "3"  "" ))
(Defun c:o4()  (command "_layer"  "off"  "4"  "" ))
(Defun c:o5()  (command "_layer"  "off"  "5"  "" ))
(Defun c:o6()  (command "_layer"  "off"  "6"  "" ))
(Defun c:o7()  (command "_layer"  "off"  "7"  "" ))
(Defun c:o8()  (command "_layer"  "off"  "8"  "" ))
(Defun c:o9()  (command "_layer"  "off"  "9"  "" ))
;************************   Turnon layer   *********************************
(Defun c:on1()  (command "_layer"  "on"  "1"  "" ))
(Defun c:on2()  (command "_layer"  "on"  "2"  "" ))
(Defun c:on3()  (command "_layer"  "on"  "3"  "" ))
(Defun c:on4()  (command "_layer"  "on"  "4"  "" ))
(Defun c:on5()  (command "_layer"  "on"  "5"  "" ))
(Defun c:on6()  (command "_layer"  "on"  "6"  "" ))
(Defun c:on7()  (command "_layer"  "on"  "7"  "" ))
(Defun c:on8()  (command "_layer"  "on"  "8"  "" ))
(Defun c:on9()  (command "_layer"  "on"  "9"  "" ))
;**************************************************************************
;*                   Autocad's command alias                              *
;**************************************************************************
(defun c:a () (command ".line" ))
(defun c:aaa () (command ".massprop" ))
(defun c:ac() (command ".arc"))
(defun c:c (/ m1) (setq m1 (ssget))(command "copy" m1 "" "m"  pause ))
(defun c:co (/ m1) (setq m1 (sget))(command "copy" m1 "" "m"  pause ))
(defun c:cc () (command ".circle"))
(defun c:ci () (command ".circle"))
(defun c:du () (command "dim1" "update"))
(defun c:dx () (command ".dimaligned"))
(defun c:dc () (command "dim1" "continue"))
(defun c:dr () (command ".dimradius"))
(defun c:dl () (command ".dimlinear"))
(defun c:dn () (command ".dim1" "n"))
(defun c:n () (command ".dim1" "n"))
(defun c:de () (command "dim1" "l"))
(defun c:da () (command ".dimangular"))
(defun c:a () (command ".line"))
(defun c:g () (command ".matchprop"))
(defun c:q () (command ".ray"))
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
(defun c:zw () (command ".zoom" "window" pause pause))
(defun c:ed () (command ".ddedit"))
(defun c:ff (/ gp) (setq gp (ssget))(command "mirror" gp "" pause pause "n"))
(defun c:fff (/ gp) (setq gp (ssget))(command "mirror" gp "" pause pause "y"))
(defun c:d () (command ".dist" ))
(defun c:ll () (command ".lengthen" "de" pause ))
(defun c:qqq () (command ".rtedit"))
(defun c:ed () (command "TEXTEDIT")) ; Neu dung CAD 2015 thi dung dong nay con khong thi comment
;*******************************************************************************
;*                         Nhom lenh ve text                                   *
;*******************************************************************************
;********************Can chinh chu theo phuong dung ben phai********************
(defun c:hp ( / sset ssl temp ed old new )
  (setq sset (ssget))
  (setq ssl (sslength sset) 
        nsset (ssadd)
  )
  (print ssl)
  (princ "entities found. ")  
(print);(text_u_ssget)
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
;********************Can chinh chu theo phuong dung ben trai******************************
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
;********************************Can chinh chu theo phuong ngang*****************************
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
;*****************************Can chinh chu theo phuong dung giua********************************
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
;**********************************Arrange texts********************************
(defun c:sx( / sset ssl temp ed old new )
  (setq sset (ssget))
  (setq ssl (sslength sset)
	nsset (ssadd)
	)
  (print ssl)
  (princ "Entities found. ")
  (print)
  ;(text_u_ssget)
  (setq ssl (sslength sset))
  (if (> ssl 0)
    (setq pt1 (getpoint "Start point: ")))
  (setq kc (getdist "Distance between texts:"))
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
;********************************Make text declination****************************************
(defun c:TN( / sset ssl temp ed old new )
  (text_uu_ssget)
  (setq ssl (sslength sset))
  (if (> ssl 0)
    (setq ob_angle (getreal "Input text declination:"))
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
;****************************Make line*********************************************
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
	sset nsset)
  (print ssl)
  (princ)
  (print)
  );defun u_ssget
;*******************************Erase under line*************************************
(defun c:NU( / sset ssl temp ed old new )
  (text_uu_ssget)
  (setq ssl (sslength sset))
  (while (> ssl 0)
    (progn
      (setq temp (ssname sset (setq ssl (1- ssl))))
      (setq ed (entget temp))
      (setq old (cons 1 (DXF 1 ed))
	    new (cons 1 (substr (DXF 1 ed) 4))
	    ed (subst new old ed))
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
	sset nsset)
  (print ssl)
  (princ)
  (print)
  );defun u_ssget
;*****************Change upper case to lower case and reverse*******************

;;;;;;;;; From this topic http://www.theswamp.org/index.php?topic=38947.0
;;;;;;;;; Lisp nay co the doi duoc chu o text trong leader lan text thuong
;;;;;;;;; Chi co tac dung doi voi text, khong co tac dung doi voi Mtext
(defun c:tcase2 ( / acsel func name ) ;; © Lee Mac 2011
  (if
    (ssget "_:L"
     '(
        (-4 . "<OR")
          (0 . "ARCALIGNEDTEXT,ATTDEF,*TEXT")
          (-4 . "<AND")
            (0 . "INSERT")
            (66 . 1)
          (-4 . "AND>")
          (-4 . "<AND")
            (0 . "*DIMENSION")
            (1 . "*?*")
          (-4 . "AND>")
          (-4 . "<AND")
            (0 . "MULTILEADER")
            (304 . "~LEADER_LINE{")
          (-4 . "AND>")
        (-4 . "OR>")
      )
    )       
    (progn
      (initget "Sentence Lower Upper Title toGgle")
      (setq func
        (eval
          (cdr
            (assoc
              (setq case
                (cond
                  (
                    (getkword
                      (strcat "\nSpecify Case [Sentence/Lower/Upper/Title/toGgle] <"
                        (setq case (cond ( case ) ( "Upper" ))) ">: "
                      )
                    )
                  )
                  ( case )
                )
              )
             '(
                ("Upper" . strcase)
                ("Lower" . (lambda ( x ) (strcase x t)))
                ("Title" . EE:TitleCase)
                ("Sentence" . LM:SentenceCase)
                ("toGgle" . LM:ToggleCase)
              )
            )
          )
        )
      )
      (vlax-for obj
        (setq acsel
          (vla-get-activeselectionset
            (vla-get-activedocument (vlax-get-acad-object))
          )
        )
        (cond
          ( (wcmatch (setq name (vla-get-objectname obj)) "AcDb*Dimension")
            (vla-put-textoverride obj (func (vla-get-textoverride obj)))
          )
          ( (or (eq "RText" name)(eq "AcDbArcAlignedText" name))
            (vlax-put-property obj 'contents (func (vlax-get-property obj 'contents)))
          )
          ( (eq "AcDbMLeader" name)
            (if (eq acmtextcontent (vla-get-contenttype obj))
              (vla-put-textstring obj (func (vla-get-textstring obj)))
            )
          )
          ( (eq "AcDbBlockReference" name)
            (foreach att (vlax-invoke obj 'getattributes)
              (vla-put-textstring att (func (vla-get-textstring att)))
            )
          )
          ( t
            (vla-put-textstring obj (func (vla-get-textstring obj)))
          )
        )
      )
      (vla-delete acsel)
    )
  )
  (princ)
)

(defun EE:TitleCase ( s ) ;; ElpanovEvgeniy
  (vl-list->string
    (mapcar
      (function
        (lambda ( a b c ) (if (= 32 a) b c))
      )
      (cons 32 (vl-string->list s))
      (vl-string->list (strcase s))
      (vl-string->list (strcase s t))
    )
  )
)

(defun LM:SentenceCase ( s / f )
  (vl-list->string
    (mapcar
      (function
        (lambda ( a b c )
          (if (or f (= 46 a)) (progn (setq f (= 32 b)) b) c)
        )
      )
      (cons 46 (vl-string->list s))
      (vl-string->list (strcase s))
      (vl-string->list (strcase s t))
    )
  )
)

(defun LM:ToggleCase ( s )
  (vl-list->string
    (mapcar
      (function
        (lambda ( a b c ) (if (< 96 a 123) b c))
      )
      (vl-string->list s)
      (vl-string->list (strcase s))
      (vl-string->list (strcase s t))
    )
  )
)

(princ)
(defun c:CA() (c:tcase2))
;END tcase1;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Lisp doi chu khac khong hay bang lisp tren
(defun c:CA1( / sset ssl temp ed old new )
  (text_uu_ssget)
  (initget 1 "U u L l")
  (setq ssl (sslength sset))
  (if (> ssl 0)
    (setq sw (getkword "Loai chu can dieu chinh [U/L] : ")))
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
      (if (or (= sw "l") (= sw "L"))
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
  );defun CA1
;************************Edit multitext*********************************
(defun C:MET (/ SS CNT NUM ENT ANS oldsnp)
  (command"undo""group")
  (setq oldsnp (getvar"snapmode"))
  (setvar"snapmode"0)
  (prompt "\n  Multi Edit Text, Attributes, or Dimensions. ")
  (if(>=(getvar"acadver")"13")
    (setq SS (ssget '((-4 . "<or")(0 . "ATTDEF")(0 . "DIMENSION")
		      (0 . "MTEXT")(0 . "TEXT")(0 . "TOLERANCE")
		      (-4 . "<and")(0 . "INSERT")(66 . 1)(-4 . "and>")(-4 . "or>"))))
    (setq SS (ssget '((-4 . "<or")(0 . "ATTDEF")(0 . "TEXT")
		      (-4 . "<and")(0 . "INSERT")(66 . 1)(-4 . "and>")(-4 . "or>")))))
  (if SS
    (progn
      (setq NUM (sslength SS) CNT 0)
      (while(< CNT NUM)
	(princ(strcat"\r  Edit Text or Attribute: "(rtos(1+ CNT) 2 0)" of "(rtos NUM 2 0)": "))
	(setq ENT (ssname SS CNT))
	(setq ANS (cdr(assoc 0(entget ENT))))
	(cond
	  ((or(eq ANS "ATTDEF")(eq ANS "DIMENSION")(eq ANS "MTEXT")
	      (eq ANS "TEXT")(eq ANS "TOLERANCE"))
	   (command "TEXTEDIT" ENT \))
	  ((eq ANS "INSERT")
	   (command "DDatte" ENT \))
	  )
	(entupd ENT)
	(setq CNT (1+ CNT))
	)
      (princ"  Done... "))
    (princ"  Nothing selected. "))
  (setvar"snapmode"oldsnp)
  (command"undo""end")
  (princ)
  ) ;EOF
;*******************************************************************************
;*                         Nhom lenh ve hieu chinh tao hinh                    *
;*******************************************************************************
;**************************Join objects*****************************************
(defun c:nn (/ tdt ssdt sodt index)
  (defun ObjName (ssdt /)
    (cdr (assoc '0 (entget ssdt)))
    )
  (defun MoPL (ssdt /)
    (= (cdr (assoc '70 (entget ssdt))) 0)
    )
  (defun NoiPL (ssdt /)
    (if (MoPL ssdt)
      (command ".PEDIT" ssdt "J" "All" "" "X")
      )
    )
  (defun NoiLC (ssdt /)
    (command ".PEDIT" ssdt "Y" "J" "All" "" "X")
    )
  (setq
    tdt (ssget)
    sodt (sslength tdt)
    index 0
    )
  (repeat sodt
    (setq
      ssdt (ssname tdt index)
      index (1+ index)
      )
    (if (or (= (Objname ssdt) "LWPOLYLINE")
	    (= (Objname ssdt) "POLYLINE")
	    )
      (NoiPL ssdt)
      )
    (if (or (= (Objname ssdt) "LINE") (= (Objname ssdt) "ARC"))
      (NoiLC ssdt)
      )
    )
  (princ)
  )
;*;**************************Cut dim and base dim*****************************************
(defun myerror (s)                    ; If an error (such as CTRL-C) occurs
                                        ; while this command is active...
  (cond
    ((= s "quit / exit abort") (princ))
    ((/= s "Function cancelled") (princ (strcat "\nError: " s)))
    )
  (setvar "cmdecho" CMD)             ; Restore saved modes
  (setvar "osmode" OSM)
  (setq *error* OLDERR)               ; Restore old *error* handler
  (princ)
  ); end myerror
;***Main prog****
(DEFUN C:CD (/ CMD SS LTH DEM PT DS KDL N70 GOCX GOCY PT13 PT14 PTI PT13I PT14I
                PT13N PT14N O13 O14 N13 N14 OSM OLDERR PT10 PT11)
  (SETQ CMD (GETVAR "CMDECHO"))
  (SETQ OSM (GETVAR "OSMODE"))
  (SETQ OLDERR *error*
	*error* myerror)
  (PRINC "Please select dimension object!")
  (SETQ SS (SSGET))
  (SETVAR "CMDECHO" 0)
  (SETQ PT (GETPOINT "Point to trim or extend:"))
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
	  (IF (OR (= N70 0) (= N70 32) (= N70 33) (= N70 160) (= N70 161))
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
;******************************************************************************
(DEFUN C:BD (/ CMD SS LTH DEM PT DS KDL N70 GOCX GOCY PT13 PT14 PTI
                PT10 PT10I PT10N O10 N10 PT11 PT11N O11 N11 KC OSM OLDERR)
  (SETQ CMD (GETVAR "CMDECHO"))
  (SETQ OSM (GETVAR "OSMODE"))
  (SETQ OLDERR *error*
      *error* myerror)
  (PRINC "Please select dimension object!")
  (SETQ SS (SSGET))
  (SETVAR "CMDECHO" 0)
  (SETQ PT (GETPOINT "Point to trim or extend:"))
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
	  (IF (OR (= N70 0) (= N70 32) (= N70 33) (= N70 160) (= N70 161))
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
;*******************************************************************************
;*                               Utilities command                             *
;*******************************************************************************
; Change betweeen model and layout tab.
(defun c:TM( / TILE)
  (setq TILE (getvar "TILEMODE"))
  (if (= TILE 0)
    (progn
      (setvar "TILEMODE" 1)
      )
    (progn
      (setvar "TILEMODE" 0)
      )
    );if
  )
;; Created by: Lee Ambrosius
;; Date Written: 12/11/03

;; Includes four commands which can be redefined.
;; NextLayout - Navigates to the right and will wrap around
;;              to the left side once the end is reached

;; PreviousLayout - Navigates to the left and will wrap around
;;                  to the right side once the end is reached

;; FirstLayout - Navigates to the left most tab

;; LstLayout - Navigates to the right most tab

;; Gets a listing of all Layouts in the drawing
(defun Get-Layout-List( / acadObj acDoc acDocLayouts layoutCount loopCount
                          layoutListLocal layoutListSorted layoutName layoutPosition
                          loopCountSorted)
  (vl-load-com)
  (setq acadObj (vlax-get-acad-object))
  (setq acDoc (vlax-get-property acadObj 'ActiveDocument))
  (setq acDocLayouts (vlax-get-property acDoc 'Layouts))
  (setq layoutCount (vlax-get-property acDocLayouts 'Count)
	loopCount 0
	layoutListLocal (list)
	layoutListSorted (list)
	)
  (while (> layoutCount loopCount)
    (setq layoutName (vlax-get-property (vlax-invoke-method acDocLayouts 'Item loopCount) 'Name))
    (setq layoutPosition (vlax-get-property (vlax-invoke-method acDocLayouts 'Item loopCount) 'TabOrder))
    (setq layoutListLocal (append layoutListLocal (list (list layoutPosition layoutName))))
    (setq loopCount (1+ loopCount))
    )
  ;; Resort listing by TabOrder
  (setq layoutCountSorted 0)
  (while (> (length layoutListLocal) (length layoutListSorted))
    (setq loopCountSorted 0)
    (foreach layoutLocation layoutListLocal
      (progn
	(if (and (= (car layoutLocation) (length layoutListSorted)) (= (car layoutLocation) layoutCountSorted))
	  (progn
	    (setq layoutListSorted (append layoutListSorted (cdr (nth loopCountSorted layoutListLocal))))
	    (setq layoutCountSorted (1+ layoutCountSorted))
	    )
	  )
	(setq loopCountSorted (1+ loopCountSorted))
	)
      )
    )
  layoutListSorted
  )
(defun c:nextlayout ( / layout-mem-list layout-list layoutLocation)
  (setq layoutLocation 0)
  (setq layout-list (get-layout-list))
  (setq layout-mem-list (member (getvar "CTAB") layout-list))
  (if layout-mem-list
    (progn
      (setq layoutLocation (- (length layout-list) (length layout-mem-list)))
      )
    (setq layoutLocation (1+ layoutLocation))
    )
  (if (>= (1+ layoutLocation) (length layout-list))
    (setvar "CTAB" (nth 0 layout-list))
    (setvar "CTAB" (nth (1+ layoutLocation) layout-list))
    )
  )
(defun c:previouslayout ( / layout-mem-list layout-list layoutLocation)
  (setq layoutLocation 0)
  (setq layout-list (get-layout-list))
  (setq layout-mem-list (member (getvar "CTAB") layout-list))
  (if layout-mem-list
    (progn
      (setq layoutLocation (- (length layout-list) (length layout-mem-list)))
      )
    (setq layoutLocation (1- layoutLocation))
    )
  (if (= layoutLocation 0)
    (setvar "CTAB" (nth (1- (length layout-list)) layout-list))
    (setvar "CTAB" (nth (1- layoutLocation) layout-list))
    )
  )
(defun c:FirstLayout ( / layout-list layoutLocation)
  (setvar "CTAB" (nth 0 (get-layout-list)))
  )
(defun c:LastLayout ( / layout-list)
  (setq layout-list (get-layout-list))
  (setvar "CTAB" (nth (- (length layout-list) 1) layout-list))
  )
(princ)
(DEFUN C:TMN() (C:NEXTLAYOUT))
(DEFUN C:TMP() (C:PREVIOUSLAYOUT))
(DEFUN C:TMF() (C:FIRSTLAYOUT))
(DEFUN C:TML() (C:LASTLAYOUT))
;******************************************************************Command: Delete Objects Not on Any Viewport******************************************************************************************************
    (defun DelObjectsNotOnAnyViewport ()
     (setq ssview (ssadd))
     (setvar 'ctab "MODEL")
     (setq app (vlax-get-acad-object))
     (vlax-for lay                         ; for each layout
                   (vla-get-layouts (vla-get-activedocument (vlax-get-acad-object)))
       (setq id1 nil)                      ; ignore the first vp
       (if (eq :vlax-false (vla-get-modeltype lay))
         (progn (vlax-for obj (vla-get-block lay) ; for each obj in layout
                  (if (and (= (vla-get-objectname obj) "AcDbViewport")
                           (or id1 (not (setq id1 t))) ; ignore first viewport because that is the viewport tab itself
                      )
                    (progn (vla-GetBoundingBox obj 'LPVP 'UPVP)
                           (setq LPVP (vlax-safearray->list LPVP))
                           (setq UPVP (vlax-safearray->list UPVP))
                           (setq LPMODEL (PCS2WCS LPVP (vlax-vla-object->ename obj)))
                           (setq UPMODEL (PCS2WCS UPVP (vlax-vla-object->ename obj)))
                           (setq minx (car LPMODEL))
                           (setq maxx (car UPMODEL))
                           (setq miny (cadr LPMODEL))
                           (setq maxy (cadr UPMODEL))
                           (setq pt1 (list minx miny))
                           (setq pt2 (list maxx miny))
                           (setq pt3 (list maxx maxy))
                           (setq pt4 (list minx maxy))
                           (vla-zoomwindow app (vlax-3d-point pt1) (vlax-3d-point pt3))
                           (if (setq ss (ssget "_CP" (list pt1 pt2 pt3 pt4) (list (cons 410 "MODEL"))))
                             (setq ssview (kdub:ssunion ssview ss))
                           )
                    )
                  )
                )
         )
       )
     )
     (setq ssall (ssget "_X" (list (cons 410 "MODEL"))))
     (setq sstodel (kdub:sssubtract ssall ssview))
     (repeat (setq n (sslength sstodel)) (setq ent (ssname sstodel (setq n (1- n)))) (entdel ent))
    )
     
     
    ;;; Union of two selection sets
    (defun kdub:ssunion (ss1 ss2 / ss index)
    ;;; Source : http://www.theswamp.org/index.php?topic=46652.0
     (setq ss (ssadd))
     (cond ((and ss1 ss2)
            (setq index -1)
            (repeat (sslength ss1) (ssadd (ssname ss1 (setq index (1+ index))) ss))
            (setq index -1)
            (repeat (sslength ss2) (ssadd (ssname ss2 (setq index (1+ index))) ss))
           )
           (ss1 (setq ss ss1))
           (ss2 (setq ss ss2))
           (t (setq ss nil))
     )
     ss
    )
     
    ;; Subtracts one selection set from another and returns their difference
    ;; NOT optimal because it changes the previous/last selection set.
    (defun kdub:sssubtract (ss1 ss2 / ss)
    ;;; Source : http://www.theswamp.org/index.php?topic=46652.0
     (cond ((and ss1 ss2) (vl-cmdf "._Select" ss1 "_Remove" ss2 "") (setq ss (ssget "_P")))
           (ss1 (setq ss ss1))
           (t (setq ss nil))
     )
     ss
    )
     
    (defun PCS2WCS (pnt ent / ang enx mat nor scl)
    ;;; Source : http://forums.autodesk.com/t5/Visual-LISP-AutoLISP-and-General/Auto-Update-of-XY-coord-in-Model-Space-onto-Paper-Space-Layout/td-p/4591789/page/2
     (setq pnt (trans pnt 0 0)
           enx (entget ent)
           ang (- (cdr (assoc 51 enx)))
           nor (cdr (assoc 16 enx))
           scl (/ (cdr (assoc 45 enx)) (cdr (assoc 41 enx)))
           mat (mxm (mapcar (function (lambda (v) (trans v 0 nor t))) '((1.0 0.0 0.0) (0.0 1.0 0.0) (0.0 0.0 1.0)))
                    (list (list (cos ang) (- (sin ang)) 0.0) (list (sin ang) (cos ang) 0.0) '(0.0 0.0 1.0))
               )
     )
     (mapcar '+
             (mxv mat (mapcar '+ (vxs pnt scl) (vxs (cdr (assoc 10 enx)) (- scl)) (cdr (assoc 12 enx))))
             (cdr (assoc 17 enx))
     )
    )
     
    ;; Matrix Transpose  -  Doug Wilson
    ;; Args: m - nxn matrix
     
    (defun trp (m) (apply 'mapcar (cons 'list m)))
     
    ;; Matrix x Matrix  -  Vladimir Nesterovsky
    ;; Args: m,n - nxn matrices
     
    (defun mxm (m n) ((lambda (a) (mapcar '(lambda (r) (mxv a r)) m)) (trp n)))
     
    ;; Matrix x Vector  -  Vladimir Nesterovsky
    ;; Args: m - nxn matrix, v - vector in R^n
     
    (defun mxv (m v) (mapcar '(lambda (r) (apply '+ (mapcar '* r v))) m))
     
    ;; Vector x Scalar  -  Lee Mac
    ;; Args: v - vector in R^n, s - real scalar
     
    (defun vxs (v s) (mapcar '(lambda (n) (* n s)) v))
	
   
;========================================================================================================================================    

;*********************Command: invis and vis******************************
(defun c:InVis (/ SSet Count Elem)
  (defun Dxf (Id Obj)
    (cdr (assoc Id (entget Obj)))
    );end Dxf
  (prompt "\nSelect object(s) to hide: ")
  (cond
    ((setq SSet (ssget))
     (repeat (setq Count (sslength SSet))
       (setq Count (1- COunt)
	     Elem (ssname SSet Count))
       (if (/= 4 (logand 4 (Dxf 70 (tblobjname "layer" (Dxf 8 Elem)))))
	 (if (Dxf 60 Elem)
	   (entmod (subst '(60 . 1) (assoc 60 (entget Elem)) (entget Elem)))
	   (entmod (append (entget Elem) (list '(60 . 1))))
	   )
	 (prompt "\nEntity on a locked layer. Cannot hide this entity. ")
	 );end if
       );end repeat
     )
    );end cond
  (princ)
  );end c:InVis
(defun c:Vis (/ WhatNextSSet Count Elem)
  (defun Dxf (Id Obj)
    (cdr (assoc Id (entget Obj)))
    );end Dxf
  (cond
    ((setq SSet (ssget "_X" '((60 . 1))))
     (initget "Yes No")
     (setq WhatNext (cond
		      ((getkword "\nAll hidden entities will be visible. Continue? No, <Yes>: "))
		      (T "Yes")))
     (cond
       ((= WhatNext "Yes")
	(prompt "\nPlease wait...")
	(repeat (setq Count (sslength SSet))
	  (setq Count (1- COunt)
		Elem (ssname SSet Count))
	  (if (/= 4 (logand 4 (Dxf 70 (tblobjname "layer" (Dxf 8 Elem)))))
	    (entmod (subst '(60 . 0) '(60 . 1) (entget Elem)))
	    (prompt "\nEntity on a locked layer. Cannot make visible this entity. ")
	    );end if
	  );end repeat
	(prompt "\nDone...")
	)
       );end cond
     )
    (T (prompt "\nNo objects was hidden. "))
    )
  )
;*******************************************************************************
;*                      Command about expresstool                              *
;*******************************************************************************



;*******************************************************************************


(load "acettest.fas")
(c:expresstools)
(defun c:ov() (c:overkill))
;*******************************************************************************
;********************************* Current layer   *****************************
(defun layset (/ LAY) (setvar "cmdecho" 0)
  (setq LAY (entsel "\nSelect object for current layer: "))
  (if LAY     (progn
		(setq LAY (cdr (assoc 8 (entget (car LAY)))))
		(command "_.layer" "set" LAY "")
		(princ (strcat "\nLayer : " LAY " was current layer.")))
    (progn
      (if (not ddlop) (load "ddlop"))
      (if (setq LAY (ddlop))
	(progn
	  (command "_.LAYER" "ON" LAY "THAW" LAY "SET" LAY "")
	  (princ (strcat "\nLayer : " LAY " da la hien hanh.")) ) ) ) )
  	(princ) )
(defun c:hh()(layset))
;;  LayerIsolateOnOff.lsp [command names: LIO, LUO]
;;  To Isolate and Unisolate only the On-Off condition of Layers of selected objects.
;;  LIO isolates Layers of selected objects, leaving those Layers on and turning all
;;  other Layers off that are not already off.  If repeated before LUO turns those
;;  Layers back on, makes further isolations, to as many levels as desired.
;;  LUO turns latest set of turned-off Layers back on, without undoing other Layer
;;  options that may have been used under isolated conditions [as happens with
;;  some (e.g. colors) if using AutoCAD's standard LAYERUNISO to return to un-
;;  isolated conditions after using LAYISO].  When repeated, steps back through
;;  as many isolations as were done with LIO [LAYISO can only step back once].
;;  Kent Cooper, August 2011

(vl-load-com)

(defun liV (sub); = build Variable name with subtype and current integer ending
  (read (strcat "li" sub (itoa liinc)))
); defun

(defun liG (sub); = Get what's in the above variable
  (eval (read (strcat "li" sub (itoa liinc))))
); defun

(defun C:LIO (/ ss cmde laysel layname lion layobj); = Layer Isolate -- On-Off condition only
  (prompt "\nTo designate Layer(s) to remain on,")
  (if (setq ss (ssget)); object selection
    (progn
      (setq cmde (getvar 'cmdecho))
      (setvar 'cmdecho 0)
      (command "_.undo" "_begin")
      (repeat (sslength ss); make list of Layer names to remain on
        (setq laysel (cdr (assoc 8 (entget (ssname ss 0))))); Layer name
        (if (not (member laysel lion)) (setq lion (cons laysel lion))); add if not already there
        (ssdel (ssname ss 0) ss)
      ); repeat
      (setq liinc (if liinc (1+ liinc) 1)); liinc is global; 1 for first time, etc.
      (if
        (set (liV "cur"); global variable(s), but need(s) to be:
          (if (not (member (getvar 'clayer) lion)); nil if current Layer kept on
            (vlax-ename->vla-object (tblobjname "layer" (getvar 'clayer)))
          ); if
        ); set
        (setvar 'clayer (nth 0 lion)); then - make some selected object's Layer current
      ); if
      (while (setq layname (cdadr (tblnext "layer" (not layname)))); step through Layers
        (if
          (and
            (not (member layname lion)); not among selected objects' Layers
            (> (cdr (assoc 62 (tblsearch "layer" layname))) 0); currently on
          ); and
          (progn
            (setq layobj (vlax-ename->vla-object (tblobjname "layer" layname)))
            (set (liV "off") (cons layobj (liG "off")))
              ; put in list of Layers turned off -- makes global variables lioff1, lioff2, etc.
            (vla-put-LayerOn layobj 0); turn off
          ); progn
        ); if
      ); while
      (prompt
        (strcat
          "\n"
          (itoa (length lion))
          " Layer(s) isolated, "
          (itoa (length (liG "off")))
          " Layer(s) turned off."
          (if (liG "cur")
            (strcat "  Layer " (getvar 'clayer) " has been made current."); then
            "" ; else - add nothing to prompt if current Layer remains on
          ); if
        ); strcat
      ); prompt
      (command "_.undo" "_end")
      (setvar 'cmdecho cmde)
    ); progn
    (prompt "\nNothing selected.")
  ); if
  (princ)
); defun

(defun C:LUO (/ cmde lugone lucur); = Layer Unisolate -- On-Off condition only
  (if (> liinc 0); at least one list of turned-off Layers exists
    (progn ; then
      (setq cmde (getvar 'cmdecho))
      (setvar 'cmdecho 0)
      (command "_.undo" "_begin")
      (foreach lay (liG "off"); latest numbered list
        (if (vlax-vla-object->ename lay); still in drawing
          (vla-put-LayerOn lay -1); then - turn on
          (progn ; else
            (vl-remove lay (liG "off")); to adjust number for prompt later
            (setq lugone (if lugone (1+ lugone) 1)); quantity of no-longer-present Layers
          ); progn
        ); if
      ); foreach
      (if ; restore Layer current at time of corresponding LIO if it was turned off
        (and
          (liG "cur"); nil if it wasn't
          (vlax-vla-object->ename (liG "cur")); Layer still in drawing, even if renamed
        ); and
        (progn
          (setq lucur (vla-get-Name (liG "cur"))); present name if renamed since its LIO
          (setvar 'clayer lucur); restore as current
        ); progn
      ); if
      (prompt
        (strcat
          "\n"
          (itoa (length (liG "off")))
          " Layer(s) turned back on."
          (if (liG "cur") ; corresponding LIO turned off current Layer at the time
            (strcat ; then
              "\nLayer "
              (if (vlax-vla-object->ename (liG "cur")); still in drawing
                (vla-get-Name (liG "cur")); then - name, even if renamed
                "current at time of LIO purged, and not"
              ); if
              " restored as current."
            ); strcat
            "" ; else - add nothing if corresponding LIO kept current Layer on
          ); if
          (if lugone (strcat "\n" (itoa lugone) " purged Layer(s) not turned back on.") "")
        ); strcat
      ); prompt
      (set (liV "off") nil); clear list ending with latest integer in use
      (set (liV "cur") nil); clear current-at-LIO-Layer-if-changed value with latest integer
      (setq liinc (1- liinc)); increment downward for next-earlier list
      (command "_.undo" "_end")
      (setvar 'cmdecho cmde)
    ); progn
    (prompt "\nNo Layers to Unisolate."); else
  ); if
  (princ)
); defun

(prompt "\nType LIO to Isolate Layers [On/Off only]; LUO to Unisolate.")

;;;;********************************* Layer Iso ***********************************
;;;(Defun LAYISO (/ SS CNT LAY LAYLST VAL)
;;;  (setvar "cmdecho" 0)
;;;  (if (not (setq SS (ssget "i")))
;;;    (progn
;;;      (prompt "\nChon doi tuong cho layer muon hien: ")
;;;      (setq SS (ssget))
;;;      )
;;;    )
;;;  (if SS
;;;    (progn
;;;      (setq CNT 0)
;;;      (while (setq LAY (ssname SS CNT))
;;;	(setq LAY (cdr (assoc 8 (entget LAY))))
;;;	(if (not (member LAY LAYLST))
;;;	  (setq LAYLST (cons LAY LAYLST)))
;;;	(setq CNT (1+ CNT)))
;;;      (if (member (getvar "CLAYER") LAYLST)
;;;	(setq LAY (getvar "CLAYER"))
;;;	(setvar "CLAYER" (setq LAY (last LAYLST))))
;;;      (command "_.LAYER" "_OFF" "*" "_Y")
;;;      (foreach VAL LAYLST (command "_ON" VAL))
;;;      (command "")
;;;      (if (= (length LAYLST) 1)
;;;	(prompt (strcat "\nLayer " (car LAYLST) " da tach ra."))
;;;	(prompt (strcat "\n" (itoa(length LAYLST)) " layers da tach ra. ""layer" LAY " la hien hanh.")))))
;;;  (princ)
;;;  )
;;;(defun c:LI () (layiso))
;*************************Scale*******************************
(defun c:tl (/ tm zoom_xp sslen vp)
  (setq tm (getvar "TILEMODE"))
  (if (= tm 1)
    (setvar "TILEMODE" 0)
  )
  (setq sslen 0)
  (while (/= sslen 1)
    (setq vp (ssget '((0 . "VIEWPORT"))))
    (setq sslen (sslength vp))
  )
  (setq	zoom_xp	(/ (cdr (assoc 41 (entget (ssname vp 0))))
		   (cdr (assoc 45 (entget (ssname vp 0))))
		)
  )
  (setq ly_scale (/ 1 zoom_xp))
  (setq #Bacc (getint "\nInput accuracy: "))
  (setvar "TILEMODE" tm)
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun c:up(/ ss ss_dim ss_blo ss_tex ssl i)
  (setq ss (ssget))
  (setq ss_dim (ssadd))
  (setq ss_blo (ssadd))
  (setq ss_tex (ssadd))
  (setq i 1)
  (setq ssl (sslength ss))
  (while (< i ssl)
    (if
      (= (cdr (assoc 0 (entget (ssname ss i))))"DIMENSION")
      (setq ss_dim (ssadd (ssname ss i) ss_dim)))
    (if
      (= (cdr (assoc 0 (entget (ssname ss i)))) "INSERT")
      (setq ss_dim (ssadd (ssname ss i) ss_blo)))
    (if
      (= (cdr (assoc 0 (entget (ssname ss i)))) "TEXT")
      (setq ss_dim (ssadd (ssname ss i) ss_tex)))
    (setq i (+ i 1))
  )
  (dup ss_dim)
  (bup ss_blo)
  (ct1 ss_tex)
  )
;;;;;;;;;;; ve duong tu nhien ;;;;;;;;;;;;;;;
(defun C:dtn(/ ena dis)
	(command "insert" "C:\\DTN.dwg" "0,0,0" "2" "" "")
	(command "erase" (entlast) "")
	(while (setq ena (car (entsel "\nSelect polyline")))
		(setq dis (getdist "\nKhoang cach 2 ky hieu"))
		(if (not dis) (setq dis 10.00))
		(command "measure" ena "B" "DTN" "" dis)
	)
)
;;;;; Auto Purge;;;
(defun c:atp()
  (command "-purge" "a" "*" "n" "qsave")
  (DelObjectsNotOnAnyViewport())
  (princ)
  )

;*******************************************************************************
;*                             Home tab                                        *
;*******************************************************************************
;************
;*Draw Panel*
;************
;; CVPOLY.LSP Copyright 1996, 1997  Tony Tanzillo  all rights reserved
;;
;; Converts 3D polylines to 2D polylines that lie on
;; the XY plane of the current UCS.

(setq filter:3dpoly
  '(  (-4 . "<AND")
      (0 . "POLYLINE")
      (-4 . "&=")
      (70 . 8)
      (-4 . "AND>")
   )
)

(defun get (key alist)
   (cdr (assoc key alist))
)

(defun UCSZdir ()
   (trans '(0 0 1) 0 1)
)

(defun UCSPoint2D (wcspoint)
   (  (lambda (p)
         (list (car p) (cadr p) 0.0)
      )
      (trans wcspoint 0 1)
   )
)

(defun mapvertex (e function / e d rslt)
   (while (/= "SEQEND" (get 0 (setq d (entget (setq e (entnext e))
'("*")))))
      (setq rslt
         (cons
            (  (lambda (function data / e d rslt)
                  (apply function (list data))
               )
               function
               d
            )
            rslt
         )
      )
   )
   (reverse rslt)
)

(defun cvpoly (pline / data ucs)
   (setq data (entget pline '("*")))
   (entmake
      (subst
         (cons 70 (logand (get 70 data) (~ 8)))
         (assoc 70 data)
         (subst
            (cons 210 (setq ucs (ucszdir)))
            (assoc 210 data)
            (subst
               '(100 . "AcDb2dPolyline")
               '(100 . "AcDb3dPolyline")
               data
            )
         )
      )
   )

   (mapvertex pline
     '(lambda (data)
         (if (zerop (logand (get 70 data) 9))
            (entmake
               (list
                 '(0 . "VERTEX")
                 '(100 . "AcDbEntity")
                  (assoc 8 data)
                 '(100 . "AcDbVertex")
                 '(100 . "AcDb2dVertex")
                  (cons 10 (UCSPoint2d (get 10 data)))
                  (cons 70 (logand (get 70 data) (~ 32)))
               )
            )
         )
      )
   )

   (entmake '((0 . "SEQEND")))

   (entdel pline)
)

(defun C:CVPOLY ( / ss i)
   (cond
      (  (not (setq ss (ssget filter:3dpoly)))
         (princ "\nNo 3D Polylines selected."))
      (t (setvar "CMDECHO" 0)
         (command "._UNDO" "_Begin")
         (repeat (setq i (sslength ss))
            (cvpoly (ssname ss (setq i (1- i))))
         )
         (command "._UNDO" "_En")
      )
   )
   (princ)
)

(princ "\nCVPOLY command loaded.")
(princ)
;; END CVPOLY.LSP
;************
;*Layers Panel*
;************
(defun c:0 ()
(setvar "clayer" "0")
(princ)
) 

;*******************************************************************************
;*                             Layout tab                                      *
;*******************************************************************************


;;;**************************ViewPort Lock & Unlock**************************
;;; Command
;;;    VPL ~ Lock selected viewport
;;;    VPU ~ Unlock selected viewport
;;;    VPLA ~ Lock ALL viewports
;;;    VPUA ~ Unlock ALL viewports


; By Lee-Mac found at the Augi forums

;; Lock Selected Viewport

(vl-load-com)
(defun c:vpl nil
  (if (SSVPLock	(ssget "_+.:E:S:L" '((0 . "VIEWPORT")))
		:vlax-true
      )
    (princ "\n--> Viewport Locked.")
  )
  (princ)
)
;; Unlock Selected Viewport
(defun c:vpu nil
  (if (SSVPLock	(ssget "_+.:E:S:L" '((0 . "VIEWPORT")))
		:vlax-false
      )
    (princ "\n--> Viewport Unlocked.")
  )
  (princ)
)

;; Lock All Viewports

(defun c:vpla nil

  (SSVPLock (ssget "_X" '((0 . "VIEWPORT"))) :vlax-true)

  (princ "\n--> All Viewports Locked.")

  (princ)

)

;; Unlock All Viewports

(defun c:vpua nil
  ;; changed "VPLU" to "VPUA" to be consistant with the above function

  (SSVPLock (ssget "_X" '((0 . "VIEWPORT"))) :vlax-false)

  (princ "\n--> All Viewports UnLocked.")

  (princ)

)

(defun SSVPLock	(ss lock / i)

  (if ss

    (repeat (setq i (sslength ss))

      (vla-put-displaylocked
	(vlax-ename->vla-object (ssname ss (setq i (1- i))))
	lock
      )
      t

    )

  )

)


