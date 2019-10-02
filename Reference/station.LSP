;Ham DXF
(defun DXF (code elist)
  (cdr (assoc code elist))
);dxf
;====================================================

(princ "Danh 'nt' de xac dinh tim tuyen")

; Ham xac dinh tim tuyen
(defun c:cl()
  (setq ss1 nil)
  (while (= ss1 nil)
    (terpri)(princ "Chon tim tuyen")
    (SETQ SS1 (SSGET '((0 . "LWPOLYLINE"))))
  )
  (setq s_len (sslength ss1))
  (if (/= s_len 1) (c:nt))

  (setq goc (getpoint "\nXac dinh diem dau tuyen: "))

  (setq tuyen (ssname ss1 0))
  (setq ptdau (cdr (assoc 10 (entget tuyen))))

  (if (and
	(= (car goc) (car ptdau))
	(= (cadr goc) (cadr ptdau))
      )
    (setq total 0)
    (progn
      (addlen ss1)
      (setq total totallen)
    )
  )
  
)

;==========================

(defun c:st(/ d1 d2 ss2 ss3 ltrinh ten)
  (setq os (getvar "OSMODE"))
  (setq d1 (getpoint "\nInput point:"))
  (setvar "OSMODE" 128)
  (setq d2 (getpoint d1 "\nCenter line point:"))
  (setvar "OSMODE" os)
  (command ".copy" ss1 "" d2 d2)
  (setq ss2 (ssget "_l"))
  (command ".break" ss2 d2 d2)
  (setq ss3 (ssget "_l"))
  (addlen ss2)
  (setq ltrinh1 totallen)
  (addlen ss3)
  (setq ltrinh2 totallen)
  (if (= total 0)
    (setq ltrinh_chon ltrinh1)
    (setq ltrinh_chon ltrinh2)
  )
  ;==========

  (setq so_str (rtos ltrinh_chon 2 3))
  (setq so_len (strlen so_str))
  (setq j 1)
  (while (<= j so_len)
    (progn
      (setq subj (substr so_str j 1))
      (if (= subj ".")
	(progn
	  (if (> (- j 4) 0)
	    (progn
	      (if (= (setq km (substr so_str 1 (- j 4))) "") (setq km "0"))
	      (setq sole (substr so_str (- j 3) (+ j 2)))
	    )
	    (progn
	      (setq km "0")
	      (setq sole so_str)
	    )
	  )
      )
     )
     (setq j (+ j 1))
   )
  )
  (setq lt (strcat "KM" km " + " sole))
  (setvar "OSMODE" os)
  (command ".erase" ss2 "")
  (command ".erase" ss3 "")
  (prompt lt)
  (princ)
  )


  

;=============================
;Ham lay chieu dai cua mot doi tuong
(defun addlen(eset / en enlist entype len tong_len cntr)
  (setvar "cmdecho" 0)
  (defun getArc(en)
    (command "lengthen" en "")
    (getvar "perimeter")
  )
  (defun getLine(en)
    (setq enlist(entget en))
    (distance (cdr(assoc 10 enlist)) (cdr(assoc 11 enlist)))
  )
  (defun getPoly(en)
    (command "area" "Object" en)
    (getvar "perimeter")
  )  
      (setq tong_len 0)
      (setq cntr 0)
      (while (< cntr (sslength eset))
        (setq en (ssname eset cntr))
        (setq enlist (entget en))
        (setq enType (cdr (assoc 0 enlist)))
        (cond
          ((= enType "ARC"       )(setq len(getArc en)))
          ((= enType "CIRCLE"    )(setq len(getPoly en)))
          ((= enType "ELLIPSE"   )(setq len(getPoly en)))
          ((= enType "LINE"      )(setq len(getLine en)))
          ((= enType "LWPOLYLINE")(setq len(getPoly en)))
          ((= enType "POLYLINE"  )(setq len(getPoly en)))
          ((= enType "SPLINE"    )(setq len(getPoly en)))
          (T (setq len 0.0))
        )
        (while (< (strlen enType) 12) (setq enType (strcat enType " ")))
        (setq tong_len (+ tong_len len))
        (setq cntr (+ cntr 1))
      )
  (setvar "cmdecho" 1)
  (setq totallen tong_len)
  ;(princ)
)
;==========================