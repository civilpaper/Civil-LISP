;Ham DXF
(defun DXF (code elist)
  (cdr (assoc code elist))
);dxf
;====================================================
(princ "type CL to specify the center line")
; Ham xac dinh tim tuyen
(defun c:cl()
  (setq ss1 nil)
  (while (= ss1 nil)
    (terpri)(princ "Cetter line")
    (SETQ SS1 (SSGET '((0 . "LWPOLYLINE"))))
  )
  (setq s_len (sslength ss1))
  (if (/= s_len 1) (c:nt))
  (setq goc (getpoint "\nStation 0.00: "))
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

  
;;;;;;;;;;;
(Defun c:tl()
  (setq ly_scale (getreal "Input Scale: "))
)
(defun c:EL(/ ssa ssl i stt si sname os p1 p2 str1 str2)
  (setq os (getvar "OSMODE"))
  (setq unit (getvar "INSUNITS"))
  (setvar "INSUNITS" 4)
  (if (= ly_scale nil) (c:tl))
  (setq p1 (getpoint "\nInsert point:  "))
  (setvar "OSMODE" 128)
  (setq p2 (getpoint "\nPoint for station:  " p1))
  (setvar "OSMODE" os)

  
  (command ".copy" ss1 "" p2 p2)

  (setq ss2 (ssget "_l"))
  (command ".break" ss2 p2 p2)

  (setq ss3 (ssget "_l"))

  (addlen ss2)
  (setq ltrinh1 totallen)
  (addlen ss3)
  (setq ltrinh2 totallen)

  (if (= total 0)
    (setq ltrinh_chon ltrinh1)
    (setq ltrinh_chon ltrinh2)
  )

  (setq lt ltrinh_chon)
  (setvar "OSMODE" os)
  (command ".erase" ss2 "")
  (command ".erase" ss3 "")
 



  
  (setq p3 (getpoint "\Insert Name:  " p1))
  (setq p4 (polar p3 (ANGLE P1 P3) (* -1.5 ly_scale)))
  (setq os (getvar "OSMODE"))
  (setvar "OSMODE" 0)
  (setq ssa (ssget "x"))
  (setq ssl (sslength ssa))
  (setq i 0)
  (setq stt 0)
  (while (< i ssl)
    (setq si (cdr (assoc 0 (entget (ssname ssa i)))))
    (setq sname (cdr (assoc 2 (entget (ssname ssa i)))))
    (if (and (= si "INSERT") (= sname "E2F")) (setq stt (+ stt 1)))
    (setq i (+ i 1))
  )
  (setq stt (+ stt 1))
  (command ".pline" p1 p4 "")
  (mt p1 p4)
  (command ".insert" "E2F.dwg" p3 ly_scale ly_scale "" (rtos (car p1) 2 3) (rtos (cadr p1) 2 3) (rtos stt 2 0) (rtos lt 2 3) (rtos (distance p1 p2) 2 3))
  ;(command ".trim" (entnext (entlast)) p2 "")
  
  (setvar "OSMODE" os)
  (setvar "INSUNITS" unit)

)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;======OOOOOOOOOOOOOOOOOOkkkkkkkkkkkkkkkkkkkkkkkkkkkk===========


(defun c:e2f()
  (setq ss (ssget "x"))
  (setq ssl (sslength ss))
  (setq coor_ss (ssadd))
  (setq i 0)
  (while (< i ssl)
    (setq si (cdr (assoc 0 (entget (ssname ss i)))))
    (setq sname (cdr (assoc 2 (entget (ssname ss i)))))
    (if (and (= si "INSERT") (= sname "E2F"))
      (progn
	(setq coor_ss (ssadd (ssname ss i) coor_ss))
      )
    )
    (setq i (+ i 1))
  )
  (setq all coor_ss)
  (setq all_len (sslength all))
  (setq i 0)
  (setq list_bb '())
  (while (< i all_len)
    (progn
    
      (setq i_ename (ssname all i))
      (setq i_cat (cdr (assoc 0 (entget i_ename))))
      (if (= i_cat "INSERT")
	(progn
	  (setq i_name (cdr (assoc 2 (entget i_ename))))
	  (if (= i_name "E2F")
	    (progn
	      (While (/= i_cat "SEQEND")
		(progn
		   (if (= i_cat "ATTRIB")
		    (progn

		      
		      (if (= (setq i_tag (cdr (assoc 2 (entget i_ename)))) "PT")
			(setq i1_value (cdr (assoc 1 (entget i_ename))))
		      )
		      (if (/= i1_value nil) (setq list_att (list i1_value)))

		      
		      (if (= (setq i_tag (cdr (assoc 2 (entget i_ename)))) "X")
			(progn
			  (setq i2_value (cdr (assoc 1 (entget i_ename))))
			)
		      )

		      (if (= (setq i_tag (cdr (assoc 2 (entget i_ename)))) "Y")
			(progn
			  (setq i3_value (cdr (assoc 1 (entget i_ename))))
			)
		      )

		      (if (= (setq i_tag (cdr (assoc 2 (entget i_ename)))) "STA.")
			(progn
			  (setq i4_value (cdr (assoc 1 (entget i_ename))))
			)
		      )

		      (if (= (setq i_tag (cdr (assoc 2 (entget i_ename)))) "DIS.")
			(progn
			  (setq i5_value (cdr (assoc 1 (entget i_ename))))
			)
		      )
		      


		      
		    )
		  )
		  (setq i_ename (entnext i_ename))
		  (setq i_cat (cdr (assoc 0 (entget i_ename))))
		)
              )
	      (setq list_att (list i1_value i2_value i3_value i4_value i5_value))
	      (if (/= list_att nil) (setq list_bb (cons list_att list_bb)))
	    )
	  )
	)
      )
      (setq i (+ i 1))
      
    
    )
  )

  (setq tf (getstring "\nFile name:"))
  (setq tf (strcat (getvar "DWGPREFIX") tf ".txt"))
  (setq f (open tf "a"))
  (write-line (strcat " " "POINT"  "     " "Y COOR" "          " "X COOR" "       " "STATION" "       " "DIS") f)
  (setq l (length list_bb))
  (setq i 0)
  (while (< i l)
    (setq ssi (nth i list_bb))
    (setq tpt (nth 0 ssi))
    
    (setq ty (nth 1 ssi))
    (setq tx (nth 2 ssi))
    (setq sta (nth 3 ssi))
    (setq dis (nth 4 ssi))
    
    (if (= (strlen tpt) 1) (write-line (strcat "   " tpt  "     " ty "     " tx "     " sta "       " dis) f))
    (if (= (strlen tpt) 2) (write-line (strcat "  " tpt  "     " ty "     " tx "     " sta "       " dis) f))
    (if (= (strlen tpt) 3) (write-line (strcat " " tpt  "     " ty "     " tx "     " sta "       " dis) f))
    (if (= (strlen tpt) 4) (write-line (strcat "" tpt  "     " ty "     " tx "     " sta "       " dis) f))
    (setq i (+ 1 i))

  )
  (close f)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun tach(str / strl str1 str1l sub sub1 i )
  (setq strl (strlen str))
  (setq sub (substr str 1 1))
  (setq i 2)
  (while (< i strl)
    (setq sub (substr str i 1))
    (if (= sub " ")
      (progn
	(setq ten (substr str 1 (- i 1)))
	(setq str1 (substr str (+ i 1) (- strl i)))
	(setq i strl)
      )
    )
    (setq i (+ 1 i))
  )

  (setq str1l (strlen str1))
  (setq sub1 (substr str1 1 1))
  (setq i 2)
  (while (< i str1l)
    (setq sub1 (substr str1 i 1))
    (if (= sub1 " ")
      (progn
	(setq part1 (substr str1 1 (- i 1)))
	(setq part2 (substr str1 (+ i 1) (- str1l i)))
	(setq i str1l)
      )
    )
    (setq i (+ 1 i))
  )
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;
