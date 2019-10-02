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

(defun DXF (code elist)
  (cdr (assoc code elist))
)

(defun deflt (str1 def)
  (strcat Str1 "<" (rtos def 2 3) ">: ")
) ;defun

(defun defltint	(str1 int)
  (strcat Str1 "<" (itoa int) ">: ")
) ;defun

(defun defltstr	(str1 def)
  (strcat Str1 "<" def ">: ")
) ;defun
  ;-------------------------------------------
(defun USTR (bit msg def spflag / inp nval)
  (if (and def (/= def ""))
    (setq msg (strcat "\n" msg " <" def ">: ")
	  inp (getstring spflag msg)
	  inp (if (= inp "")
		def
		inp
	      )
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
) ;defun USTR
  ;-----------------------------------------------------

(defun DtR (deg) ; converts degree to radian
  (/ (* deg pi) 180)
) ;defun

(defun RtD (rad) ; converts radian to degree
  (/ (* rad 180) pi)
) ;defun

(defun *error* (msg)
  (setvar "OSMODE" oslast)
  (princ msg)
  (princ)
) ;defun

(setvar "cmdecho" 0)

  ;-------------------------------------------------------------------
(prompt "\n DT2 : Calculating section properties.")
(prompt "\n Modified in 11/1999 - IX calculation added.")

(Defun c:dt2 (/ pt VSIZE)

;  (setq file (USTR 4 "Ten file ket qua (khong nhap phan mo rong)" file T))
  (setq tenf (getfiled "Nhap ten file ket qua" "d:\\cuong\\" "geo" 9))

  ;(setq file "C:\\HH")
;  (setq tenf (strcat file ".geo"))
  (setq fi (open tenf "a"))
  (write-line
    "MC \t Xc \t Yc \t F \t IZ \t IY \t IX \t Sx \t Sy \t Ymax \t Ymin \t P"
    fi
  )

  (if (not *count)
    (setq *count 1)
  )
  (setq
    count (getint (defltint "\nNhap STT mat cat dau tien : " *count))
  )
  (if (not count)
    (setq count *count)
    (setq *count count)
  )
  (setq oslast (getvar "OSMODE"))
  (if (= tl nil)
    (progn
      (setq tl (getreal "\nDrawing scale : "))
      (setq ntl (/ 1000 tl))
      (setq tl2 (* ntl ntl))
      (setq tl3 (* tl2 ntl))
      (setq tl4 (* tl3 ntl))
    )
  ) ;if TL

  (setq vsize (/ (getvar "VIEWSIZE") 5))
  (setq ST1 (getstring (defltstr "\nTen mat cat : " (itoa count))))
  (setvar "OSMODE" 2)
  ;  (setq Org (getpoint "\n Bam goc toa do cua mat cat :"))
  ;  (command "ucs" "O" Org)
  (setvar "OSMODE" 0)
  (setq pt1 (getpoint "\n Bam diem trong mat cat :"))
  (if (= ST1 "")
    (setq ST1 (itoa count))
    (setq count (read st1))
  )
  (setq X   nil
	  Y   nil
	  Ym1 nil
	  Ym2 nil
	  Ym3 nil
  )

  (while (/= pt1 nil)
    (command "-boundary" pt1 "")
    (setq ed (entget (entlast)))
    (command "erase" "l" "")
    (setq n 1)
    (while (< n (length ed))
      (if (= (car (nth n ed)) 10)
	(progn
	  (setq pt (nth n ed))
	  (setq X (append X (list (cadr pt))))
	  (setq Y (append Y (list (nth 2 pt))))
	)
      )
      (setq n (+ n 1))
    )
    (setq X (append X (list (car X))))
    (setq Y (append Y (list (car Y))))
    (setq F1   0.0
	  X1   0.0
	  y1   0.0
	  Jx1  0.0
	  Jy1  0.0
	  i    0
	  j    1
	  maxY (nth 0 Y)
	  minY (nth 0 Y)
    )

  ;----------------------------------------------tim maxY, minY
    (while (< j (length Y))
      (if (< maxY (nth j Y))
	(setq maxY (nth j Y))
      ) ;if max
      (if (> minY (nth j Y))
	(setq minY (nth j Y))
      ) ;if min
      (setq j (+ j 1))
    ) ; while
    (setq j 1)
  ;-------------
    (setq Ym1	(subst minY maxY Y) ;max2
	  Ym2	(subst maxY minY Y) ;min2
	  maxY2	(nth 0 Ym1)
	  minY1	(nth 0 Ym2)
    )

    (while (< j (length Ym1))
      (if (< maxY2 (nth j Ym1))
	(setq maxY2 (nth j Ym1))
      ) ;if max
      (if (and (> minY1 (nth j Ym2)) (> minY1 minY))
	(setq minY1 (nth j Ym2))
      ) ;if min
      (setq j (+ j 1))
    ) ; while
    (setq j 1)
  ;-------------------------
    (setq Ym3	(subst maxY minY1 Ym2) ;min2
	  minY2	maxY
    )
    (while (< j (length Ym3))
      (if (and (> minY2 (nth j Ym3)) (> minY2 minY1))
	(setq minY2 (nth j Ym3))
      ) ;if min
      (setq j (+ j 1))
    ) ; while
      ;----------------------------------------------tinh CHU VI
      (setq j 0)
      (setq sumdist 0.0)
      (while (< j (- (length X) 1))
         (setq ptd1 (list (nth j X) (nth j Y))
            ptd2 (list (nth (+ 1 j) X) (nth (+ j 1) Y))
         )
         (setq seg (distance ptd1 ptd2)
            sumdist (+ sumdist seg)
         )
         (setq j (+ j 1))
      ) ; while

    (setq j 1)

  ;----------------------------------------
    (while (< j (length X))
  ;----------------------------------------------tinh dien tich
      (setq
	F1 (+ F1 (* (- (nth i X) (nth j X)) (+ (nth i Y) (nth j Y))))
      )


  ;----------------------------------------------Tinh toa do trong tam theo phuong y
      (setq y1 (+ y1
		  (* (- (nth i X) (nth j X))
		     (+	(expt (nth i Y) 2)
			(* (nth i Y) (nth j Y))
			(expt (nth j Y) 2)
		     )
		  )
	       )
      )
  ;----------------------------------------------Tinh toa do trong tam theo phuong x
      (setq x1 (+ x1
		  (* (- (nth j Y) (nth i Y))
		     (+	(expt (nth i X) 2)
			(* (nth i X) (nth j X))
			(expt (nth j X) 2)
		     )
		  )
	       )
      )
  ;----------------------------------------------Tinh mo men quan tinh theo phuong x
      (setq Jx1	(+ Jx1
		   (* (- (nth i X) (nth j X))
		      (+ (expt (nth i Y) 3)
			 (* (expt (nth i Y) 2) (nth j Y))
			 (* (nth i Y) (expt (nth j Y) 2))
			 (expt (nth j Y) 3)
		      )
		   )
		)
      )
  ;----------------------------------------------Tinh mo men quan tinh theo phuong y
      (setq Jy1	(+ Jy1
		   (* (- (nth J Y) (nth i Y))
		      (+ (expt (nth i X) 3)
			 (* (expt (nth i X) 2) (nth j X))
			 (* (nth i X) (expt (nth j X) 2))
			 (expt (nth j X) 3)
		      )
		   )
		)
      )

      (setq i (+ i 1)
	    j (+ j 1)
      )
    ) ;while

    (setq pt2 (getvar "ucsorg"))
    (setq F (/ F1 2))
    (setq Yc2 (/ y1 (* 6 F)))
    (setq Xc2 (/ x1 (* 6 F)))
    (setq Yc (- Yc2 (cadr pt2)))
    (setq Xc (- Xc2 (car pt2)))
    (setq maxY (/ (- maxY (cadr pt2) Yc) ntl)
	  minY (/ (- minY (cadr pt2) Yc) ntl)
    )
    (setq maxY2	(/ (- maxY2 (cadr pt2) Yc) ntl)
	  minY2	(/ (- minY2 (cadr pt2) Yc) ntl)
    )
    (setq maxTB	(/ (+ maxY maxY2) 2.0)
	  minTB	(/ (+ minY minY2) 2.0)
    )
    (setq Sx (/ (* Yc F) tl3))
    (setq Sy (/ (* Xc F) tl3))
    (setq Jx (/ (- (/ Jx1 12) (* (expt Yc2 2) F)) tl4))
    (setq Jy (/ (- (/ Jy1 12) (* (expt Xc2 2) F)) tl4))
    (setq F (/ F tl2))
    (setq Jz (/ (expt F 4) 40 (+ Jx Jy)))
    (setq Yc (/ Yc ntl))
    (setq Xc (/ Xc ntl))
    (setq sumdist (/ sumdist ntl))

    (prompt "\n Area F      :")
    (princ (rtos F 2 6))
    (prompt "\n Perimeter P :")
    (princ (rtos sumdist 2 6))
    (prompt "\n Centroid :")
    (prompt "\n         + Yc:")
    (princ (rtos Yc 2 4))
    (prompt "\n         + Xc:")
    (princ (rtos Xc 2 4))
    (prompt "\n Moment of inertia :")
    (prompt "\n         + Sx:")
    (princ (rtos Sx 2 4))
    (prompt "\n         + Sy:")
    (princ (rtos Sy 2 4))
    (prompt "\n Principal moments about N/A :")
    (prompt "\n         + Jx:")
    (princ (rtos Jx 2 6))
    (prompt "\n         + Jy:")
    (princ (rtos Jy 2 6))
    (prompt "\n         + Jtors:")
    (princ (rtos Jz 2 6))
    (prompt "\n Furthest distances about N/A :")
    (prompt "\n         + Ymax:")
    (princ (rtos maxTB 2 6))
    (prompt "\n         + Ymin:")
    (princ (rtos minTB 2 6))
    (prompt "\n")

    //them
    (setq ptcen	(list (* Xc ntl) (* Yc ntl))
	  pttop	(list (* Xc ntl) (* (+ Yc maxTB) ntl))
	  ptbot	(list (* Xc ntl) (* (+ Yc minTB) ntl))
    )
    (command "insert" "centroid" ptcen "" "" "")
    (setq ptd (list (+ 50 (* Xc ntl)) (* Yc ntl)))

;    (command "DIM1" "VER" ptcen pttop ptd "")
;    (command "DIM1" "VER" ptcen ptbot ptd "")

    (if	(/= 0 (DXF 40 (tblsearch "STYLE" (getvar "TEXTSTYLE"))))
  ;    (command "text" "m" (list (car ptcen) (+ (cadr ptcen) 100)) "0" (strcat "F: " (rtos F 2 6) " Jx: " (rtos Jx 2 6)) )
  ;    (command "text" "m" (list (car ptcen) (+ (cadr ptcen) 100)) (/ vsize 6) "0" (strcat "F: " (rtos F 2 6) " Jx: " (rtos Jx 2 6)) )
      (command "text"
	       "m"
               (list (car ptcen) (+ (cadr pttop) 42))
	       "0"
	       (strcat "F:"
		       (rtos F 2 6)
		       " P:"
		       (rtos sumdist 2 6)
		       " IZ:"
		       (rtos Jx 2 6)
		       " IY:"
		       (rtos Jy 2 6)
		       " IX:"
		       (rtos Jz 2 6)
	       )
      )
      (command "text"
	       "m"
	       (list (car ptcen) (+ (cadr ptcen) 100))
	       (/ vsize 6)
	       "0"
	       (strcat "F:"
		       (rtos F 2 6)
		       " IZ:"
		       (rtos Jx 2 6)
		       " IY:"
		       (rtos Jy 2 6)
		       " IZ:"
		       (rtos Jz 2 6)
	       )
      )
    ) ;if
    (write-line
      (strcat ST1
  ;" F:"
	      "\t"
	      (rtos Xc 2 6)
	      "\t" ;" Xc:"
	      (rtos Yc 2 6)
	      "\t" ;" Yc:"
	      (rtos F 2 6)
	      "\t" ;" IZ:"
	      (rtos Jx 2 6)
	      "\t" ;" IY:"
	      (rtos Jy 2 6)
	      "\t" ;" IX:"
	      (rtos Jz 2 6)
	      "\t" ;" Sx:"
	      (rtos Sx 2 6)
	      "\t" ;" Sy:"
	      (rtos Sy 2 6)
	      "\t" ;" maxY:"
	      (rtos maxTB 2 6)
	      "\t" ;" minY:"
	      (rtos minTB 2 6)
              "\t" ; perimeter
              (rtos sumdist 2 6)

      )
      fi
    )
    (setq count (+ 1 count))
    (setq ST1 (getstring (defltstr "\nTen mat cat : " (itoa count))))
    (setvar "OSMODE" 2)
  ; (setq Org (getpoint "\n Bam goc toa do cua mat cat: <mid_of>"))
  ; (command "ucs" "O" Org)
    (setvar "OSMODE" 0)
    (setq pt1 (getpoint "\n Bam diem trong mat cat :"))
    (if	(= ST1 "")
      (setq ST1 (itoa count))
      (setq count (read st1))
    )
    (setq *count count)
    (setq X   nil
	  Y   nil
	  Ym1 nil
	  Ym2 nil
	  Ym3 nil
    )

  ) ;while
  (close fi)
  (princ)
  (setvar "OSMODE" OSLAST)
  (princ)
)




