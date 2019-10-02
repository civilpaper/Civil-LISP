(prompt "\n\nL O A D I N G    D R A W _ U T I . L S P, SEP. 2000")

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
(defun fopen ( fname mode msg / fh )
  (setq fh (open fname mode))
  (defun *error* (msg)
     (princ msg)
     (princ)
  )
  (while (= fh nil)
    (progn
       (Prompt (strcat "\nFile " msg " Not Found !"))
       (setq fname (USTR 1 (strcat "\nInput " msg " Again  ") nil T)
             fh (open fname mode)
       ) 
    )
  )
  fh
)
;------------------------------------------------------------------
(defun UDIST (bit kwd msg def bpt / inp)
  (if def
    (setq msg (strcat "\n" msg "<" (rtos def) ">: ")
          bit (* 2 (fix (/ bit 2)))
    )
    (if (= " " (substr msg (strlen msg) 1)) 
      (setq msg (strcat "\n" (substr msg 1 (1- (strlen msg))) ": "))
      (setq msg (strcat "\n" msg ": "))
    )
  )
  (initget bit kwd)
  (setq inp
    (if bpt
      (getdist msg bpt)
      (getdist msg)
    )
  )
  (if inp inp def)
)	
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
(defun UINT (bit kwd msg def / inp)
  (if def
    (setq msg (strcat "\n" msg "<" (itoa def) ">: ")
	  bit (* 2 (fix (/ bit 2)))
    )
    (if (= " " (substr msg (strlen msg) 1)) 
      (setq msg (strcat "\n" (substr msg 1 (1- (strlen msg))) ": "))
      (setq msg (strcat "\n" msg ": "))
    )
  )
  (initget bit kwd)
  (setq inp (getint msg))
  (if inp inp def)
)	
;------------------------------------------------------------------
(defun UREAL (bit kwd msg def / inp)
  (if def
    (setq msg (strcat "\n" msg "<" (rtos def 2) ">: ")
	  bit (* 2 (fix (/ bit 2)))
    )
    (if (= " " (substr msg (strlen msg) 1)) 
      (setq msg (strcat "\n" (substr msg 1 (1- (strlen msg))) ": "))
      (setq msg (strcat "\n" msg ": "))
    )
  )
  (initget bit kwd)
  (setq inp (getreal msg))
  (if inp inp def)
)	
;------------------------------------------------------------------
(defun UPOINT (bit kwd msg def bpt / inp)
  (if def
    (setq pts (strcat 
                 (rtos (car def)) "," (rtos (cadr def))
                 (if
                   (and (caddr def) (= 0 (getvar "FLATLAND")))
                   (strcat "," (rtos (caddr def))) 
                   ""
                 )
               )
          msg (strcat "\n" msg "<" pts ">: ")
          bit (* 2 (fix (/ bit 2)))
    ) 
    (if (= " " (substr msg (strlen msg) 1))
       (setq msg (strcat "\n" (substr msg 1 (1- (strlen msg))) ": "))
       (setq msg (strcat "\n" msg ": "))
    )
  )
  (initget bit kwd)
  (setq inp
    (if bpt
      (getpoint msg bpt)
      (getpoint msg)
    )
  )
  (if inp inp def)
) 
;------------------------------------------------------------------
(defun CMDECH (mode)
  (if (= mode 1)
    (setvar "CMDECHO" 0)
    (setvar "CMDECHO" 1) 
  )
  (graphscr)
  (princ)
)
;------------------------------------------------------------------
(defun DXF (code elist)
  (cdr (assoc code elist))
)
;------------------------------------------------------------------
(defun RAD (degree)
  (setq radian (/ (* degree 3.1416) 180))
  radian
)
;------------------------------------------------------------------
(defun DEG (Rad)
  (setq degree (/ (* rad 180) 3.1416))
  degree
)
;------------------------------------------------------------------
(defun OSMODE ( mode )
  ( if (= mode 1) 
       (setvar "OSMODE" #OSMODE) 
       (progn
          (setq #OSMODE (getvar "OSMODE"))
          (setvar "OSMODE" 0)
       )
  )
)
;------------------------------------------------------------------
(defun c:CUT( / pt1 pt2 pt3 pt4 pt5 pt6 x1 x2 y1 y2 dx dy xx1 yy1 
                xx2 yy2 xx3 yy3 xx4 yy4 a b)
  (prompt "\nExpansion symbol. ") 
  (OSMODE 0)
  (CMDECH 1)
  (if #dist #dist (setq #dist 0))
  (setq pt1 (getpoint "\nFirst point : ")
  	pt2 (getpoint "\nSecond point : " pt1)
  	#dist (abs (UDIST 0 "" "How big ?" #dist nil))
  	x1 (car pt1)
        y1 (cadr pt1)
	x2 (car pt2)
	y2 (cadr pt2)
        a (angle pt1 pt2)
        b (- (/ PI 2) a)
	dx (* (/ #dist 3) (cos a))
	dy (if (= (cos a) 0) (/ #dist 3) (* dx (/ (sin a) (cos a))))
        xx1 (- (/ (+ x2 x1) 2) (/ dx 2))
	yy1 (- (/ (+ y2 y1) 2) (/ dy 2))
	xx2 (+ (/ (+ x2 x1) 2) (/ dx 2))
	yy2 (+ (/ (+ y2 y1) 2) (/ dy 2))
	dx  (* (/ #dist 2) (cos b))
	dy  (* (/ #dist 2) (sin b))
	xx3 (- (/ (+ x2 x1) 2) dx)
	yy3 (+ (/ (+ y2 y1) 2) dy)
	xx4 (+ (/ (+ x2 x1) 2) dx)
	yy4 (- (/ (+ y2 y1) 2) dy)
	pt3 (list xx1 yy1)
	pt4 (list xx3 yy3)
	pt5 (list xx4 yy4)
	pt6 (list xx2 yy2)
  )
(command "PLINE" pt1 pt3 pt4 pt5 pt6 pt2 "")
(OSMODE 1)
) 
(prompt "\n CUT() : Draw expansion symbol")
;------------------------------------------------------------------
(defun c:st( / a pt1 pt2 pt3 pt4 x1 y1 times dist dl dx dy pt)
(prompt "\nBar sections, Welding line. ")
(cmdech 1)
(if #dia dia (setq #dia 20))
(if #inter #inter (setq #inter 200))
(if #kind #kind (setq #kind "Bar"))
(setq #kind (ukword 1 "Bar Siteweld Factoryweld" " Bar, weld in Site, weld in Factory " #kind)
      pt1 (getpoint "\nFrom...")
      pt2 (getpoint "\nTo..." pt1)
      #dia (abs (UDIST 0 "" "How big :" #dia nil))
      #inter (abs (UDIST 0 "" "Interval :" #inter nil))
      x1 (car pt1)
      y1 (cadr pt1)
      a (angle pt1 pt2)
      dist (distance pt1 pt2)
)
(setq times1 (/ dist #inter)
      times (fix (/ dist #inter))
)
(if (> (- times1 times) 0.5)
  (setq times (+ times 1))
)
(setq dl (/ dist times)
      dx (* dl (cos a))
      dy (* dl (sin a))
)
(setq #OSMODE (getvar "OSMODE"))
(setvar "OSMODE" 0)
(while (>= times 0)
  (setq x (+ x1 (* times dx))
      y (+ y1 (* times dy))
      pt (list x y))
  (if (= #kind "Bar")
     (command "DONUT" 0 #dia pt ""))
  (if (= #kind "Factoryweld")
     (command "LINE" pt (polar pt (+ a (/ PI 2)) #dia) ""))
  (if (= #kind "Siteweld")
     (progn
        (setq pt1 (polar pt (+ a PI) (/ #dia 2))
              pt2 (polar pt a (/ #dia 2))
              pt3 (polar pt1 (+ a (/ PI 4)) (* 1.414 #dia))
              pt4 (polar pt2 (+ a (/ (* 3 PI) 4)) (* 1.414 #dia))
        )      
        (command "LINE" pt1 pt3 "")
        (command "LINE" pt2 pt4 "")
     )
  )
  (setq times (1- times))
)
(setvar "OSMODE" #OSMODE)
)
(prompt "\n ST() : Bar sections")
;------------------------------------------------------------------
(defun c:st1( / a count pt1 pt2 x1 y1 dist dl dx dy pt)
(prompt "\nBar sections. ")
(cmdech 1)
(if #dia dia (setq #dia 20))
(if #times #Hmany (setq #times 2))
(setq pt1 (getpoint "\nFrom...")
      pt2 (getpoint "\nTo..." pt1)
      #dia (abs (UDIST 0 "" "Diameter of bar :" #dia nil))
      #times (UINT 4 "" "How many bars :" #times)
      x1 (car pt1)
      y1 (cadr pt1)
      a (angle pt1 pt2)
      dist (distance pt1 pt2)
      dl (/ dist (- #times 1))
      dx (* dl (cos a))
      dy (* dl (sin a))
      count (- #times 1)
)
(setq #OSMODE (getvar "OSMODE"))
(setvar "OSMODE" 0)
(while (>= count 0)
  (setq x (+ x1 (* count dx))
      y (+ y1 (* count dy))
      pt (list x y)
  )
  (command "DONUT" 0 #dia pt "")
  (setq count (- count 1))
)
(setvar "OSMODE" #OSMODE)
)
(prompt "\n ST1() : Bar sections ")
;------------------------------------------------------------------
(DEFUN C:CW( / ss1 emax count et ed en)
  (PROMPT "\nCHANGE WIDTH ! ")
  (PROMPT " \nSelect all Entities wanted to change... ")
  (IF #width #width (setq #width 0.5))
  (cmdech 1)
  (SETQ ss1 (ssget)
      emax (sslength ss1)
      count 0
      #width (UREAL 4 "" "Enter new width for all entities " #width)
  )
  (WHILE (< count emax)
    (setq en (ssname ss1 count)
        ed (entget en)
        et (cdr (assoc 0 ed))
    )
    (if (= et "LINE") (COMMAND "PEDIT" en "Y" "W" #width ""))
    (if (= et "PLINE") (COMMAND "PEDIT" en "W" #width ""))
    (setq count (1+ count))
  )
  (princ)
)
(prompt "\n CW() : Change the width of selected entities")
;------------------------------------------------------------------
(DEFUN C:HK( / pt1 pt2 pt3 pt4 da ang1 ang2 sidepoint)
  (PROMPT "\nDraw a hook. ")
  (cmdech 1)
  (if #diaa #diaa (setq #diaa 25))
  (setq pt1 (getpoint "\nBegin from :")
        pt2 (getpoint "\nDirection..." pt1)
        sidepoint (getpoint "\nSelect side ..." pt1)
        #diaa (abs (UDIST 0 "" "Enter diameter of hook :" #diaa pt1))
        ang1 (angle pt1 pt2)
        da (angle pt1 sidepoint) 
  )
  (setq #OSMODE (getvar "OSMODE"))
  (setvar "OSMODE" 0)
  (if (and (<= ang1 4.71239) (>= ang1 1.5708))
        (if (> da ang1)
           (setq ang2 (+ ang1 (/ PI 2)))
           (setq ang2 (- ang1 (/ PI 2)))
        )
  )
  (if (and (<= ang1 1.5708) (>= ang1 0))
         (if (or (> da 4.71239) (< da ang1))
             (setq ang2 (- ang1 (/ PI 2)))
             (setq ang2 (+ ang1 (/ PI 2)))
         )
   )
   (if (and (<= ang1 6.2832) (>= ang1 4.71239))
         (if (or (<= da 1.5708) (>= da ang1))
            (setq ang2 (+ ang1 (/ PI 2)))
            (setq ang2 (- ang1 (/ PI 2)))
         ) 
   )
  (setq pt3 (polar pt1 ang2 #diaa)
        pt4 (polar pt3 (+ ang1 PI) (* 1.5 #diaa))
  )
  (command "PLINE" pt1 "A" "D" pt2 pt3 "L" pt4 "")
  (setvar "OSMODE" #OSMODE)
) 
(prompt "\n HK() : Draw a hook ")
;------------------------------------------------------------------
(defun NS (x1 y1 x2 y2 x)
  (+ y1 (* (- x x1) (/ (float (- y1 y2)) (float (- x1 x2)))))
)
;------------------------------------------------------------------
(defun c:code( / ptlist pt a b n len pt1 ptc so pt2 count ptg)
  (if (= SCALE nil) (setq SCALE (UREAL 4 "" "Scale " SCALE)))
  (setq pt (upoint 0 "" "\nPick steel position" nil nil))
  (while (/= pt nil)
     (setq ptlist (append ptlist (list pt))
           pt (upoint 0 "" "\nPick steel position" nil pt)
     )
  )
  (setq pt1 (upoint 0 "" "\nFrom..." nil (nth 0 ptlist))
        pt2 (upoint 0 "" "\nTo..." nil pt1)
        len (distance (nth 0 ptlist) pt1)
        a (angle (nth 0 ptlist) pt1)
        b (angle pt1 pt2)
        n (length ptlist)
        ptc (polar pt2 b (* SCALE 1.75))
        count 0
  )
  (setq #OSMODE (getvar "OSMODE"))
  (setvar "OSMODE" 0)
  (while (< count n)
     (progn
        (setq pt (polar (nth count ptlist) a len)
              ptg (inters pt1 pt2 (nth count ptlist) pt seg)
        )  
        (command "LINE" (nth count ptlist) ptg "")
        (setq count (+ 1 count))
     )
  )
  (command "LINE" pt1 pt2 "")
  (if #dk #dk (setq #dk "00"))
  (setq str (strcat (rtos n 2 0) "$" (ustr 0 "Diameter " #dk T) " "))
  (setq so (ustr 0 "Number " "00" T))
  (if (>= (car pt2) (car pt1))
    (progn
     (command "TEXT" "J" "BR" pt2 (DEG b) str)
     (command "TEXT" "J" "M" ptc (DEG b) so)
    )
    (progn 
     (command "TEXT" "J" "BL" pt2 (DEG (angle pt2 pt1)) str)
     (command "TEXT" "J" "M" ptc (DEG (angle pt2 pt1)) so)
    )
  )
  (command "CIRCLE" ptc "D" (* 3.5 SCALE))
  (setvar "OSMODE" #OSMODE)
)
(prompt "\n CODE to code steels in a section")
;------------------------------------------------------------------
(defun c:btk( / fh ncol kch th i y ymin x width w dcx dcxlist colw line str)
  (setq DZ (getvar "DIMZIN"))
  (setvar "DIMZIN" 0)
  (if (= scale nil) (setq scale (getreal "\nInput current scale : ")))
  (setq kch (* 5.0 scale))
  (setq fn (getfiled "Select a data file" "d:\kl" "txt" 8)
        fh (fopen fn "r" fn)
        ps (getpoint "Diem bat dau:")
        kch (UREAL 0 "" " Khoang cach hang " kch)
        i 1
        y (cadr ps)
        x (car ps)
        width 0
  )

  (setq #OSMODE (getvar "OSMODE"))
  (setvar "OSMODE" 0)

  (setq str (read-line fh))
  (setq ncol (length (read (strcat "(" str ")"))))
  (setq w (* 20 scale))
  (setq dcx 0)
  (while (<= i ncol)
     (progn
        (setq chuoi (strcat "Be Rong Cot Thu " (rtos i 2 0) " ")
              w (UREAL 0 "" chuoi w)
              colw (append colw (list w))
              width (+ width w)
              chuoi (strcat "Do Chinh Xac Cua Cot Thu " (rtos i 2 0) " ")
              dcx (UINT 0 "" chuoi dcx)
              dcxlist (append dcxlist (list dcx))
              i (+ 1 i)
        )
     )
  )
  (while (/= str nil)
    (progn
       (setq line (read (strcat "(" str ")")))
       (setq i 0)
       (setq y (- y kch))
       (setq x (car ps))
       (while (< i ncol) 
         (progn
            (setq So (nth i line)
                  x (+ x (/ (nth i colw) 2.0))
                  pt (list x y)
                  x (+ x (/ (nth i colw) 2.0))
            )
            (if (/= (etos So) "O")
                (if (= (type So) 'SYM)
                   (command "TEXT" "J" "MC" pt "0" (etos So))
                   (command "TEXT" "J" "MC" pt "0" (rtos (float So) 2 (nth i dcxlist)))
                )
            )
            (setq i (+ i 1))
         )
       )
       (command "LINE" (list (car ps) (+ y (/ kch 2.0))) (list (+ (car ps) width) (+ y (/ kch 2.0))) "")
       (setq str (read-line fh))
    )
  )
  (setq y (- y (/ kch 2.0)))
  (setq ymin (- (cadr ps) (/ kch 2.0)))
  (command "LINE" (list (car ps) ymin) (list (car ps) y) (list (+ (car ps) width) y) "")
  (setq i 0
        x (car ps)
  )
  (while (< i ncol)
    (progn
       (setq x (+ x (nth i colw))
             i (+ i 1)
       )
       (command "LINE" (list x ymin) (list x y) "")
    )
  )
  (close fh)
  (setvar "DIMZIN" DZ)

  (setvar "OSMODE" #OSMODE)
)
(prompt " \n BTK : Import from text file to table")
;---------------------------------------------------------------------
(defun donut_get ( / ssl  nsset temp ed )
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
      (if (AND (= (DXF 0 ed) "LWPOLYLINE") (= (DXF 90 ed) 2) (= (DXF 70 ed) 1))
 	(ssadd temp nsset)
      )
    )
  )
  (setq ssl (sslength nsset)
        sset nsset
  )
  (print ssl)
  (princ "DONUT entities found. ")
  (princ)
);defun donut_get

;-------------------------------------------------------------

(prompt "\nDTB : Change Donut To CTHEP BLOCK")

(defun c:DTB ( / sset ssl temp new old cen X Y )
(donut_get)
(if (= scale nil) (setq scale (getreal "\nInput current scale : ")))
(setq ssl (sslength sset))
(setvar "ORTHOMODE" 0)
(while (> ssl 0)
	(progn
		(setq temp (ssname sset (setq ssl (1- ssl)))
			ed (entget temp)
		)	
		(setq n 1)
		(while (< n (length ed))
			(progn
				(if (= (car (nth n ed)) 10)
					(progn 
						(setq pt (nth n ed)) 
						(setq X (append X (list (cadr pt))))
						(setq Y (append Y (list (nth 2 pt))))
				 	) 
				)
				(setq n (+ n 1))
			)
     		)
		(setq cen (list (/ (+ (car X) (cadr X)) 2) (/ (+ (car Y) (cadr Y)) 2)))	
		(command "INSERT" "CTHEP" cen scale scale "0")
		(setq X nil Y nil)
		(entdel temp)
      )
)
(princ)
);defun
;-------------------------------------------------------------

(prompt "\nLE : LEADER")

(defun c:LE ( / pt1 pt2 )
	(setq pt1 (getpoint "\nPick first point"))
	(setq pt2 (getpoint "\nPick second point" pt1))
	(setq #OSMODE (getvar "OSMODE"))
	(setvar "OSMODE" 0)
	(command "LINE" pt1 pt2 "")
	(command "INSERT" "MT" pt1 scale scale pt2)
	(setvar "OSMODE" #OSMODE)
	(COMMAND "LINE" pt2)
)


;------------------------------------------------------------------
(prompt " \n BTK1 : Import from text file to table of quantity ")

(defun c:btk1( / fh ncol height th i y ymin x width w line str)
  (setq DZ (getvar "DIMZIN"))
  (setvar "DIMZIN" 0)
  (if (= scale nil) (setq scale (getreal "\nInput current scale : ")))
  (setq fn (getfiled "Select a data file" "" "txt" 8)
        fh (fopen fn "r" fn)
        ps (getpoint "Start point:")
        i 1
        y (cadr ps)
        x (car ps)
        width (* 10.0 scale)
	height (* 8.0 scale)
  )
  (command "INSERT" "KL" ps scale scale "0")


  (setq #OSMODE (getvar "OSMODE"))
  (setvar "OSMODE" 0)

  (setq str (read-line fh))
  ;(setq ncol (length (read (strcat "(" str ")"))))
  (setq ncol 8)   


  (setq str (read-line fh))
    
  (command "LINE" ps  (list (+ (car ps) (* width 9)) y) "")

  
  (while (/= str nil)  	(progn
  	(setq line (read (strcat "(" str ")")))
       	(setq i 0)
	(setq j 0)
	(setq ve_ct 0)
       	(setq y (- y (/ height 2)))
       	(setq x (car ps))
       	(while (< i ncol) 	(progn
            	(setq	So (nth i line)
                      	x (+ x (if (= i 1) width (/ width 2)))
                  	pt (list x y)
			x (+ x (if (= i 1) width (/ width 2)))
            	)
            	(if (/= (etos So) "O")
                	(if (= (type So) 'SYM)
			  	(if (= i 1)
				  	(insert_steel So line pt)
                   			(command "TEXT" "J" "MC" pt "0" (etos So))
				)
				(if (= i 2)
			  		(command "TEXT" "J" "MC" pt "0" (strcat "#" (rtos (float So) 2 (if (> i 5) 1 0))))
					(command "TEXT" "J" "MC" pt "0" (rtos (float So) 2 (if (> i 5) 1 0)))
				)
			  
               		)
            	)
            	(setq i (+ i 1))
       	))

	(setq y (- y (/ height 2)))
        (command "LINE" (list (car ps) y)   (list (+ (car ps) (* width 9)) y) "")
        (setq str (read-line fh))
  ))
;;;;;;;
  (setq ymin (cadr ps))
  (setq i 0
        x (car ps)
  )
  (command "LINE" (list x ymin) (list x y) "")
  (while (< i ncol)
    (progn
       (setq x (+ x (if (/= i 1) width (* width 2)))
             i (+ i 1)
       )
       (command "LINE" (list x ymin) (list x y) "")
    )
  )
  (close fh)
  (setvar "DIMZIN" DZ)

  (setvar "OSMODE" #OSMODE)
);defun btk1



;------------------------------------------------------------------
(prompt " \n BTK2 : Import from text file to table of quantity ")

(defun c:btk2( / fh ncol height th i y ymin x width w line str)
  (setq DZ (getvar "DIMZIN"))
  (setvar "DIMZIN" 0)
  (if (= scale nil) (setq scale (getreal "\nInput current scale : ")))
  (setq fn (getfiled "Select a data file" "" "txt" 8)
        fh (fopen fn "r" fn)
        ps (getpoint "Start point:")
        i 1
        y (cadr ps)
        x (car ps)
        width (* 10.0 scale)
	height (* 8.0 scale)
  )
  (command "INSERT" "KL2" ps scale scale "0")


  (setq #OSMODE (getvar "OSMODE"))
  (setvar "OSMODE" 0)

  (setq str (read-line fh))
  ;(setq ncol (length (read (strcat "(" str ")"))))
  (setq ncol 9)   


  (setq str (read-line fh))
    
  (command "LINE" ps  (list (+ (car ps) (* width 10)) y) "")

  
  (while (/= str nil)  	(progn
  	(setq line (read (strcat "(" str ")")))
       	(setq i 0)
	(setq j 0)
	(setq ve_ct 0)
       	(setq y (- y (/ height 2)))
       	(setq x (car ps))
       	(while (< i ncol) 	(progn
            	(setq	So (nth i line)
                      	x (+ x (if (= i 2) width (/ width 2)))
                  	pt (list x y)
			x (+ x (if (= i 2) width (/ width 2)))
            	)
            	(if (/= (etos So) "O")
                	(if (= (type So) 'SYM)
			  	(if (= i 2)
				  	(insert_steel2 So line pt)
                   			(command "TEXT" "J" "MC" pt "0" (etos So))
				)
				(if (= i 3)
			  		(command "TEXT" "J" "MC" pt "0" (strcat "#" (rtos (float So) 2 (if (> i 6) 1 0))))
					(command "TEXT" "J" "MC" pt "0" (rtos (float So) 2 (if (> i 6) 1 0)))
				)
			  
               		)
            	)
            	(setq i (+ i 1))
       	))

	(setq y (- y (/ height 2)))
        (command "LINE" (list (car ps) y)   (list (+ (car ps) (* width 10)) y) "")
        (setq str (read-line fh))
  ))
;;;;;;;
  (setq ymin (cadr ps))
  (setq i 0
        x (car ps)
  )
  (command "LINE" (list x ymin) (list x y) "")
  (while (< i ncol)
    (progn
       (setq x (+ x (if (/= i 2) width (* width 2)))
             i (+ i 1)
       )
       (command "LINE" (list x ymin) (list x y) "")
    )
  )
  (close fh)
  (setvar "DIMZIN" DZ)

  (setvar "OSMODE" #OSMODE)
);defun btk2
;--------------------

; Insert cot thep vao table
(defun insert_steel ( So line pt )

  (setq a (if (= (type (nth 8 line)) 'SYM) (etos (nth 8 line)) (nth 8 line))
	b (if (= (type (nth 9 line)) 'SYM) (etos (nth 9 line)) (nth 9 line))
	c (if (= (type (nth 10 line)) 'SYM) (etos (nth 10 line)) (nth 10 line))
  )
  (cond
	((= (etos So) "C3") (command "INSERT" "C" pt scale scale "0" c a b))
	((= (etos So) "L2") (command "INSERT" "L" pt scale scale "0" b a))
	((= (etos So) "D3") (command "INSERT" "D" pt scale scale "0" c b a))
	((= (etos So) "J2") (command "INSERT" "J" pt scale scale "0" b b a))
	((= (etos So) "U3") (command "INSERT" "U" pt scale scale "0" b c a))
	((= (etos So) "I1") (command "INSERT" "I" pt scale scale "0" a))
	((= (etos So) "Z3") (command "INSERT" "Z" pt scale scale "0" c a b))
	((= (etos So) "V3") (command "INSERT" "V" pt scale scale "0" c c b a a))
	((= (etos So) "S2") (command "INSERT" "S" pt scale scale "0" b a b))
  );cond  
		
);defun insert_steel
;---------------------------------------------------------------------

; Insert cot thep vao table
(defun insert_steel2 ( So line pt )

  (setq a (if (= (type (nth 9 line)) 'SYM) (etos (nth 9 line)) (nth 9 line))
	b (if (= (type (nth 10 line)) 'SYM) (etos (nth 10 line)) (nth 10 line))
	c (if (= (type (nth 11 line)) 'SYM) (etos (nth 11 line)) (nth 11 line))
  )
  (cond
	((= (etos So) "C3") (command "INSERT" "C" pt scale scale "0" c a b))
	((= (etos So) "L2") (command "INSERT" "L" pt scale scale "0" b a))
	((= (etos So) "D3") (command "INSERT" "D" pt scale scale "0" c b a))
	((= (etos So) "J2") (command "INSERT" "J" pt scale scale "0" b b a))
	((= (etos So) "U3") (command "INSERT" "U" pt scale scale "0" b c a))
	((= (etos So) "I1") (command "INSERT" "I" pt scale scale "0" a))
	((= (etos So) "Z3") (command "INSERT" "Z" pt scale scale "0" c a b))
	((= (etos So) "V3") (command "INSERT" "V" pt scale scale "0" c c b a a))
	((= (etos So) "S2") (command "INSERT" "S" pt scale scale "0" b a b))
  );cond  
		
);defun insert_steel
;---------------------------------------------------------------------





;------------------------------------------------------------------
(prompt " \n VD : VE COT THEP DAM ")

(defun c:VD( / HSCALE)

  (if (= SCALE nil) (setq SCALE (UREAL 4 "" "Scale " SCALE)))
  (setq HSCALE (getint "\nShrink percent (%)"))

  (setq #OSMODE (getvar "OSMODE"))
  (setvar "OSMODE" 0)

  
  (setq B (UREAL 4 "" "\nBeam Width=  " 20))
  (setq H (UREAL 4 "" "\nBeam Height=  " 30))
  (setq Lconsolt (UREAL 4 "" "\nLength of Left consol= " 0))
  (setq Lconsolp (UREAL 4 "" "\nLength of Right consol= " 0))

 
  (setq ncols (getint "\nNumber of Column= "))
  (setq colw (UREAL 4 "" "\nColumn Width=  " 20))


  (setq i 1)
  (setq bayw nil)
  
  (while (< i ncols)
    (progn
       (setq chuoi (strcat "\nBay Width No." (rtos i 2 0) " ")
              w (UREAL 0 "" chuoi 400)
              BayW (append BayW (list w))
              i (+ i 1)
       )
    )
  )

  (setq bpt (getpoint "\nPick Reference Point :"))

  (setq Col_Pt nil)
  (setq i 0)
  (setq w (car bpt))
  (setq ipt (list w (cadr bpt)))
  (setq Col_Pt (append Col_Pt (list ipt)))
  
  (while (< i (- ncols 1))
    (progn
      (setq w (+ w (nth i BayW)))
      (setq ipt (list w (cadr bpt)))
      (setq Col_Pt (append Col_Pt (list ipt)))
      (setq i (1+ i))
    )
  )

  
  
  ;-----Insert column---------------------------------------------------------------
  (setvar "CLAYER" "0")
  (setq i 0)
  (while (< i ncols)
    (progn
      (command "INSERT" "C20" (nth i Col_Pt) "1" "1" "0")
      (setq i (+ i 1))
    )
  )

  ;-----Draw Beam Boundary---------------------------------------------------------------
  (setq i 0)
  (while (< i (- ncols 1))
    (progn
      (setq pt1 (polar (nth i Col_Pt) 0 (/ colw 2)))
      (setq pt2 (polar (nth (+ i 1) Col_Pt) pi (/ colw 2)))
      (command "LINE" pt1 pt2 "")

      (setq pt1 (polar (polar (nth i Col_Pt) (/ (* pi 3) 2) H) 0 (/ colw 2)))
      (setq pt2 (polar (polar (nth (+ i 1) Col_Pt) (/ (* pi 3) 2) H) pi (/ colw 2)))
      (command "LINE" pt1 pt2 "")

      (setq i (+ i 1))
    )
  )
  ;-----Draw Beam Consol---------------------------------------------------------------
  (if (/= LconsolT 0)
    (progn
      (setq pt1 (polar (nth 0 Col_Pt) pi (/ colw 2)))
      (setq pt2 (polar (polar (nth 0 Col_Pt) (/ (* pi 3) 2) H) pi (/ colw 2)))      
      (setq pt3 (polar pt1 pi (- LconsolT (/ colw 2))))
      (setq pt4 (polar pt2 pi (- LconsolT (/ colw 2))))
      (command "LINE" pt1 pt3 pt4 pt2 "")
    )
    (progn
      (setq pt1 (polar (nth 0 Col_Pt) pi (/ colw 2)))
      (setq pt2 (polar (polar (nth 0 Col_Pt) (/ (* pi 3) 2) H) pi (/ colw 2)))      
      (command "LINE" pt1 pt2 "")
    )
  )

  (if (/= LconsolP 0)
    (progn
      (setq pt1 (polar (nth (- ncols 1) Col_Pt) 0 (/ colw 2)))
      (setq pt2 (polar (polar (nth (- ncols 1) Col_Pt) (/ (* pi 3) 2) H) 0 (/ colw 2)))      
      (setq pt3 (polar pt1 0 (- LconsolP (/ colw 2))))
      (setq pt4 (polar pt2 0 (- LconsolP (/ colw 2))))
      (command "LINE" pt1 pt3 pt4 pt2 "")
    )
    (progn
      (setq pt1 (polar (nth (- ncols 1) Col_Pt) 0 (/ colw 2)))
      (setq pt2 (polar (polar (nth (- ncols 1) Col_Pt) (/ (* pi 3) 2) H) 0 (/ colw 2)))      
      (command "LINE" pt1 pt2 "")
    )
  )

;-----Draw STIRRUP---------------------------------------------------------------
 (setvar "CLAYER" "3")
 (setq socotdai 0) 
 (setq n 0)
 
 (while (< n (- ncols 1))
    (progn
      (setq width (nth n BayW))
      (setq 1stcol_pt (nth n Col_pt))
      (setq 2rdcol_pt (nth (+ n 1) Col_pt))
      
      (setq 1st_pt (list  (+ (car 1stcol_pt) (* width 0.25)) (cadr 1stcol_pt)))
      (setq 2rd_pt (list  (+ (car 1stcol_pt) (* width 0.75)) (cadr 1stcol_pt)))

     ; Stirrup step 10cm
      (setq n10 (fix (/ width 40)))
      (setq step (/ (/ width 4) n10))
      (setq pt1 (list (car 1stcol_pt) (- (cadr 1stcol_pt) 3)))
      (setq pt11 (list (car 2rdcol_pt) (- (cadr 2rdcol_pt) 3)))
      (setq socotdai (+ socotdai (* n10 2)))
      (setq i 0)
      (while (< i n10)
	(progn
           (setq pt1 (polar pt1 0 step))
	   (setq pt2 (polar pt1 (- 0 (/ PI 2)) (- H 6)))
	   (command "line" pt1 pt2 "")

           (setq pt11 (polar pt11 PI step))
	   (setq pt21 (polar pt11 (- 0 (/ PI 2)) (- H 6)))
	   (command "line" pt11 pt21 "")

	   (setq i (+ i 1))
	)
      )

     ; Stirrup step 20cm
      (setq n20 (fix (/ width 40)))
      (setq step (/ (/ width 2) n20))
      
      (setq pt1 (list (+ (car 1stcol_pt) (/ width 4)) (- (cadr 1stcol_pt) 3)))

      (setq socotdai (+ socotdai n20))
      
      (setq i 0)
      (while (< i n20)
	(progn
           (setq pt1 (polar pt1 0 step))
	   (setq pt2 (polar pt1 (- 0 (/ PI 2)) (- H 6)))
	   (command "line" pt1 pt2 "")
	   (setq i (+ i 1))
	)
      )
      (setq n (+ n 1))
    )
  )

     ; Stirrup in left consol
  (if (/= LconsolT 0)
    (progn
      (setq width (- LconsolT (/ colw 2)))
      (setq n10 (fix (/ width 10)))
      (setq step (/ width n10))
      (setq 1stcol_pt (nth 0 Col_pt))

      (setq socotdai (+ socotdai n10))
      
      (setq pt1 (list (car 1stcol_pt) (- (cadr 1stcol_pt) 3)))

      (setq i 0)
      (while (< i n10)
	(progn
           (setq pt1 (polar pt1 PI step))
	   (setq pt2 (polar pt1 (- 0 (/ PI 2)) (- H 6)))
	   (command "line" pt1 pt2 "")
	   (setq i (+ i 1))
	)
      ) ;while i
    );progn
  );if

  (if (/= LconsolP 0)
    (progn
      (setq width (- LconsolP (/ colw 2)))
      (setq n10 (fix (/ width 10)))
      (setq step (/ width n10))
      (setq 1stcol_pt (nth (- ncols 1) Col_pt))

      (setq socotdai (+ socotdai n10))

      (setq pt1 (list (car 1stcol_pt) (- (cadr 1stcol_pt) 3)))

      (setq i 0)
      (while (< i n10)
	(progn
           (setq pt1 (polar pt1 0 step))
	   (setq pt2 (polar pt1 (- 0 (/ PI 2)) (- H 6)))
	   (command "line" pt1 pt2 "")
	   (setq i (+ i 1))
	)
      ) ;while i
    );progn
  );if
  (prompt "\nThe number of stirrups are ")
  (princ socotdai)
  
;-----Draw Longitudinal Bars---------------------------------------------------------------

  (if (/= LconsolT 0)
     (setq widthT (- LconsolT 3))
     (setq widthT (- (/ colw 2) 3))
  )  
  (if (/= LconsolP 0)
     (setq widthP (- LconsolP 3))
     (setq widthP (- (/ colw 2) 3))
  )


  (setq pt1 (polar (polar (nth 0 Col_pt) PI widthT) (- 0 (/ PI 2)) 3))
  (setq pt2 (polar pt1 (- 0 (/ PI 2)) (- H 6)))
  (setq pt3 (polar (polar (nth (- ncols 1) Col_pt) 0 widthP) (- 0 (/ PI 2)) 3))
  (setq pt4 (polar pt3 (- 0 (/ PI 2)) (- H 6)))

  (command "LINE" pt1 pt3 "")
  (command "LINE" pt2 pt4 "")

  (if (/= LconsolT 0)
     (command "INSERT" "ConsolEnd" pt1 "" "" "")
     (command "INSERT" "EndL" pt1 "" "" "")
  )  
  (if (/= LconsolP 0)
     (command "INSERT" "ConsolEnd" pt4 "" "" 180)
     (command "INSERT" "EndR" pt3 "" "" 0)
  )

 (setq n 0)
  
 (while (< n (- ncols 1))
    (progn
      (setq width (nth n BayW))

      (setq pt1 (polar (polar (nth n Col_pt) 0 (/ width 4)) (- 0 (/ PI 2)) 3))
      (setq pt2 (polar (polar (nth (+ n 1) Col_pt) PI (/ width 4)) (- 0 (/ PI 2)) 3))
      (setq pt3 (polar (polar pt1 (- 0 (/ PI 2)) (- H 6)) PI H))
      (setq pt4 (polar (polar pt2 (- 0 (/ PI 2)) (- H 6)) 0 H))

      (command "insert" "hkL" pt1 "" "" 0)
      (command "insert" "hkL" pt3 "" "" 180)

      (command "insert" "hkR" pt2 "" "" 0)
      (command "insert" "hkR" pt4 "" "" 180)
      (setq n (+ n 1))
   ) ;progn
 )	   
  
  
;-----Draw DIMENSIONS & TEXT---------------------------------------------------------------

 (setvar "CLAYER" "1")
 (setq n 0)
  
 (while (< n (- ncols 1))
    (progn
      (setq width (nth n BayW))
      (setq pt1 (polar (nth n Col_pt) (/ PI 2) (* scale 9)))
      (setq pt4 (polar (nth (+ n 1) Col_pt) (/ PI 2) (* scale 9)))

      (setq pt2 (polar pt1 0 (/ width 4)))
      (setq pt3 (polar pt4 PI (/ width 4)))

      (setq pt5 (polar pt1 (/ PI 2) (* scale 2)))
      (command "dim1" "hor" pt1 pt2 pt5 "")
      (command "dim1" "hor" pt2 pt3 pt5 "")
      (command "dim1" "hor" pt3 pt4 pt5 "")
      (setq pt5 (polar pt1 (/ PI 2) (* scale 6)))
      (command "dim1" "hor" pt1 pt4 pt5 "")

      
      (command "TEXT" "j" "m" (list (/ (+ (car pt1) (car pt2)) 2) (cadr pt1)) "0" "$6a100" )
      (command "TEXT" "j" "m" (list (/ (+ (car pt2) (car pt3)) 2) (cadr pt1)) "0" "$6a200" )
      (command "TEXT" "j" "m" (list (/ (+ (car pt3) (car pt4)) 2) (cadr pt1)) "0" "$6a100"  )
            
      (setq pt1 (polar (nth n Col_pt) (- 0 (/ PI 2)) (+ (* scale 8) H)))
      (setq pt4 (polar (nth (+ n 1) Col_pt) (- 0 (/ PI 2)) (+ (* scale 8) H)))
      
      (setq pt2 (polar pt1 0 (- (/ width 4) H)))
      (setq pt3 (polar pt4 PI (- (/ width 4) H)))

      (setq pt5 (polar pt1 (- 0 (/ PI 2)) (* scale 2)))

      (command "dim1" "hor" pt1 pt2 pt5 "")
      (command "dim1" "hor" pt2 pt3 pt5 "")
      (command "dim1" "hor" pt3 pt4 pt5 "")
    
      (setq n (+ n 1))
      
   );progn
 ) ;while 


 (if (/= LconsolT 0)
    (progn
      (setq pt1 (polar (nth 0 Col_pt) (/ PI 2) (* scale 9)))
      (setq pt2 (polar pt1 PI LconsolT))

      (setq pt5 (polar pt1 (/ PI 2) (* scale 2)))
      (command "dim1" "hor" pt1 pt2 pt5 "")
      (command "TEXT" "j" "m" (list (/ (+ (car pt1) (car pt2)) 2) (cadr pt1)) "0" "$6a100" )
     )
  )

 (if (/= LconsolP 0)
    (progn
      (setq pt1 (polar (nth (- ncols 1) Col_pt) (/ PI 2) (* scale 9)))
      (setq pt2 (polar pt1 0 LconsolP))

      (setq pt5 (polar pt1 (/ PI 2) (* scale 2)))
      (command "dim1" "hor" pt1 pt2 pt5 "")
      (command "TEXT" "j" "m" (list (/ (+ (car pt1) (car pt2)) 2) (cadr pt1)) "0" "$6a100" )
     )
  )

 (setq n 0)
 (while (< n (- ncols 1))
    (progn
      (setq width (nth n BayW))
      (setq pt1 (polar (polar (nth n Col_pt) 0 (/ width 8)) (- 0 (/ PI 2)) 3))
      (setq pt2 (polar (polar (nth n Col_pt) 0 (/ width 2)) (- 0 (/ PI 2)) 3))
      (setq pt3 (polar (polar (nth (+ n 1) Col_pt) PI (/ width 8)) (- 0 (/ PI 2)) 3))
      (setq pt4 (polar pt2 (- 0 (/ PI 2)) (- H 6)))

      (command "insert" "code1" pt1 scale "" "")
      (command "insert" "code2" pt2 scale "" "")
      (command "insert" "code3" pt3 scale "" "")
      (command "insert" "code4" pt4 scale "" "")

      (setq n (+ n 1))
    )
 )




  
  (setvar "OSMODE" #OSMODE)
  (princ)
);end of VD


