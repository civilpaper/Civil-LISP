;(Defun rtd (A)
;	(/ (* A 180) Pi)
;)
;(Setq AGL (Getorient " \nChoose two X,Y,Z coordinates: "))
;(Prompt "\nThe absolute angle is: ")(rtd AGL)
;****************************************************************************************
;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(Defun C:DISTONLY ( / pt1 dst)
  (Setq pt1 (Getpoint "\n Pick a starting point in your drawing:")
             pt2 (Getpoint "\n Pick a second point in your Drawing:")
)
(Prompt "\n The specified diastance is ")
(Distance pt1 pt2) 
(Prompt " This Lisp Written by Mr Tuan Anh")
(Princ)
)
;************************************************************************************
;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(Defun C:CDiem ()
  (Setvar "CMDECHO" 0 )
(Setq DORSIZ (Getdist "\nSpecify door width: "))
(Setvar "OSMODE" 32)
(Setq PX (Getpoint "\nPick intersection: "))
(Setvar "OSMODE" 512)
(Setq P1 (Getpoint "\nPick opening start point (on same line):"))
(Setvar "LASTPOINT" P1)
(Setvar "OSMODE" 128)
(Setq P2 (Getpoint "\nPick second line:"))
(Setvar "OSMODE" 0)
(Setq ANG1 (angle PX P1)
           ANG2 (angle P1 P2)
           P3 (Polar P1 ANG1 DORSIZ)
           P4 (Polar P3 ANG2 (distance P1 P2))
)
(Command ".break" P1 P3
                    ".break" P2 P4
	  ".line" P1 P2 ""
	  ".line" P3 P4 ""
)
(Prompt " \nThis Lisp Written by Mr Tuan Anh")
(princ)
)
;****************************************************************************************
;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~	

;File name : MAKESLOT
;Note : two circle have same diameter.
(Defun C:MAKESLOT (/ NP1 NP2 CP1 CP2 ANGL  ANGW CRAD LP1 LP2 OP1 TP1 TP2)
(Setvar "OSMODE" 512)
(Setq NP1 (Getpoint "\n Pick a point on the first circle:")
           NP2  (getpoint "\npick a point on the second circle:")
)
(Setvar "OSMODE" 0)
(Setq CP1 (osnap NP1 "CEN")
           CP2 (osnap NP2 "CEN")
           ANGL (angle CP1 CP2)
           ANGW (+ (* 0.5 Pi) ANGL)
           CRAD (Distance CP1 NP1)
           LP1 (Polar CP1 ANGW CRAD)
           LP2 (Polar LP1 ANGL (Distance CP1 CP2))
)
(Command ".line" LP1 LP2 ""
                     ".select" "last" ""
)
(Setq OP1 (osnap LP1 "MID"))
(Command ".offset" (* 2 CRAD) OP1 CP1"")
(Setq TP1 (Polar CP1 ANGL CRAD)
           TP2 (Polar CP2 (+ pi ANGL) CRAD)
)
(Command ".trim" "p" "l" "" TP1 TP2 "")
(Prompt "\n Done. \n")
(Prompt" This Lisp Written by Mr Tuan Anh")
(princ)
)
;***************************************************************************************************
;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(Defun C:PERPL (/ SA SB SNP OM OS PT1 PT2)
     (Setvar "CMDECHO" 0)
     (Setq 
	SA (Getvar "snapang")
	SB (Getvar "snapbase")
	SNP (Getvar "snapmode")
	OM (Getvar "othormode")
	OS (Getvar "osmode")
	PT1 (osnap (Getpoint "\n Pick point on line to draw perpendicular from:" ) "near")
     )
     (Setvar "osmode" 0)
     (Setq PT2 (Osnap PT1 "end"))
(if (equal PT1 PT2)
     (Setq PT2 (osnap PT1 "MID"))
)
(command ".snap" "r" PT1 PT2)
(Setvar "snapmode" 0)
(Setvar "orthomode" 1)
(Prompt "\n to point:")
(command ".line" PT1 pause "")
(Setvar "snapang" SA)
(Setvar "snapbase" SB)
(Setvar "snapmode" SNP)
(Setvar "orthomode" OM)
(Setvar "osmode" OS)
(Setvar "Cmdecho" 1)
(Princ)
)
;****************************************************************************************
;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;File name : tt
;Note : two circle have same or different diameter.
(prompt " \n Makes : to makeslot two circle  \n")
(Defun C:tt (/ NP1 NP2 CP1 CP2 ANGL  ANGW CRAD1 CRAD2 LP1 LP2  TP1 TP2)
(Setvar "OSMODE" 512)
(Setq NP1 (Getpoint "\n Pick a point on the first circle:")
           NP2  (getpoint "\npick a point on the second circle:")
)
(Setvar "OSMODE" 0)
(Setq CP1 (osnap NP1 "CEN")
           CP2 (osnap NP2 "CEN")
           ANGL (angle CP1 CP2)
           ANGW (+ (* 0.5 Pi) ANGL)
           CRAD1 (Distance CP1 NP1)
           LP1 (Polar CP1 ANGW CRAD1)
           CRAD2 (Distance CP2 NP2)
           LP2 (Polar CP2 ANGW CRAD2)
)
(Command ".line" "tan" LP1 "tan" LP2 ""
                     ".select" "last" ""
                     ".mirror" "last" "" CP1 CP2 "N"
)
(Setq TP1 (Polar CP1 ANGL CRAD1)
           TP2 (Polar CP2 (+ pi ANGL) CRAD2)
)
(Command ".trim" "p" "last" "" TP1 TP2 "")
(Prompt "\n Done. \n")
(Prompt" This Lisp Written by Mr Tuan Anh")
(Princ)
)
;*********************************************************************
;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(Defun C:ROOT(/ NUM R)
      (Initget 1)
      (Setq Num  (Getreal " \n Enter base number:"))
      (Setq R (Getreal "\n Enter Root level: "))
       
      (Prompt "\n The specified root is :")(exp (/ (Log NUM) R))
      )
      )
)
;***************************************************************************
;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(prompt " \n  CV : To draw line in circle\n")
(Defun C:CV (/ PT1 CPT DIS AGL DIV)
   (Setvar "CMDECHO" 0)
   (Setq PT1 (osnap (Getpoint "\n Select Circle: ") "NEA")
              CPT (osnap PT1 "Cen")
              DIS (distance PT1 CPT)
              AGL (Getstring "\n Angle for radial line: ")
              DIV (Getint "\n Number of equal  divisions: ")
)
(Command ".line" CPT (strcat "@" (rtos DIS 2 8) "<" AGL) ""
                     ".array" "L" "" "p" CPT DIV 360 "y"
)
(Setvar "CMDECHO" 1)
(Princ)
)
;***************************************************************************
;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;Purpose : Insert Date over Screen.
(Defun C:DATE( / TODAY YEAR MONTH DAY MDY INSPT HGT)
   (Setvar "cmdecho" 0)
   (Setq TODAY (rtos (Getvar "CDATE") 2 0)
              YEAR (Substr TODAY 1 4)
              MONTH (substr TODAY 5 2)
              DAY (substr TODAY 8 2)
              MDY (strcat " Today's day : " DAY "_" MONTH "_" YEAR)
              INSPT (Getpoint "\n Start point for text:")
          ;    HGT (Getreal "\n Text height:")
   )
   (Command ".text" INSPT 0.0 MDY)
   (princ)
)  	
;******************************************************************************************
;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;;(Prompt  "This Lisp Written by Mr Tuan Anh")
;(Defun C : ATEAM (/ ST M1 M2 M3 M4 LST)
;(Setvar "cmdecho" 0)
;(Setq ST "\nEnter Team Members "
    ;       M1 (Getstring T (strcat ST "1:"))
   ;        M2 (Getstring T (strcat ST "2:"))
    ;      M3 (Getstring T (strcat ST "3:"))
    ;       M4 (Getstring T (strcat ST "4:"))
    ;       LST (acad_strlsort (list M1 M2 M3 M4))
;   )
;   (princ "\nPick Text start point: ")
;	(Command ".text" pause)
;	(princ "\n Text height: ")
;	(command  pause 0.0 (car LST))
;(setq LST (cdr LST))
;	(command ".text" "" (car LST))
;(setq LST (cdr LST))
;	(command ".text" "" (car LST))
;(setq LST (cdr LST))
;	(command ".text" "" (car LST))
;(Setvar "cmdecho" 1)
;;(princ)
;)
;******************************************************************************************
;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(Prompt " Poly : to edit poly with")
(Defun C:po ()
   (Setvar "cmdecho" 0)
   (setq LW (getdist "\n Enter new width: "))
   (setq A 1)
   (prompt "\nSelect Polylines to change: ")
   (while (/= A nil)
     (progn
        (setq A (entsel))
        (if (/= A nil)
           (progn
	(setq B (entget (car A)))
	(setq C (cdr (assoc 40 B)))
	(princ C)
	(command "pedit" A "W" LW "")
	)
             )
         )
     )
(Prompt" This Lisp Written by Mr Tuan Anh")
(princ)
) 
;**********************************************************************************************
;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(Defun C:poly()
(if
     (and
          (not (prompt "\n Pick two point:"))
          (setq PT1 (getpoint))
          (setq PT2 (getpoint PT1))
     )
     (command ".POLYGON" "6" "E" PT1 PT2)
     (prompt "\n Two ponts must be picked!")
)
     (princ)
)

:************************************************************************************************
;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(defun C:PTBH (/ a b c delta X1 X2)
     (initget 3)
     (setq a (getreal "\n Enter value a:"))
     (setq b (getreal "\n Enter value b:"))
     (setq c (getreal "\n Enter value c:"))
     (setq delta (- (* b b) (* 4 a c)))
     (setq b (- 0 b))
     (if (< delta 0)	; Solving Quadratic Equation
        (princ "\n Quadratic Equation have no root:")
        (if (= delta 0)
          (progn
	(princ "\n Quadaric Equation have two twin root:")
	     (Princ "\n X1 = X2 =")
	     (princ (/ (+ b (sqrt delta)) 2 a))
          )
          (progn
	(princ "\n Quadratic Equation have two real root:")
	(Princ " \n X1 =")
	(princ (/ (+ b (sqrt delta)) 2 a))
	(Princ " \n X2 =")
	(princ (/ (- b (sqrt delta)) 2 a))
          )
        )
     )
(Command ".textscr")
(prompt " \n\n This lisp written by Mr Le Tuan Anh -  BHC - Tedi south")
(princ)
)
