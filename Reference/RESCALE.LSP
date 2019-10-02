;;;;;;;;;;;;;;;;;;;;;;;RESCALE.LSP;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Purpose: Rescales Text to Viewport scale or Dimscale value
;; Author: Herman Mayfarth
;; h_mayfarth@tktn.com
;; Version: 1.2
;; Date: 7 Mar. 99
;;
;; Written for and tested with AutoCAD R13c4 for DOS
;; V1.1 tested with AutoCAD R14
;; V1.2 tested with AutoCAD 2000 beta T085
;;
;; Copyright (c) 1998,1999 by Herman Mayfarth
;; Provided "as is." No warranty, express or implied.
;; Permission granted to freely use and redistribute
;; without fee, provided copyright notice and disclaimer
;; of warranty remain intact. All other rights reserved.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Modification history:
;;
;; v1.1 added support for MTEXT ->default text ht *ONLY*
;; 26 Nov. 98 added entity counter to distinguish processed vs.
;; changed entities
;;
;; v1.2 corrected error handler and fixed deficiency in earlier
;; 7 March 99 version which did not trap arbitrary input correctly
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Usage: AutoCAD Release 13 & later, any platform
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Program Operation:
;;
;; Rescales TEXT and MTEXT objects to a "normalized" size based on
;; the value of DIMTXT, scaled by:
;;
;; 1. Value of DIMSCALE, if in tiled model space,
;; 2. Viewport scale, if in a floating viewport,
;; 3. One, if in paper space,
;; 4. A scale factor relative to "normal" text size, for which the
;; user is always prompted. The DIMTEXT value is considered to be
;; the "normal" text size, and all size choices are relative to
;; this value.
;;
;; The user is presented with a choice of pre-defined heights relative
;; to the normal size, described by keywords, as well as the choice of
;; typing any arbitrary scale factor. The sizes of the pre-defined heights
;; may be changed by altering numerical values in an association list
;; near the end of the file, which is assigned to the global variable
;; NTEXTSCALES if it does not exist. It will not overwrite an existing
;; value of this variable.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;;
;; Usage Notes:
;;
;; 1. This program makes no attempt to deal with absolute text heights in
;; MTEXT objects which differ from the default text height. To make
;; varying text heights in MTEXT rescalable by this program, always use
;; relative, not absolute, text heights in your MTEXT. Refer to the AutoCAD
;; documentation for information on this topic.
;;
;; 2. Users of NTEXT may already have the value of NTEXTSCALES initialized,
;; if they have loaded that program in the current drawing session. If you
use
;; NTEXT, it should *always* be loaded before this program, since it
;; unconditionally initializes the global variable NTEXTSCALES.
;; Users of NTEXT who may wish to change the pre-defined heights must
redefine
;; those heights using that program in order for it to function properly.
;; If you are a licensed user of NTEXT who wishes to do this, please
contact
;; the author for a method of doing so.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;
(defun C:RESCALE ( /
; local symbols
adj
e1
i
j
k
new
old
olderr
sadj
sf
ss1
txtht
; local functions
setscl
sing_pl
*lerror*
)

;;;--------------- Local Functions ---------------------------------
;;--------------------- setscl--------------------------------------
;; Purpose: calculate scale factor
;; Uses: nothing
;; Returns: sf
;;
;; local variables
;; curvpt ;current viewport (integer - not saved by drawing)
;;-------------------------------------------------------------------
(defun setscl( / curvpt )
(setq curvpt (getvar "CVPORT"))
(cond ((= 1 (getvar "TILEMODE")) (setq sf (getvar "DIMSCALE")));tiled
space
((= 1 curvpt) (setq sf 1.0));in paper space
;; in a viewport
(T
(setq sf (last (trans '(0 0 1.0) 3 2)));calc scale factor
);end default
);end cond
);end setscl

;;---------------------sing_pl---------------------------------------
;; Purpose: supplies correct suffix for singular vs. plural noun
;; Needs: string for root of noun
;; counter for quantity
;; Returns: correct string for quantity referenced
(defun sing_pl(root i / )
(if (/= i 1)
(strcat (substr root 1 (- (strlen root) 1)) "ies")
(substr root 1 (strlen root))
);if
);end sing_pl
;--------------------- lerror ---------------------------------------
;; local error handler
(defun *lerror* (msg)
(if
(or (= msg "Function cancelled")
(= msg "quit / exit abort")
)
(princ)
(princ (strcat "\nError: " msg))
)
(setq *error* olderr)
(command "_.UNDO" "END")
(setvar "CMDECHO" 1)
(princ)
);*lerror*
;-----------------------------------------------------------------
(setvar "CMDECHO" 0)
(command "_.UNDO" "BEGIN")
(setq olderr *error*
*error* *lerror*)

;; handle nonsense case of zero dimscale in tiled space
(if(and(= 0 (getvar "DIMSCALE"))(= 1 (getvar "TILEMODE")))
(progn((alert " Zero Dimension Scale\n Set Tilemode = 0 or
Use a Non-Zero Dimension Scale")(exit))))
;; calculate text scale factor
(setscl)

;; the program calculates a new text height based on a scale factor
;; which uses the value of DIMTXT as the "normal" text height
;; the following line sets the normal text height

(setq txtht (* (getvar "DIMTXT") sf))

;;prompt for new text height, relative to "normal" height
;;you can enter a predefined constant, or any arbitrary number, at the
prompt
;;the text will be rescaled to this value multiplied by the "normal" height
;;to change the value of the predefined constants, just edit the real
numbers
;;defined in the (setq NTEXTSCALES ...) statement near the end of the file

(setq adj nil)				;setup for while loop
(while (null adj)
  (prompt "\nWhat Size Text?")
  (initget 134 "Jumbo Medium Big Normal Small Tiny")
					;arbitrary OK, no neg or 0
  (setq	adj (getkword
	      "\nHeight: Jumbo Big Medium Small Tiny (or X
Normal):"   )
  )
  (if (null adj)
    (setq adj "Normal")
  )					;user pressed "return"
  ;;trap arbitrary non-numerical input
  (if (and (not	(member	adj
			'("Jumbo" "Big"	"Medium" "Normal" "Small" "Tiny")
		)
	   )
	   (= (atof adj) 0.0)
      )
    (progn
      (princ
	"\nInvalid Input. Use Defined Keyword or Valid Numerical Scale
Factor."
      )
      (setq adj nil)
    )					;progn
  )					;if
)					;while
(cond ((equal adj "Jumbo")
       (setq sadj (cdr (assoc 'Jumbo NTEXTSCALES)))
      )
      ((equal adj "Big")
       (setq sadj (cdr (assoc 'Big NTEXTSCALES)))
      )
      ((equal adj "Medium")
       (setq sadj (cdr (assoc 'Medium NTEXTSCALES)))
      )
      ((equal adj "Normal")
       (setq sadj (cdr (assoc 'Normal NTEXTSCALES)))
      )
      ((equal adj "Small")
       (setq sadj (cdr (assoc 'Small NTEXTSCALES)))
      )
      ((equal adj "Tiny")
       (setq sadj (cdr (assoc 'Tiny NTEXTSCALES)))
      )
      (T (setq sadj (atof adj)))
)					;cond
;;adjust text height
(setq txtht (* txtht sadj))
(princ "\nSelect Text/Mtext Entities to Change: ")
(setq ss1 (ssget '((-4
		    .
		    (0 . "MTEXT")
		    (0 . "TEXT")
		    (-4 . "OR>")
		   )
		  )
	  )
      (if (not (null ss1))
	(progn
	  (setq	i 0
		j (sslength ss1)
		k 0
	  )
	  (repeat j
	    (setq e1  (entget (ssname ss1 i))
		  old (assoc 40 e1)
	    )
	    (if	(not (= txtht (cdr old)))
	      (setq k	(1+ k)		;entity changed
		    new	(cons 40 txtht)
		    e1	(subst new old e1)
	      )
	    )				;if
	    (entmod e1)
	    (setq i (1+ i))		;entity processed
	  )				;repeat
	)				;progn
	(setq j 0)			;null set
      )					;if
       (if (= j 0)
	 (princ "\nNo Entities Selected.")
	 (princ	(strcat	"\n"
			(itoa j)
			" "
			(sing_pl "Entity" j)
			" Processed, "
			(itoa k)
			" "
			(sing_pl "Entity" k)
			" Changed. Selected Text Height is "
			(rtos
			  txtht
			)
		)
	 )
       )				;if
      (setq *error* olderr)
       (command "_.UNDO" "END")
      (setvar "CMDECHO" 1)
       (princ)
)					;end rescale

;;initialize NTEXTSCALES (global)
;;if it does not exist

(if (not NTEXTSCALES)
  (setq	NTEXTSCALES
	 (list '(JUMBO . 2.0)		;may change if you wish
	       '(BIG . 1.5)		;refer to Usage Notes
	       '(MEDIUM . 1.25)		;for co-operation with NTEXT
	       '(NORMAL . 1.0) '(SMALL . 0.75) '(TINY . 0.5)) ;list
  )					;setq
)					;if
;;load prompt
(princ "\nRESCALE V1.2 (c) 1999 by Herman Mayfarth...")
(princ)