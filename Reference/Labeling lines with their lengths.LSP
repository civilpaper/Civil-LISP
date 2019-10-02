;;;;;Labeling lines with their lengths
;;------------------------------------ DIMLP.LSP ----------------------------------;;

;; fixo () 2012 * all rights released
;; edited 3/3/12

(defun C:DIMLP (/      *error*	     acsp   adoc   ang	  curve	 deriv
		en     mid    mp     ppt1   ppt2   prex	  sset	 txh
		txt    txt1   txtpt1
	       )

  (defun *error* (msg)
    (vla-endundomark
      (vla-get-activedocument
	(vlax-get-acad-object)
      )
    )
    (cond
      ((or (not msg)
	   (member
	     msg
	     '("console break" "Function cancelled" "quit / exit abort")
	   )
       )
      )
      ((princ (strcat "\nError: " msg)))
    )

    (princ)
  )

  (setq	adoc (vla-get-activedocument
	       (vlax-get-acad-object)
	     )
	acsp (vla-get-block (vla-get-activelayout adoc))
  )


  (vla-startundomark adoc)

  (setq	txh  (getvar "dimtxt")

	prex (getvar "dimdec")

  )

  (while (not sset)

    (setq sset (ssget '((0 . "*LINE")))

    )
  )

  (while (setq en (ssname sset 0))

    (setq curve (vlax-ename->vla-object en))

    (setq txt (if (= (getvar "measurement") 0)

		(rtos (vla-get-length curve) 3 2)

		(rtos (vla-get-length curve) 2 prex)
	      )

    )

    (setq mid	(/ (abs	(- (vlax-curve-getendparam curve)
			   (vlax-curve-getstartparam curve)
			)
		   )
		   2.
		)

	  mp	(vlax-curve-getpointatparam curve mid)

	  deriv	(vlax-curve-getfirstderiv
		  curve
		  (vlax-curve-getparamatpoint curve mp)
		)
    )

    (if	(zerop (cadr deriv))
      (setq ang 0)
      (setq ang (- (/ pi 2) (atan (/ (car deriv) (cadr deriv)))))
    )

    (if	(< (/ pi 2) ang (* pi 1.5))
      (setq ang (+ pi ang))
    )
    (setq ppt1 (polar mp (+ ang (/ pi 2)) (* txh 0.5))
    )

    (setq txtpt1 (vlax-3d-point (trans ppt1 1 0)))

    (setq txt1 (vla-addtext acsp txt txtpt1 txh))

    (vla-put-alignment txt1 acalignmentbottomcenter)

    (vla-put-textalignmentpoint txt1 txtpt1)

    (vla-put-insertionpoint
      txt1
      (vla-get-textalignmentpoint txt1)
    )

    (vla-put-rotation txt1 ang)

    (ssdel en sset)

  )

  (*error* nil)

  (princ)
)
(princ "\n\t---\tStart command with \"DIMLP\"\t---")
(princ)
(or (vl-load-com)
    (princ)
)
;;------------------------------------ code end ----------------------------------;;

