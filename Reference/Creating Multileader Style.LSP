;; original code by VVA
(defun make_mleader_style	(mleaderstylename
				 textcolor
				 leadercolor
				 /
				 adoc
				 mldrdict
				 newldrstyle
				 objcolor
				)
  (vl-load-com)
  (setq adoc (vla-get-activedocument (vlax-get-acad-object)))
  (setq	mldrdict
	 (vla-item (vla-get-dictionaries adoc) "ACAD_MLEADERSTYLE")
  ) ;_ end of setq
  (setq	newldrstyle
	 (vlax-invoke
	   mldrdict
	   'addobject
	   mleaderstylename
	   "AcDbMLeaderStyle"
	 ) ;_ end of vlax-invoke
  ) ;_ end of setq
  (setq	objcolor (vla-getinterfaceobject
		   (vlax-get-acad-object)
		   (strcat "AutoCAD.AcCmColor."
			   (substr (getvar "acadver") 1 2)
		   ) ;_ end of strcat
		 ) ;_ end of vla-getinterfaceobject
  ) ;_ end of setq
  (vla-put-colorindex objcolor textcolor)
  (vla-put-textcolor newldrstyle objcolor)
  (vla-put-colorindex objcolor leadercolor)
  (vla-put-leaderlinecolor newldrstyle objcolor)


  (foreach item
	   (list
	     '("AlignSpace" 4)
	     (list
	       "ArrowSize"
	       (fix (/ (vla-get-arrowsize (vla-item mldrdict "Standard"))
		       2
		    ) ;_ end of /
	       ) ;_ end of fix
	     ) ;_ end of list
	     '("BitFlags" 0)
	     '("BlockConnectionType" 1)
	     '("BlockRotation" 0.0)
	     '("BlockScale" 1.0)
	     '("BreakSize" 0.125)
	     '("ContentType" 2)		;mtext
	     '("Description" "My Style Description")
	     '("DoglegLength" 1.25)
	     '("DrawLeaderOrderType" 0)
	     '("DrawMLeaderOrderType" 1)
	     '("EnableBlockRotation" -1)
	     '("EnableBlockScale" -1)
	     '("EnableDogleg" -1)
	     '("EnableFrameText" 0)
	     '("EnableLanding" -1)
	     '("FirstSegmentAngleConstraint" 0)
	     (list "LandingGap"
		   (vla-get-landinggap (vla-item mldrdict "Standard"))
	     ) ;_ end of list
	     '("LeaderLineType" 1)
	     '("LeaderLineTypeId" "ByLayer")
	     '("LeaderLineTypeId" "ByLayer")
	     '("LeaderLineWeight" -3)
	     '("MaxLeaderSegmentsPoints" 2)
	     '("ScaleFactor" 1.0)
	     '("SecondSegmentAngleConstraint" 0)
	     '("TextAlignmentType" 0)
	     '("TextAngleType" 0)
	     '("TextHeight" 1.5)
	     '("TextLeftAttachmentType" 4) ;original 3
	     '("TextRightAttachmentType" 4);original 3
	     '("TextString" "Default\\PText")
	     '("TextStyle" "STANDARD")
	   ) ;_ end of list

    (vlax-put newldrstyle (car item) (cadr item))
  ) ;_ end of foreach
  newldrstyle
) ;_ end of defun

;; Test
(defun c:msv (/ *error* ms)
  (vl-load-com)

  (defun *error* (msg)
    (command "_undo" "_e")

    (if	(and msg
	     (not (wcmatch (strcase msg) "*BREAK,*CANCEL*,*EXIT*"))
	) ;_ end of and
      (princ (strcat "\nError: " msg))
    ) ;_ end of if
    (princ)
  ) ;_ end of defun
  (command "_undo" "_be")

  (if (vl-catch-all-error-p
	(vl-catch-all-apply
	  '(lambda ()
	     (setq ms (make_mleader_style "MyCoolstyle" 5 72));change "MyCoolstyle" and colors to suit
	   ) ;_ end of lambda
	) ;_ end of vl-catch-all-apply
      ) ;_ end of vl-catch-all-error-p
    (alert "Problem creating Mleader Style")
    (setvar "CMLEADERSTYLE" "MyCoolstyle");change "MyCoolstyle"  to suit
  ) ;_ end of if
  (if (vl-catch-all-error-p
	(vl-catch-all-apply
	  '(lambda () (vla-put-arrowsymbol ms "_Origin2"));; existing arrow block in the drawing
	) ;_ end of vl-catch-all-apply
      ) ;_ end of vl-catch-all-error-p
    (not
      (vl-catch-all-error-p
	(vl-catch-all-apply
	  '(lambda () (vla-put-arrowsymbol ms acarrowdefault))
	) ;_ end of vl-catch-all-apply
      ) ;_ end of vl-catch-all-error-p
    ) ;_ end of not
  ) ;_ end of if
  (princ)
) ;_ end of defun