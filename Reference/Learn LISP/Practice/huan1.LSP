(defun c:huan (/ FILTLST LFILTOBJ SAFECODE SAFEFILT)
  (vl-load-com)
  (setq	FILTLST	 (list
		   ;;name  Lay     col  lt   flags lw   plot
		   (list "Arch..Detail" "A-DETL*" "*" "*" 0 "*" "*")
		   (list "Arch..Annotation" "*NOTE*" "*" "*" 0 "*" "*")
		   (list "Arch..Dimensions" "*DIMS*" "*" "*" 0 "*" "*")
		   (list "Arch..Symbols" "*SYM*" "*" "*" 0 "*" "*")
		   (list "Arch..Sheet" "*WSHTT*" "*" "*" 0 "*" "*")

		   (list "Eng...Civil" "C-*" "*" "*" 0 "*" "*")
		   (list "Eng...Structural" "S-*" "*" "*" 0 "*" "*")
		   (list "Eng...Mechanical" "M-*" "*" "*" 0 "*" "*")
		   (list "Eng...Electrical" "E-*" "*" "*" 0 "*" "*")
		   (list "Eng...Plumbing" "P-*" "*" "*" 0 "*" "*")
		 )
	LFILTOBJ (vla-addobject
		   (vla-GetExtensionDictionary
		     (vla-Get-Layers
		       (vla-Get-ActiveDocument
			 (vlax-Get-Acad-Object)
		       )
		     )
		   )
		   "ACAD_LAYERFILTERS"
		   "AcDbDictionary"
		 )
	SAFECODE (vlax-make-safearray 2 '(0 . 6))
	SAFEFILT (vlax-make-safearray 12 '(0 . 6))
  )
  (vlax-safearray-fill SAFECODE (list 1 1 1 1 70 1 1))
  (foreach
	    SUBLST
		  FILTLST
    (vlax-safearray-fill
      SAFEFILT
      (mapcar '(lambda (X) (vlax-make-variant X 8)) SUBLST)
    )
    (vla-addxrecord LFILTOBJ (nth 0 SUBLST))
    (vla-setxrecorddata
      (vla-item LFILTOBJ (nth 0 SUBLST))
      SAFECODE
      SAFEFILT
    )
  )
  (prin1)
  (princ
    "\n* All Layer Filters have been created........... *"
  )
)