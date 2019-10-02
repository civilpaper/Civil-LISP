;; Utilities to clean-up scale lists
;; Version 2

;; Command to remove all XREF scale lists
(defun c:ScaleListRemXRef (/)
  (princ
    (strcat "\n"
	    (itoa (DelScaleListMatch "*XREF*" (GetScaleListEntities)))
	    " XREF scales deleted."
    ) ;_ end of strcat
  ) ;_ end of princ
  (princ)
) ;_ end of defun

;; Command to remove all Imperial scales
(defun c:ScaleListRemImperial (/)
  (princ
    (strcat "\n"
	    (itoa (DelScaleListMatch "*=*" (GetScaleListEntities)))
	    " Imperial scales deleted."
    ) ;_ end of strcat
  ) ;_ end of princ
  (princ)
) ;_ end of defun

;; Command to remove all Metric Scales
(defun c:ScaleListRemMetric (/)
  (princ
    (strcat "\n"
	    (itoa (DelScaleListMatch "*:*" (GetScaleListEntities)))
	    " Metric scales deleted."
    ) ;_ end of strcat
  ) ;_ end of princ
  (princ)
) ;_ end of defun

;; Command to ensure Imperial scales are present
(defun c:ScaleListImperial (/)
  (princ (strcat "\n" (itoa (InstallScaleList StdImplScaleList)) " scales added"))
  (princ)
) ;_ end of defun

;; Command to ensure ISO scales are present
(defun c:ScaleListMetric (/)
  (princ (strcat "\n" (itoa (InstallScaleList StdISOScaleList)) " scales added"))
  (princ)
) ;_ end of defun

;; Command to ensure Imperial scales are present
(defun c:ScaleListForceImperial (/)
  (princ "\nRemoving all non Imperial Scales ...")
  (princ
    (strcat "\n"
	    (itoa (DelScaleListMatch "*" (GetScaleListEntities)))
	    " Metric scales deleted."
    ) ;_ end of strcat
  ) ;_ end of princ
  (prompt "\nInstalling Standard Imperial scales ...")
  (c:ScaleListImperial)
  (princ)
) ;_ end of defun

;; Command to ensure ISO scales are present
(defun c:ScaleListForceMetric (/)
  (princ "\nRemoving all non Metric Scales ...")
  (princ
    (strcat "\n"
	    (itoa (DelScaleListMatch "*" (GetScaleListEntities)))
	    " Metric scales deleted."
    ) ;_ end of strcat
  ) ;_ end of princ
  (prompt "\nInstalling Standard Metric scales ...")
  (c:ScaleListMetric)
  (princ)
) ;_ end of defun

;; Command to remove incorrect scales & ensure standard scales
;; in relation to the MEASUREMENT sysvar
(defun c:ScaleListStandard (/)
  (if (= 0 (getvar "MEASUREMENT"))
    (c:ScaleListForceImperial)
    (c:ScaleListForceMetric)
  )
  (princ)
)


;; ----------------------------------
;; Utility functions                 
;; ----------------------------------

;; List of standard Imperial scales as per AutoCAD
(setq StdImplScaleList
       '(("1'-0\" = 1'-0\"" 12.0 . 12.0)
	 ("6\" = 1'-0\"" 6.0 . 12.0)
	 ("3\" = 1'-0\"" 3.0 . 12.0)
	 ("1-1/2\" = 1'-0\"" 1.5 . 12.0)
	 ("1\" = 1'-0\"" 1.0 . 12.0)
	 ("3/4\" = 1'-0\"" 0.75 . 12.0)
	 ("1/2\" = 1'-0\"" 0.5 . 12.0)
	 ("3/8\" = 1'-0\"" 0.375 . 12.0)
	 ("1/4\" = 1'-0\"" 0.25 . 12.0)
	 ("3/16\" = 1'-0\"" 0.1875 . 12.0)
	 ("1/8\" = 1'-0\"" 0.125 . 12.0)
	 ("3/32\" = 1'-0\"" 0.09375 . 12.0)
	 ("1/16\" = 1'-0\"" 0.0625 . 12.0)
	 ("1/32\" = 1'-0\"" 0.03125 . 12.0)
	 ("1/64\" = 1'-0\"" 0.015625 . 12.0)
	 ("1/128\" = 1'-0\"" 0.0078125 . 2.0)
	)
) ;_ end of setq

;; List of standard ISO scales (from ISO 13567)
(setq StdISOScaleList
       '(
	 ("1:1" 1.0 . 1.0)		;Scale type A
	 ("1:2" 1.0 . 2.0)		;
	 ("1:5" 1.0 . 5.0)		;Scale type B
	 ("1:10" 1.0 . 10.0)		;Scale type C
	 ("1:20" 1.0 . 20.0)		;Scale type D
	 ("1:25" 1.0 . 25.0)		;
	 ("1:50" 1.0 . 50.0)		;Scale type E
	 ("1:75" 1.0 . 75.0)		;
	 ("1:100" 1.0 . 100.0)		;Scale type F
	 ("1:200" 1.0 . 200.0)		;Scale type G
	 ("1:250" 1.0 . 250.0)		;
	 ("1:500" 1.0 . 500.0)		;Scale type H
	 ("1:750" 1.0 . 750.0)		;
	 ("1:1000" 1.0 . 1000.0)	;Scale type I
	 ("1:2000" 1.0 . 2000.0)	;Scale type J
	 ("1:2500" 1.0 . 2500.0)	;
	 ("1:5000" 1.0 . 5000.0)	;Scale type K
	 ("1:10000" 1.0 . 10000.0)	;
	)
) ;_ end of setq

;; Function to obtain list of scale entity names
(defun GetScaleListEntities (/ lst item)
  (setq lst nil)
  (foreach item	(dictsearch (namedobjdict) "ACAD_SCALELIST")
    (if	(= 350 (car item))
      (setq lst (cons (cdr item) lst))
    ) ;_ end of if
  ) ;_ end of foreach
  lst
) ;_ end of defun

;; Function to obtain list of scale types in current drawing
(defun GetScaleList (/ lst lst1 item data)
  (setq	lst  (GetScaleListEntities)
	lst1 nil
  ) ;_ end of setq
  (foreach item	lst
    (setq data (entget item))
    (setq lst1 (cons (vl-list* (cdr (assoc 300 data))
			       (cdr (assoc 140 data))
			       (cdr (assoc 141 data))
		     ) ;_ end of vl-list*
		     lst1
	       ) ;_ end of cons
    ) ;_ end of setq
  ) ;_ end of foreach

  ;; Sort the list - most detailed to least
  (setq	lst
	 (vl-sort lst1
		  '(lambda (s1 s2)
		     (> (/ (cadr s1) (cddr s1)) (/ (cadr s2) (cddr s2)))
		   ) ;_ end of lambda
	 ) ;_ end of vl-sort
  ) ;_ end of setq
) ;_ end of defun

(setq $scalelistdel nil)
;; Function to delete scale matching a wildcard name
(defun DelScaleListMatch (pattern lst / item data count)
  (setq count 0)
  (setq cmd (getvar "CMDECHO"))
  (setvar "CMDECHO" 0)
  (command "._undo" "_Begin")
  (command ".-SCALELISTEDIT")
  (foreach item lst
    (setq data (entget item))
    (if (wcmatch (cdr (assoc 300 data)) pattern)
      (progn
         ;; (entdel item) ;Removed
         (command "_Delete" (cdr (assoc 300 data)))
         (setq count (1+ count))
      ) ;_ end of progn
    ) ;_ end of if
  ) ;_ end of foreach
	(command "_Exit")
  (command "._undo" "_End")
  (setvar "CMDECHO" cmd)
  count
) ;_ end of defun

;; Function to ensure list of scale are installed
(defun InstallScaleList (stdlst / lst item cmd n e)
  (setq lst (GetScaleList))		;Get list of scale entity names

  ;; Remove items from stdlst which is already in the drawing
  (setq n 0)
  (while (< n (length stdlst))
    (setq e (nth n stdlst))
    (if (assoc (car e) lst)
      (setq stdlst (vl-remove e stdlst))
      (setq n (1+ n))
    )
  )

  (setq cmd (getvar "CMDECHO"))
  (setvar "CMDECHO" 0)
  (command "._undo" "_Begin")
  ;; Start scalelist edit if needed
  (if (> (length stdlst) 0)
    (command ".-scalelistedit")
  )
  ;; Step through remainder of stdlst
  (foreach item	stdlst
    (command "_Add" (car item) (strcat (rtos (cadr item)) ":" (rtos (cddr item))))
  ) ;_ end of foreach

  ;; End scalelist edit if needed
  (if (> (length stdlst) 0)
    (command "_Exit")
  )
  (command "._undo" "_End")
  (setvar "CMDECHO" cmd)
  (length stdlst)
)

(princ)
 ;|«Visual LISP© Format Options»
(72 2 40 2 T "end of " 60 9 0 0 0 T T nil T)
;*** DO NOT add text below the comment! ***|;
