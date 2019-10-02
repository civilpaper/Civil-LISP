					;*************************************************************************

					;                  Write Text               WTEXT.LSP

					;     By Simon Jones      Autodesk Ltd, London      19 August 1987

					;  Macro for writing to an ascii text file using AutoCAD text entities.
					;  The function can be used in conjunction with LTEXT.LSP (ASCTEXT.LSP)

					;  If the file already exists the user can choose between appending, 
					;  overwriting or selecting an new file name.

					;**************************************************************************

(defun C:WTEXT (/ a c ce e f fl l s ss ss2 x ymax emax fname)

					; Store and set system variables
  (setq ce (getvar "CMDECHO"))
  (setq fl (getvar "FLATLAND"))
  (setvar "CMDECHO" 0)
					;(setvar "FLATLAND" 1)

					; Keep looping until an acceptable file name is found
  (while (null f)
;;;     ; Prompt for name of file to write too
;;;     (setq f (getstring "\nFilename to write to: ")
;;;           a (open f "r")    ; Test whether file already exists
;;;     )
    (setq fname (getfiled "Save Text File As:" "" "txt" 1))
    (setq f (open fname "w"))
					; option if file already exists
    (if	a
      (progn
					; Close file
	(close a)
					; "\42" is the octal code for speech marks
	(prompt
	  (strcat "\nFile \42" (strcase f) "\42 already exists.")
	)
	(initget "Append Overwrite New")
	(setq s (getkword "\nAppend/Overwrite or <New name>: "))
	(cond ((= s "Append") (setq f (open f "a")))
	      ((= s "Overwrite") (setq f (open f "w")))
	      (T (setq f nil))
	)
      )
;;;       ; open file to write to
;;;       (setq f (open f "w"))
    )
;;;  )



					; Create selection set of entities to process
    (while (null ss) (setq ss (ssget)))

					; This double loop orders the selection set so that the text entities
					; are processed in descending order according to their Y position in
					; the drawing. This ensures that entities selected using a window are
					; written to the file in a logical order. If two or more text entities
					; share the same Y position then the first picked takes priority.

					; Loop unil the selection set is empty
    (setq k 0)
    (while (setq e (ssname ss 0))
      (setq c 0)
					; Process all entities left in selection
      (while e
	(setq el (entget e))
					; Check that the entity is TEXT
	(if (= (cdr (assoc 0 el)) "TEXT")
	  (progn
					; Obtain Y value of text insertion point
	    (setq y (caddr (assoc 10 el)))
	    (if	(or (null ymax) (> y ymax))
					; Record highest entity and position
	      (setq ymax y
		    emax e
	      )
	    )
	  )
					; Remove non-text entities from selection set
	  (ssdel e ss)
	)
					; Index counter

	(setq c	(1+ c)
	      e	(ssname ss c)
	)
      )

					; Write next line to file (checking first that EMAX was set)
      (if emax
	(progn
	  (prompt (strcat "\rSorting - " (itoa (setq k (1+ k)))))
	  (write-line (cdr (assoc 1 (entget emax))) f)
					; Remove entity from selection set
	  (ssdel emax ss)
	)
      )
      (setq ymax nil)
    )
    (if	(null emax)
      (prompt "\nNo text entities found. ")
    )
					; Close file
    (close f)
					; Restore system variables back to their original forms
    (setvar "CMDECHO" ce)
    (setvar "FLATLAND" fl)
    (princ)
  )


  (setvar "modemacro"
	  (strcat
	    "Time:$(substr,$(getvar,cdate),10,2)"
	    " :$(substr,$(getvar,cdate),12,2)"
	    ", T:$(substr,$(getvar,thickness),1,8)"
	    ", W:$(substr,$(getvar,plinewid),1,8)"
	    ", U:$(substr,$(getvar,ucsname),1,8)"
	    ", C:$(substr,$(getvar,cecolor),1,3)"
	    "$(IF,$(GETVAR,ORTHOMODE),_O, )"
	    "$(if,$(getvar,snapmode),_S, )"
	   )
  )
)