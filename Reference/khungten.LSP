;;;
;;;
;;;
(defun c:khungten (/ *Error* cd_dscale cd_attd cd_cmdecho)
;;; this is a locale error handler which will take
;;; care of resets if the users cancels. Provided
;;; courtesy of AcadX.com
  (defun *Error* (Msg)
    (cond
      ((or (not Msg)
	   (member Msg
		   '("console break"
		     "Function cancelled"
		     "quit / exit abort"
		    )
	   )
       )
      )
      ((princ (strcat "\nError: " Msg)))
    ) ;_ closes cond
					; re-sets go here
    (setvar "cmdecho" cd_cmdecho)
    (setvar "attdia" cd_attd)
    (princ)
  )
;;; Main function
;;; you can group setq's
  (setvar "dimscale" 1)
  (setq	cd_cmdecho (getvar "cmdecho")
	cd_attd	   (getvar "attdia")
	cd_dscale  (getvar "dimscale")
  )
;;; turn cmdecho off
  (setvar "cmdecho" 0)
  (command "_.xref" "A" "XREF" "0,0" cd_dscale "" "0")
  (command "_.zoom" "e")
  (setvar "attdia" 0)
;;; provide a message for the user
  (prompt "\nInsert point for Preliminary Stamp..?")
;;; initdia forces the dialog
  (initdia)
;;; use pause instead of getpoint. if the image is too big
;;; and the regen too slow go back to the getpoint but call it
;;; outside the command function
;;; Also, use -insert instead of insert
  (command "_.-insert" "TenBanVe" pause cd_dscale "" "0")
;;; wait until the dialog is closed
  (while (= (logand (getvar "CMDACTIVE") 8) 1)
    (command pause)
  )
;;;  (command "_.-insert" "att-b1" "0,0" cd_dscale "" "0")
;;; use the error function to re-set variables
  (*Error* nil)
;;; exit quietly, or not
  (princ "\nSee ya!")
  (princ)
)