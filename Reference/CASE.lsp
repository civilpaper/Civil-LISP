;------------------- CASE.LSP    Simon Jones   1-9-87  ---------------
;---       Macro to convert a selection of text entities to        ---
;---       lower or upper case. Non-text entities will be ignored. ---

(defun C:CASE (/ x c e ss txt cmde)
  (setq cmde (getvar "CMDECHO"))
  (setvar "CMDECHO" 0)
  ; List keywords
  (initget "Upper Lower")
  (setq x (getkword "\nChange case to? Lower/<Upper>: "))
  ; Create a selection set of entities
  (setq ss (ssget))
  ; Initialise counter
  (setq c 0)
  (if ss (setq e (ssname ss c)))
  (while e
    (setq e (entget e))
    ; Ensure entity is text
    (if (= (cdr (assoc 0 e)) "TEXT")
	(progn
	   (if (= x "Lower")
		 ; Option to convert to lower case
		 (setq txt (strcase (cdr (assoc 1 e)) t))
		 ; Option to convert to upper case
		 (setq txt (strcase (cdr (assoc 1 e))))
	   )
	   (setq e (subst (cons 1 txt) (assoc 1 e) e))
	   (entmod e)
	)
    )
    (setq c (1+ c)) ; Increment counter.
    (setq e (ssname ss c))  ; Obtain next entity.
   )
   (setvar "CMDECHO" cmde)
   (princ)
)
