
;;;Get properties on xref objects 

(defun c:demo (/ ent lst name sel xent xref)
  (if
    (and
      (setq sel (entsel "\nSelect a Xref: "))
      (setq ent (entget (car sel)))
      (assoc 2 ent)
      (= 4
	 (logand
	   4
	   (cdr
	     (assoc 70
		    (tblsearch "BLOCK" (setq name (cdr (assoc 2 ent))))
	     )
	   )
	 )
      )
    )
     (progn
       (setq xref (tblobjname "block" name))
       (while (setq xref (entnext xref))
	 (setq xent (entget xref)
	       lst  (cons (list
			    (cdr (assoc 410 xent))
			    ;; layout tab name
			    (cdr (assoc 0 xent))
			    ;; entity type
			    (cdr (assoc 8 xent))
			    ;; Layer name
			  )
			  lst
		    )
	 )
       )
       (if lst
	 (princ lst)
       )
     )
  )
  (princ)
)