;-------------------Doi chu hoa sang chu thuong va nguoc lai--------------------

(defun c:CA( / sset ssl temp ed old new )
(text_uu_ssget)
(initget 1 "U u L l")
(setq ssl (sslength sset))
	(if (> ssl 0) 
	 (setq sw (getkword "Loai chu can dieu chinh [U/L] : ")))
	  (while (> ssl 0)
	 ( if (or (= sw "U") (= sw "u"))
	    (progn
	      (setq temp (ssname sset (setq ssl (1- ssl))))
	      (setq ed (entget temp))

	       (setq old (cons 1 (DXF 1 ed))
	             new (cons 1 (strcase (DXF 1 ed)))
	             ed (subst new old ed)
	           )
	      (entmod ed)  
	      )  
	  );if
	 ( if (or (= sw "l") (= sw "L"))
	    (progn
	      (setq temp (ssname sset (setq ssl (1- ssl))))
	      (setq ed (entget temp))

	       (setq old (cons 1 (DXF 1 ed))
	             new (cons 1 (strcase (DXF 1 ed) T))
	             ed (subst new old ed)
	           )
	      (entmod ed)  
	      )  
	  );if
	);while

 (princ)
);defun CA