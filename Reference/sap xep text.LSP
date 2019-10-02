;**********************************Arrange texts********************************
(defun c:sx( / sset ssl temp ed old new )
  (setq sset (ssget))
  (setq ssl (sslength sset)
	nsset (ssadd)
	)
  (print ssl)
  (princ "Entities found. ")
  (print)
  ;(text_u_ssget)
  (setq ssl (sslength sset))
  (if (> ssl 0)
    (setq pt1 (getpoint "Start point: ")))
  (setq kc (getdist "Distance between texts:"))
  (setq p1 (cadr pt1))
  (while (> ssl 0)
    (progn
      (setq temp (ssname sset (setq ssl (1- ssl))))
      (setq ed (entget temp))
      ;;;TEXT
      (if  (= (DXF 0 ed) "TEXT")
	(progn
	  (setq
	    old1 (nth (xdtt 72 ed) ed)
	    new1 (cons 72 0)
	    old (nth (xdtt 10 ed) ed)
	    old3 (nth (xdtt 73 ed) ed)
	    new3 (cons 73 0)
	    ed (subst new3 old3 ed)
	    new (list (nth 0 old) (nth 1 old) p1 (nth 3 old))
	    p1 (- p1 kc)
	    ed (subst new old ed)
	    ed (subst new1 old1 ed)
	    );setq
	  (entmod ed)
	  ) ;progn
	);if
      );progn
    );while
  (setq pt1 nil)
  (princ)
  );defun