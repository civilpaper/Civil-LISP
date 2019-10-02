
;-----------------------------------------------------------------------------------------------------------
(defun block_u_ssget ( / ssl  nsset temp ed )
  (setq sset (ssget))
  (setq ssl (sslength sset) 
        nsset (ssadd)
  )
  (print ssl)
  (while (> ssl 0)
    (progn
      (setq temp (ssname sset (setq ssl (1- ssl))))
      (setq ed (entget temp))
      (if (= (DXF 0 ed) "INSERT") (ssadd temp nsset))
    )
  )
  (setq ssl (sslength nsset)
        sset nsset
  )
  (print ssl)
  (princ)
  (print)
);defun block_u_ssget

;-------------------------------------------Thay doi ty le chen cua block-----------------------------------

(defun c:bsca()
(block_u_ssget)
(setq ssl (sslength sset))
(if(> ssl 0)
 (progn
   (setq xsca(getreal "\nTy le chen phuong x:"))
   (setq ysca(getreal "\nTy le chen phuong y:"))
 );progn
);if
(while (> ssl 0)
  (progn
      (setq temp (ssname sset (setq ssl (1- ssl))))
      (setq ed (entget temp))
      (princ ed)
      (setq xold (nth (xdtt 41 ed) ed)
	    yold (nth (xdtt 42 ed) ed)
	    xnew (cons 41 xsca)
	    ynew (cons 42 ysca)
	    ed (subst xnew xold ed)
	    ed (subst ynew yold ed)
      )
      (entmod ed)    
      (princ xold)	
  );progn
 );while
);defun
;-------------------------------------------Thay doi ty le chen cua block-----------------------------------

(defun c:bsca()
(block_u_ssget)
(setq ssl (sslength sset))
(if(> ssl 0)
 (progn
   (setq xsca(getreal "\nTy le chen phuong x:"))
   (setq ysca(getreal "\nTy le chen phuong y:"))
 );progn
);if
(while (> ssl 0)
  (progn
      (setq temp (ssname sset (setq ssl (1- ssl))))
      (setq ed (entget temp))
      (princ ed)
      (setq xold (nth (xdtt 41 ed) ed)
	    yold (nth (xdtt 42 ed) ed)
	    xnew (cons 41 xsca)
	    ynew (cons 42 ysca)
	    ed (subst xnew xold ed)
	    ed (subst ynew yold ed)
      )
      (entmod ed)    
      (princ xold)	
  );progn
 );while
);defun