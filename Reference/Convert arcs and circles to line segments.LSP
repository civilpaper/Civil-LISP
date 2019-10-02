;;; acl.lsp
;;; 11/6/05
;;; convert arcs and circles to line segments
;;; local defun

(defun arctolines (acsp obj n / cnt d leng p pts s x y)
  (setq d (/ (setq leng (vla-get-arclength obj)) n)
	s d)
  (setq cnt 0)
	(while (<= (- d s) leng)
	  (setq p (vlax-curve-getclosestpointto obj
		    (vlax-curve-getpointatdist obj (* s cnt))))
	  (setq pts (cons p pts))
	  (setq cnt (1+ cnt))
	  (setq d (+ d s))
	  )
  (mapcar (function (lambda (x y)(vlax-invoke acsp 'AddLine x y)))
	  pts (cdr pts)
	  )
  (vla-delete obj)
  )
;;local defun
(defun circletolines (acsp obj n / cnt d leng p pts s x y)
  (setq d (/ (setq leng (vla-get-circumference obj)) n)
	s d)
  (setq cnt 0)
	(while (<= (- d s) leng)
	  (setq p (vlax-curve-getclosestpointto obj
		    (vlax-curve-getpointatdist obj (* s cnt))))
	  (setq pts (cons p pts))
	  (setq cnt (1+ cnt))
	  (setq d (+ d s))
	  )
  (setq pts (cons (vlax-curve-getclosestpointto obj
		    (vlax-curve-getpointatdist obj 0.0))
		  pts)
	)
  (mapcar (function (lambda (x y)(vlax-invoke acsp 'AddLine x y)))
	  pts (cdr pts)
	  )
  (vla-delete obj)
  )

;;main part 
(defun C:ACL (/ acapp acsp adoc obj sset)
(vl-load-com)
  (or acapp
      (setq acapp (vlax-get-acad-object))
  )
  (or adoc
      (setq adoc (vla-get-activedocument acapp))
  )
(or acsp
      (setq acsp (if (= (getvar "CVPORT") 1)
		   (vla-get-paperspace
		     adoc)
		   (vla-get-modelspace
		     adoc)
		   )
	    )
      )
(vla-endundomark adoc)
(vla-startundomark adoc)


(setq sset (ssget "_X" '((0 . "ARC,CIRCLE"))))
(vlax-for obj (vla-get-activeselectionset adoc)
(if
  (eq "AcDbArc" (vla-get-objectname obj))
  (arctolines acsp  obj 12);<-- 12 is number of segments
  (circletolines acsp  obj 12);<-- 12 is number of segments
  )  
)

(vla-endundomark adoc)
(princ)
)
(princ "\n >> Start command with ACL")
(princ)