;;; This Lisp convert text and mtext to MLeader
(defun c:t2ml (/) (c:MLeaderToExistingtext))
(defun c:MLeaderToExistingtext (/)
(vl-load-com)
  (cond
    ;;Select the text/mtext objects
    ((or
       (null (setq ss1 (ssget ":S" '((0 . "text,mtext")))))
       (= 0 (setq ssl (sslength ss1)))
     )
     nil				;nothing selected
    )
    (T
     (setq
       Textobj	(vlax-ename->vla-object (ssname ss1 0))
       ActSpace	(if (= 0 (getvar "cvport"))
		  (vla-get-paperspace
		    (vla-get-activedocument (vlax-get-acad-object))
		  )
		  (vla-get-modelspace
		    (vla-get-activedocument (vlax-get-acad-object))
		  )
		)
       StartPt	(getpoint "\nPick location for point of arrow head: ")
       txt	(vla-get-TextString Textobj)

       TextPt
		(vla-get-insertionpoint textobj)
       TextPt
		(vlax-variant-value TextPt)
       TextPt
		(vlax-safearray->list TextPt)
       ptlist
		(vlax-make-safearray
		  vlax-vbdouble
		  '(0 . 5)
		)
       ptlist
		(vlax-safearray-fill ptlist (append StartPt TextPt))
       MLObj
		(vla-addmleader
		  ActSpace
		  ptlist
		  'LeaderIndex
		)
     )
     (vla-put-textstring mlobj txt)
     (vla-delete Textobj)

    )
  )
)
