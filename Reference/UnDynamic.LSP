(defun c:UnDynamic

		   (/		       _get_item
		    _right	       _make_key
		    _dynamic->static_block
		    _get_locked	       _get_dynamic_inserts
		    _main
		   )

  (defun _get_item (collection key / item)
    (vl-catch-all-apply
      '(lambda () (setq item (vla-item collection key)))
    )
    item
  )

  (defun _right	(str n / len)
    (if	(< n (setq len (strlen str)))
      (substr str (1+ (- len n)))
      str
    )
  )

  (defun _make_key (collection prefix len / key)
    ((lambda (i pad)
       (while
	 (_get_item collection
		    (setq key
			   (strcat prefix
				   (_right
				     (strcat pad (itoa (setq i (1+ i))))
				     len
				   )
			   )
		    )
	 )
       )
       key
     )
      0
      ((lambda (pad)
	 (while	(< (strlen pad) len)
	   (setq pad (strcat "0" pad))
	 )
	 pad
       )
	""
      )
    )
  )

  (defun _dynamic->static_block	(blocks insert len)
    (vla-ConvertToStaticBlock
      insert
      (_make_key blocks (strcat (vla-get-EffectiveName insert) "_") len)
;;;      (_make_key blocks "STATIC_" len) ;;;; original settings
    )
  )

  (defun _get_locked (layers / locked)
    (vlax-for layer layers
      (if (eq :vlax-true (vla-get-lock layer))
	(setq locked (cons layer locked))
      )
    )
    locked
  )

  (defun _get_dynamic_inserts (blocks / inserts)
    (vlax-for block blocks
      (vlax-for	object block
	(if (eq "AcDbBlockReference" (vla-get-objectname object))
	  (if (eq :vlax-true (vla-get-isdynamicblock object))
	    (setq inserts (cons object inserts))
	  )
	)
      )
    )
    inserts
  )

  (defun _main (document / blocks inserts locked len)
    (if
      (setq inserts
	     (_get_dynamic_inserts
	       (setq blocks (vla-get-blocks document))
	     )
      )
       (progn
	 (foreach layer
		  (setq locked (_get_locked (vla-get-layers document)))
	   (vla-put-lock layer :vlax-false)
	 )
	 (setq len (strlen (itoa (length inserts))))
	 (foreach insert inserts
	   (_dynamic->static_block blocks insert len)
	 )
	 (foreach layer	locked
	   (vla-put-lock layer :vlax-true)
	 )
       )
    )
    (princ)
  )

  (_main (vla-get-activedocument (vlax-get-acad-object)))

)