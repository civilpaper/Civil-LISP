;;;Creating Layers with all property (plot style, lw, discription, colour)

(defun _make_layer (lname ltyp lwt col plot desc)

  (if (not (tblsearch "LAYER" lname))	;layer name
    (progn
      (setq new_layer
	     (vla-add
	       (vla-get-layers
		 (vla-get-activedocument
		   (vlax-get-acad-object)
		 )
	       )
	       lname
	     )
      )
    )
  )
  (vla-put-description new_layer desc)	;description

  (vla-put-linetype
    new_layer
    (if	(tblsearch "LTYPE" ltyp)
      ltyp				;linetype
      "Continuous"
    )
  )
  (vlax-put new_layer 'Lineweight lwt)	;lineweight

  (vla-put-plottable
    new_layer
    (if	plot
      :vlax-true
      :vlax-false
    )
  )					;plottable
  (setq	accol (vla-getinterfaceobject
		(vlax-get-acad-object)
		(strcat	"AutoCAD.AcCmColor."
			(itoa (atoi (getvar "acadver")))
		)
	      )
  )
  (vla-put-colorindex accol col)
  (vla-put-truecolor new_layer accol)	;color
  (vlax-release-object accol)
)
;;Usage:

(vl-load-com)
(defun C:MakeLayers ()
  (_make_layer
    "NEW_LAYER"				;layer name
    "CENTER"				;linetype
    50					;lineweight
    162					;color
    T					;plottable
    "The layer description is goes here" ;description
   )
  (princ)
)