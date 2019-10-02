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
;;Usage

(vl-load-com)
(defun C:mla1 ()
;;;;;;;;;;;;;;;;;;;;;;;;;;; TEXT LAYER ;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (_make_layer
    "TEXT_18"				;layer name
    "Continuous"			;linetype
    18					;lineweight -1 ~ BY LAYER; -2 ~ BY BLOCK -3 ~ DEFAULT
    0					;color
    T					;plottable T-true; F-false
    "All text, attributed tags and dimension for A3" ;description
   )
  (_make_layer
    "TEXT_25"				;layer name
    "Continuous"			;linetype
    25					;lineweight -1 ~ BY LAYER; -2 ~ BY BLOCK -3 ~ DEFAULT
    3					;color
    T					;plottable T-true; F-false
    "All text, attributed tags and dimension for A3" ;description
   )
;;;;;;;;;;;;;;;;;;;;;;;;;;; STRUCTURES ;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (_make_layer
    "STR_CONCRETE_HATCH"		;layer name
    "Continuous"			;linetype
    18					;lineweight -1 ~ BY LAYER; -2 ~ BY BLOCK -3 ~ DEFAULT
    0					;color - 0 ~ white
    T					;plottable T-true; F-false
    "ALL CONCRETE HATCHES"		;description
   )
  (_make_layer
    "STR_CONCRETE_MISC"			;layer name
    "Continuous"			;linetype
    18					;lineweight -1 ~ BY LAYER; -2 ~ BY BLOCK -3 ~ DEFAULT
    0					;color - 0 ~ white
    T					;plottable T-true; F-false
    "CONCRETE BACKGROUND INFO AND MISCELLANEOUS DETAILS TO BE SHOWN LIGHTER"
					;description
   )
  (_make_layer
    "STR_REINFORCEMENT_vBARS"		;layer name
    "Continuous"			;linetype
    25					;lineweight -1 ~ BY LAYER; -2 ~ BY BLOCK -3 ~ DEFAULT
    6					;color - 6 ~ magenta
    T					;plottable T-true; F-false
    "ALL VERTICAL BAR REINFORCEMENT"	;description
   )
  (_make_layer
    "STR_REINFORCEMENT_hBARS"		;layer name
    "Continuous"			;linetype
    25					;lineweight -1 ~ BY LAYER; -2 ~ BY BLOCK -3 ~ DEFAULT
    6					;color - 6 ~ magenta
    T					;plottable T-true; F-false
    "ALL HORIZONTAL BAR REINFORCEMENT"	;description
   )
;;;;;;;;;;;;;;;;;;;;;;;;;;; EXISTING ;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (_make_layer
    "DEFPOINTS"				;layer name
    "Continuous"			;linetype
    -3					;lineweight -1 ~ BY LAYER; -2 ~ BY BLOCK -3 ~ DEFAULT
    0					;color - 0 ~ white
    F					;plottable T-true; F-false
    "NON-PLOTTING LAYER GENERIC WITH AUTOCAD" ;description
   )
  (princ)
)
(c:mla1)