;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;BEGIN CREATING LAYERS WITH ALL PROPERTY;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;Creating Layers with all property (plot style, lw, discription, colour)

(defun _make_layer (lname ltyp lwt col plot desc)
  (vl-load-com)
  
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

(defun C:mla1 ()
;;;;;;;;;;;;;;;;;;;;;;;;;;; TEXT LAYER ;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (_make_layer
    "TEXT_18"				;layer name
    "Continuous"			;linetype
    18					;lineweight -1 ~ BY LAYER; -2 ~ BY BLOCK -3 ~ DEFAULT
    7					;color
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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;END CREATING LAYERS WITH ALL PROPERTY;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;BEGIN CREATING LAYER FILTERS;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ON/OFF
;;;  On 1
;;;  Off 3
;;; FREEZE/THAW
;;;  Freeze 4
;;;  Thaw 12
;;; CURRENT VPORT
;;;  Freeze 16
;;;  Thaw 48
;;; NEW VPORT
;;;  Freeze 64
;;;  Thaw 192
;;; LOCK/UNLOCK
;;;  Lock 256
;;;  UnLock 768
;;; PLOT
;;;  Plot 1024
;;;  Don't Plot 3072 

(defun sdg_CreateLayerFilter (filtlst / lfiltobj safecode safefilt)
  (vl-load-com)
  (setq	LFILTOBJ (vla-addobject
		   (vla-GetExtensionDictionary
		     (vla-Get-Layers
		       (vla-Get-ActiveDocument
			 (vlax-Get-Acad-Object)
		       )
		     )
		   )
		   "ACAD_LAYERFILTERS"
		   "AcDbDictionary"
		 )
	SAFECODE (vlax-make-safearray 2 '(0 . 6))
	SAFEFILT (vlax-make-safearray 12 '(0 . 6))
  )
  (vlax-safearray-fill SAFECODE (list 1 1 1 1 70 1 1))
  (foreach
	    SUBLST
		  FILTLST
    (vlax-safearray-fill
      SAFEFILT
      (mapcar '(lambda (X) (vlax-make-variant X 8)) SUBLST)
    )
    (vla-addxrecord LFILTOBJ (nth 0 SUBLST))
    (vla-setxrecorddata
      (vla-item LFILTOBJ (nth 0 SUBLST))
      SAFECODE
      SAFEFILT
    )
  )
  (prin1)
)
(defun c:MakeFilters (/ filtlst)
  (setq	filtlst
	 (list ;;Name	lay	col	lt	flags	lw	plot
	       (list "CIVIL" "CIV*" "*" "*" 0 "*" "*")
	       (list "EXISTING" "EXIS*" "*" "*" 0 "*" "*")
	       (list "PROFILES" "L*" "*" "*" 0 "*" "*")
	       (list "REVISION" "REV*" "*" "*" 0 "*" "*")
	       (list "SERVICES NEW" "S_*" "*" "*" 0 "*" "*")
	       (list "STAMPS AND IMAGES" "_*" "*" "*" 0 "*" "*")
	       (list "STRUCTURES" "STR*" "*" "*" 0 "*" "*")
	       (list "SURVEY" "SVY*" "*" "*" 0 "*" "*")
	       (list "TEXT" "TEX*" "*" "*" 0 "*" "*")
	       ;(list "OnThawed" "*" "*" "*" 13 "*" "*")
	       ;(list "OffThawed" "*" "*" "*" 15 "*" "*")
	       ;(list "OnFrozen" "*" "*" "*" 5 "*" "*")

	 )
  )
  (sdg_CreateLayerFilter filtlst)
)
(c:MakeFilters)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;END CREATING LAYER FILTERS;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;===============================================================================================================================================