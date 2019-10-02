;;; From: "GaryDF" <arch_prog...@hotmail.com>
;;; Newsgroups: autodesk.autocad.customization
;;; References: <3788944.1106770311821.JavaMail.jive@jiveforum1.autodesk.com>
;;; Subject: Re: need lisp to CREATE layer filters
;;; Date: Thu, 27 Jan 2005 08:28:08 -0600
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