;;; ------------------------------------------------------------------------
;;;    CREATE_DIMSTYLE.LSP
;;;
;;;    Copyright © October, 2007
;;;    Timothy G. Spangler
;;;
;;;    Permission to use, copy, modify, and distribute this software
;;;    for any purpose and without fee is hereby granted, provided
;;;    that the above copyright notice appears in all copies and
;;;    that both that copyright notice and the limited warranty and
;;;    restricted rights notice below appear in all supporting
;;;    documentation.
;;;
;;;    CREATE_DIMSTYLE
;;;
;;;		 Description:
;;;			Called from a menu pulldown or rightclick menu
;;;				*(DIMSTYLE_CREATE "test0" 48.0 "Archquik")
;;;
;;; ------------------------------------------------------------------------
;;;		VERSION HISTORY
;;;
;;; ------------------------------------------------------------------------

;;; MAIN FUNCTION ;;;;;;;;;;;;;;;;;;;;;;;;;
(defun DIMSTYLE_CREATE
       (StyleName StyleScale TextStyle / OldDimBlk OldCmdEcho)

  ;; Set echo
  (setq OldCmdEcho (getvar "CMDECHO"))
  (setvar "CMDECHO" 0)

  ;; Create textstyle
  (if (not (tblsearch "STYLE" TextStyle))
    (TEXTSTYLE_CREATE)
  )
  ;; Create dimblk
  (if (not (tblobjname "block" "_Oblique"))
    (progn
      (setq OldDimBlk (getvar "dimblk"))
      (if (= OldDimBlk "")
	(setq OldDimBlk ".")
      )
      (setvar "dimblk" "_Oblique")
      (setvar "dimblk" OldDimBlk)
    )
  )
  ;; Create dimstyle
  (entmake
    (list
      (cons 0 "DIMSTYLE")		; Table
      (cons 100 "AcDbSymbolTableRecord") ; Subclass marker
      (cons 100 "AcDbDimStyleTableRecord") ; Subclass marker
      (cons 2 StyleName)		; Dimstyle name
      (cons 70 0)			; Standard flag
      (cons 3 "")			; DIMPOST
      (cons 4 "")			; DIMAPOST
      (cons 5 "_Oblique")		; DIMBLK
      (cons 6 "_Oblique")		; DIMBLK1
      (cons 7 "")			; DIMBLK2
      (cons 40 StyleScale)		; DIMSCALE
      (cons 41 0.0937)			; DIMASZ
      (cons 42 0.0937)			; DIMEXO
      (cons 43 0.38)			; DIMDLI
      (cons 44 0.0625)			; DIMEXE
      (cons 45 0.0)			; DIMRND
      (cons 46 0.0625)			; DIMDLE
      (cons 47 0.0)			; DIMTP
      (cons 48 0.0)			; DIMTM
      (cons 140 0.0937)			; DIMTXT
      (cons 141 0.09)			; DIMCEN
      (cons 142 0.0)			; DIMTSZ
      (cons 143 25.4)			; DIMALTF
      (cons 144 1.0)			; DIMLFAC
      (cons 145 0.0)			; DIMTVP
      (cons 146 1.0)			; DIMTFAC
      (cons 147 0.0625)			; DIMGAP
      (cons 71 0)			; DIMTOL
      (cons 72 0)			; DIMLIM
      (cons 73 0)			; DIMTIH
      (cons 74 0)			; DIMTOH
      (cons 75 0)			; DIMSE1
      (cons 76 0)			; DIMSE2
      (cons 77 1)			; DIMTAD
      (cons 78 3)			; DIMZIM
      (cons 170 0)			; DIMALT
      (cons 171 2)			; DIMALTD
      (cons 172 0)			; DIMTOFL
      (cons 173 0)			; DIMSAH
      (cons 174 0)			; DIMTIX
      (cons 175 0)			; DIMSOXD
      (cons 176 0)			; DIMCLRD
      (cons 177 0)			; DIMCLRE
      (cons 178 2)			; DIMCRRT
      (cons 270 4)			; DIMUNIT
      (cons 271 4)			; DIMDEC
      (cons 272 4)			; DIMTDEC
      (cons 273 2)			; DIMALTU
      (cons 274 2)			; DIMALTTD
      (cons 275 0)			; DIMAUNIT
      (cons 276 2)			; DIM?????
      (cons 277 4)			; DIMLUNIT
      (cons 279 2)			; DIM????
      (cons 280 0)			; DIMJUST
      (cons 281 0)			; DIMSD1
      (cons 282 0)			; DIMSD2
      (cons 283 1)			; DIMTOLJ
      (cons 284 0)			; DIMTZIN
      (cons 285 0)			; DIMALTZ
      (cons 286 0)			; DIMALTTZ
      (cons 287 5)			; DIMFIT
      (cons 288 0)			; DIMUPT
					; DIMTXSTY
      (cons 340 (tblobjname "style" TextStyle))
					; DIMLDRBLK
      (cons 342
	    (cdr (assoc 330 (entget (tblobjname "block" "_Oblique"))))
      )
					; DIMLDRBLK1
      (cons 343
	    (cdr (assoc 330 (entget (tblobjname "block" "_Oblique"))))
      )
					; DIMLDRBLK2 Block for the leader
					;(cons 341 (cdr	(assoc 330 (entget(tblobjname "block" "")))))
    )
  )
  ;; Set dimstyle current
  (command "-dimstyle" "restore" StyleName)
  ;; Set echo
  (setvar "CMDECHO" OldCmdEcho)
  (princ (strcat "\n\n--\""
		 StyleName
		 "\" is now the current Dimstyle--"
	 )
  )
  (princ)
)
;; ENTMAKE TEXT STYLE
(defun TEXTSTYLE_CREATE	(/)

  (entmake
    (list
      (cons 0 "STYLE")
      (cons 100 "AcDbSymbolTableRecord")
      (cons 100 "AcDbTextStyleTableRecord")
      (cons 2 "Archquik")
      ;; Style Name
      (cons 70 0)
      (cons 40 0.0)
      ;; Fixed text height
      (cons 41 1.0)
      ;; Width Factor
      (cons 50 0.0)
      ;; Oblique angle
      (cons 71 0)
      (cons 42 2.0)
      ;; Last height used
      (cons 3 "archquik.shx")
      ;; Primary font name 
      (cons 4 "")
      ;;  Big font name
    )
  )
  (princ)
)
(princ)