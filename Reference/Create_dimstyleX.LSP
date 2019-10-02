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
;;;				*(DIMSTYLE_CREATE "xStandardx" "xisox")
;;;
;;; ------------------------------------------------------------------------
;;;		VERSION HISTORY
;;;
;;; ------------------------------------------------------------------------

;;; MAIN FUNCTION ;;;;;;;;;;;;;;;;;;;;;;;;;
(defun DIMSTYLE_CREATE
		       (StyleName TextStyle / OldDimBlk OldCmdEcho)

  ;; Set echo
  (setq OldCmdEcho (getvar "CMDECHO"))
  (setvar "CMDECHO" 0)

  ;; Create textstyle
  (if (not (tblsearch "STYLE" TextStyle))
    (TEXTSTYLE_CREATE)
  )
  ;; Create dimblk
;;;  (if (not (tblobjname "block" ""))
;;;    (progn
;;;      (setq OldDimBlk (getvar "dimblk"))
;;;      (if (= OldDimBlk "")
;;;	(setq OldDimBlk ".")
;;;      )
;;;      (setvar "dimblk" "")
;;;      (setvar "dimblk" OldDimBlk)
;;;    )
;;;  )
  ;; Create dimstyle
  (entmake
    (list
      (cons 0 "DIMSTYLE")		; Table
      (cons 100 "AcDbSymbolTableRecord") ; Subclass marker
      (cons 100 "AcDbDimStyleTableRecord") ; Subclass marker
      (cons 2 StyleName)		; Dimstyle name
      (cons 70 0)			; Standard flag
      (cons 3 "")			; DIMPOST
      (cons 4 "")			; DIMAPOST - Specifies a text prefix or suffix (or both) to the alternate dimension measurement for all types of dimensions except angular.
;;;      (cons 5 "")		; DIMBLK
;;;      (cons 6 "")		; DIMBLK1
;;;      (cons 7 "")		; DIMBLK2
      (cons 40 1.0)			; DIMSCALE
      (cons 41 1.8)			; DIMASZ - Controls the size of dimension line and leader line arrowheads. Also controls the size of hook lines.
      (cons 42 0.55)			; DIMEXO - Specifies how far extension lines are offset from origin points.
      (cons 43 0.38)			; DIMDLI
      (cons 44 0.5)			; DIMEXE - Specifies how far to extend the extension line beyond the dimension line.
      (cons 45 0.0)			; DIMRND - Rounds all dimensioning distances to the specified value.
      (cons 46 0.0625)			; DIMDLE - Sets the distance the dimension line extends beyond the extension line when oblique strokes are drawn instead of arrowheads.
      (cons 47 0.0)			; DIMTP
      (cons 48 0.0)			; DIMTM
      (cons 140 1.8)			; DIMTXT - Specifies the height of dimension text, unless the current text style has a fixed height
      (cons 141 0.09)			; DIMCEN - Controls drawing of circle or arc center marks and centerlines by the DIMCENTER, DIMDIAMETER, and DIMRADIUS commands.
      (cons 142 0.0)			; DIMTSZ
      (cons 143 25.4)			; DIMALTF
      (cons 144 1000.0)			; DIMLFAC - Sets a scale factor for linear dimension measurements.
      (cons 145 0.0)			; DIMTVP
      (cons 146 1.0)			; DIMTFAC
      (cons 147 0.75)			; DIMGAP - Sets the distance around the dimension text when the dimension line breaks to accommodate dimension text.
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
      (cons 176 1)			; DIMCLRD - Assigns colors to dimension lines, arrowheads, and dimension leader lines.
      (cons 177 0)			; DIMCLRE - Assigns colors to extension lines, center marks, and centerlines.
      (cons 178 7)			; DIMCLRT - Assigns colors to dimension text.
;;;      (cons 270 4)			; DIMUNIT
      (cons 271 2)			; DIMDEC - Sets the number of decimal places displayed for the primary units of a dimension.
      (cons 272 2)			; DIMTDEC
      (cons 273 2)			; DIMALTU - Sets the units format for alternate units of all dimension substyles except Angular.
      (cons 274 3)			; DIMALTTD - Sets the number of decimal places for the tolerance values in the alternate units of a dimension.
      (cons 275 0)			; DIMAUNIT - Sets the units format for angular dimensions.
;;;      (cons 276 2)			; DIMFRAC - Sets the fraction format when DIMLUNIT is set to 4 (Architectural) or 5 (Fractional).
      (cons 277 2)			; DIMLUNIT - Sets units for all dimension types except Angular.
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
;;;					; DIMLDRBLK
;;;      (cons 342
;;;	    (cdr (assoc 330 (entget (tblobjname "block" ""))))
;;;      )
;;;					; DIMLDRBLK1
;;;      (cons 343
;;;	    (cdr (assoc 330 (entget (tblobjname "block" ""))))
;;;      )
;;;					; DIMLDRBLK2 Block for the leader
;;;					;(cons 341 (cdr	(assoc 330 (entget(tblobjname "block" "")))))
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
      (cons 2 "xisox")
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
      (cons 3 "iso3098b.shx")
      ;; Primary font name 
      (cons 4 "")
      ;;  Big font name
    )
  )
  (princ)
)
(princ)