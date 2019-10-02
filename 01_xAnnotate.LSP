(defun c:xAnnotate ()
  (DIMSTYLE_CREATE "xStandard" "civilpaper")
  (createmultileaderstyle "xStandard" "civilpaper")
)

;;;-------------------------------------------------------------------------------------;;;
;;;					CREATE_TEXTSTYLE				;;;
;;;-------------------------------------------------------------------------------------;;;
;;;					VERSION HISTORY					;;;
;;; - Chinh sua ngay 21/06/2014 dua tren ma DXF cua phien ban CAD 2015			;;;
;;; ------------------------------------------------------------------------------------;;;

(defun TEXTSTYLE_CREATE	(/)
  (entmake
    (list
      (cons 0 "STYLE")
      (cons 100 "AcDbSymbolTableRecord")
      (cons 100 "AcDbTextStyleTableRecord")
      (cons 2 "civilpaper")			; Style Name
      (cons 70 0)
      (cons 40 0.0)			; Fixed text height
      (cons 41 1.0)			; Width Factor
      (cons 50 0.0)			; Oblique angle
      (cons 71 0)
      (cons 42 2.0)			; Last height used
      (cons 3 "iso3098b.shx")		; Primary font name 
      (cons 4 "as_sym.shx")		;  Big font name
    )
  )
  (princ)
)

;;;-------------------------------------------------------------------------------------;;;
;;;					CREATE_DIMSTYLE					;;;
;;;-------------------------------------------------------------------------------------;;;
;;;Description:										;;;
;;;	Called command									;;;
;;;				(DIMSTYLE_CREATE "xStandard" "civilpaper") 		;;;
;;;											;;;
;;; ------------------------------------------------------------------------------------;;;


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
      (cons 141 0.25)			; DIMCEN - Controls drawing of circle or arc center marks and centerlines by the DIMCENTER, DIMDIAMETER, and DIMRADIUS commands.
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
      (cons 173 0)			; DIMSAH - Controls the display of dimension line arrowhead blocks.
      (cons 174 0)			; DIMTIX - Draws text between extension lines.
      (cons 175 0)			; DIMSOXD - Suppresses arrowheads if not enough space is available inside the extension lines.
      (cons 176 1)			; DIMCLRD - Assigns colors to dimension lines, arrowheads, and dimension leader lines.
      (cons 177 1)			; DIMCLRE - Assigns colors to extension lines, center marks, and centerlines.
      (cons 178 7)			; DIMCLRT - Assigns colors to dimension text.
;;;      (cons 270 4)			; DIMUNIT
      (cons 271 0)			; DIMDEC - Sets the number of decimal places displayed for the primary units of a dimension.
      (cons 272 2)			; DIMTDEC
      (cons 273 2)			; DIMALTU - Sets the units format for alternate units of all dimension substyles except Angular.
      (cons 274 3)			; DIMALTTD - Sets the number of decimal places for the tolerance values in the alternate units of a dimension.
      (cons 275 0)			; DIMAUNIT - Sets the units format for angular dimensions.
;;;      (cons 276 2)			; DIMFRAC - Sets the fraction format when DIMLUNIT is set to 4 (Architectural) or 5 (Fractional).
      (cons 277 2)			; DIMLUNIT - Sets units for all dimension types except Angular.
      (cons 279 2)			; DIMTMOVE
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



;;;-------------------------------------------------------------------------------------;;;
;;;					CREATE_MULTILEADERSTYLE				;;;
;;;-------------------------------------------------------------------------------------;;;
;|
    <function>cadcoder:createmultileaderstyle</function>
    <summary>Creates a MultiLeader style in accordance with GT standards</summary>
    <param name="$stylename">Multileader Style name</param>
    <param name="$fontname">Textstyle name to use</param>
         <returns>Nothing</returns>
    |;
;;;-------------------------------------------------------------------------------------;;;

;;;(createmultileaderstyle "xStandard" "civilpaper")
;;;(updatemultileaderstyle "xStandard")
(DEFUN createmultileaderstyle
       ($stylename $fontstyle / $styleentity lst)
  (DEFUN createmultileader (data / dic obj)
    ;;If we can reference the Mleaderstyle dictionary object
    ;;and the proposed style name doesn't exist
    ;;and the entmake was successful
    (IF	(AND (SETQ dic (DICTSEARCH (NAMEDOBJDICT) "ACAD_MLEADERSTYLE"))
	     (NOT (DICTSEARCH (SETQ dic (CDR (ASSOC -1 dic))) "GTSTD"))
	     (SETQ obj (ENTMAKEX data))
	)
      ;;Add the style to the dictionary
      (DICTADD dic (CDR (ASSOC 3 data)) obj)
    )
  )


  ;;If the text style doesn't exist, exit
  (IF (= nil (TBLSEARCH "STYLE" $fontstyle))
    (EXIT)
  )
  ;;Create the Mleader DXF style list
  (SETQ	lst
	 (LIST
	   (CONS 0 "MLEADERSTYLE")
	   (CONS 100 "AcDbMLeaderStyle")
	   (CONS 179 2)			;Text Attachment Point
	   (CONS 170 2)			;Content Type
	   (CONS 171 1)			;Draw MLeaderOrder Type
	   (CONS 172 0)			;DrawLeaderOrderType
	   (CONS 90 0)			;MaxLeader Segments
	   (CONS 40 0.0)		;First Segment Angle Constraint
	   (CONS 41 0.0)		;Second Segment Angle Constraint
	   (CONS 173 1)			;Leader Line Type
	   (CONS 91 (colour->mleaderstylecolour 1))
					;Leader Line Color (Red)
	   (CONS 340 (TBLOBJNAME "LTYPE" "ByLayer")) ;Leader Line Type
	   (CONS 92 -1)			;Leader Line weight
	   (CONS 290 1)			;Enable Landing
	   (CONS 42 1.5)		;Landing Gap
	   (CONS 291 1)			;Enable Dog Leg
	   (CONS 43 0)			;Dogleg Length
	   (CONS 3 $stylename)		;MLeaderDescription
;;;                    (CONS 341 (CDR (ASSOC 330 (ENTGET (TBLOBJNAME "BLOCK" "GT-ARR7"))))) ;Leader ArrowID
	   (CONS 44 1.8)		;Arrow Head Size
	   (CONS 300 "")		;Default Text contents
	   (CONS 342 (TBLOBJNAME "STYLE" $fontstyle)) ;MTextStyleID
	   (CONS 174 5)			;Text Left Attachment Type
	   (CONS 178 5)			;Text Right Attachment Type
	   (CONS 175 1)			;Text Angle Type
	   (CONS 176 0)			;Text Alignment Type
	   (CONS 93 (colour->mleaderstylecolour 7)) ;Text Color
	   (CONS 45 1.8)		;Text Height
	   (CONS 292 0)			;Enable Frame Text
	   (CONS 297 1)			;Text Always Left Justify
	   (CONS 46 0.18)		;Align Space
	   (CONS 142 1.0)		;Scale
	   (CONS 295 1)			;Overright Property Value
	   (CONS 296 0)			;Is Annotative
	   (CONS 143 0.0)		;Break Gap Size
	   (CONS 271 0)			;Text Attachment Direction (0 = Horizontal, 1 = Vertical)
	   (CONS 272 9)			;Bottom Text Attachment Direction (9 = Center, 10 = Underline & Center)
	   (CONS 273 9)			;Top Text Attachment Direction (9 = Center, 10 = Underline & Center)
	 )
  )
  ;;Create the MLeader Style dictionary
  (createmultileader lst)
  ;;Set the new style as current
  (SETVAR "CMLEADERSTYLE" $stylename)
  (princ (strcat "\n\n--\""
		 $stylename
		 "\" is now the current MLeader style--"
	 )
  )
  (PRINC)
)

;;;-------------------------------------------------------------------------------------
;|
    <function>cadcoder:updatemultileaderstyle</function>
    <summary>Updates an existing multileader style.</summary>
    <param name="$stylename">Multileader Style name</param>
    <returns>Nothing</returns>
    |;
;;;-------------------------------------------------------------------------------------
(DEFUN updatemultileaderstyle ($stylename / lst dic data)
  (IF
    ;;Ensure a dictionary reference
    (AND (SETQ dic (DICTSEARCH (NAMEDOBJDICT) "ACAD_MLEADERSTYLE"))
	 ;;And ensure the multileader style exists
	 (SETQ
	   data	(DICTSEARCH (SETQ dic (CDR (ASSOC -1 dic))) $stylename)
	 )
    )
     (PROGN
       ;;Adjust the DXF Codes as required
       (SETQ data (SUBST (CONS 44 1.8) (ASSOC 44 data) data))
       ;;Update the Multi-leader style entry
       (ENTMOD data)
       ;;Update the dictionary
       (ENTUPD dic)
     )
  )
  (PRINC)
)

;;;-----------------------------------------------------------------------------;;;
;|
    <function>color->mleaderstylecolor</function>
    <summary>Converts an ACI color to an mleader color.</sumary>
    <param name="c">ACI color</param>
     
    <returns>Mleader color expressed as a 24bit value.</returns>
    |;
;;;-----------------------------------------------------------------------------;;;
(DEFUN colour->mleaderstylecolour (c)
  ;;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;;
  ;|
        <function>color:rgb->true</function>
        <summary>Converts an RGB color to a true color</sumary>
        <param name="r">Red color value</param>
        <param name="g">Green color value</param>
        <param name="b">Blue color value</param>
        <returns>Color value</returns>
        |;
  ;;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;;

  (DEFUN color:rgb->true (r g b)
    (+
      (LSH (FIX r) 16)
      (LSH (FIX g) 8)
      (FIX b)
    )
  )
  (COND
    ((LISTP c)
     (+ -1040187392 (APPLY 'color:rgb->true c))
    )
    ((= 0 c)
     -1056964608
    )
    ((= 256 c)
     -1073741824
    )
    ((< 0 c 256)
     (+ -1023410176 (color:rgb->true 0 0 c))
    )
  )
)


;;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;;
;|
    <function>mleaderstylecolor->color</function>
    <summary>Converts an MLeader color to the True or ACI color.</sumary>
    <param name="c">Mleader color</param>
     
    <returns>True or ACI color.</returns>
    |;
					;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;;
(DEFUN mleaderstylecolour->colour (c)

  ;;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;;
  ;|
        <function>color:true->rgb</function>
        <summary>Converts an True color to a RGB color</sumary>
        <param name="c">True color to convert</param>
        <returns>Color value</returns>
        |;
  ;;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;;
  (DEFUN color:true->rgb (c)
    (LIST
      (LSH (LSH (FIX c) 8) -24)
      (LSH (LSH (FIX c) 16) -24)
      (LSH (LSH (FIX c) 24) -24)
    )
  )
  (IF (< 0 (LOGAND 16777216 c))
    (LAST (color:true->rgb c))
    (IF	(EQUAL '(0 0 0) (SETQ c (color:true->rgb c)))
      256
      c
    )
  )
)
(princ)