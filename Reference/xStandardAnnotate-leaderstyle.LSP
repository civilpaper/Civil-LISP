;;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;;
;|
    <function>cadcoder:createmultileaderstyle</function>
    <summary>Creates a MultiLeader style in accordance with GT standards</summary>
    <param name="$stylename">Multileader Style name</param>
    <param name="$fontname">Textstyle name to use</param>
     
    <returns>Nothing</returns>
    |;
;;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;;

;;;(createmultileaderstyle "xStandard" "xiso")
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
	   (CONS 45 1.8)			;Text Height
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
  (PRINC)
)

;;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;;
;|
    <function>cadcoder:updatemultileaderstyle</function>
    <summary>Updates an existing multileader style.</summary>
    <param name="$stylename">Multileader Style name</param>
    <returns>Nothing</returns>
    |;
;;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;;
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

;;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;;
;|
    <function>color->mleaderstylecolor</function>
    <summary>Converts an ACI color to an mleader color.</sumary>
    <param name="c">ACI color</param>
     
    <returns>Mleader color expressed as a 24bit value.</returns>
    |;
					;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;;
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

