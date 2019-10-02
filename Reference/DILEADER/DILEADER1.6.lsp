;|										;;
	DIL (Dynamic Intelligent Leader) 					;;
										;;
	By: Andrea Andreetti 2009-06-17						;;
										;;
	Command:DIL								;;
	Allow user to create Dynamic Intelligent Leader				;;
										;;
	Command:DILEDIT								;;
	Allow user to EDIT DIL object.						;;
										;;
	Updates:								;;
	V.1.1 - 22-06-09							;;
	- DL_textsize variable improved						;;
	- vla-intersectwith against vlax-curve-getClosestPointTo		;;
	- Arrow size as per DIMASZ variable					;;
	- Left/Right text update when object was moved				;;
	- "NOTE" text gone when picking the leader location text		;;
										;;
	V.1.2 - 22-06-09							;;
	- Little Bug when selection line and ARC fixed				;;
										;;
	V.1.3 - 17-07-09							;;
	- Work with Block entities.						;;
	- Dimscale & Textsize readjusted					;;
										;;
	V.1.4 - 04-08-09							;;
	- Multiple selection allowed.						;;
	- Text color adjusted with DIMCLRT variable				;;
	- Leader position readjusted when Moved or stretched			;;
	- MOVE,STRETCH,ROTATE,SCALE are now added to reactor			;;
	- Reactor now work for block entities and/with multiple selection	;;
	- DILEDIT remake to allow block recongnition				;;
	- TEXT & MTEXT selection option addded					;;
										;;
	V.1.5 - 06-08-09							;;
	- Block Arrow Switch by pressing the TAB key				;;
										;;
	V.1.6 - 13-08-09							;;
	- Textstyle follow DIMTXSTY variable					;;
	- DILEDIT can now allow to select Leader and Text			;;
	- Icon Added on mouse mouve to see wich leader have been		;;
	  created by DiLEADER ( follow the GRIPSCOLOR variable)			;;
	- Bug Fixed when Text was deleted from DiLeader object allow now to	;;
	  Edit Leader who do not Contain any Text or Mtext			;;
										|;
										;;


(vl-load-com)
(princ "\nDILEADER (DIL) v. 1.6 by Andrea Andreetti        -Loaded-")

(defun c:DILEADER () (C:DIL))
(defun c:DIL (/ llead        ltext        maindlactive dqobject     dragmess     pointmethod  aspoint
                dqlcursorposition         okaccept     ltext        input        dlactive     dl_textsize  vlao
                dxitemhandle dql_llpoint  dql_urpoint  cen          dl_dist      dl_ang       cursp2       cursp
                int DLtextMode1 DIMTXSTY
               )

(defun *error* (msg)
  (redraw)
  (princ (strcat "\n" msg))

  (if ltext
    (progn (command "._erase" ltext "") (setq ltext nil))
  )
  (if llead
             (progn (command "._erase" llead "") (setq llead nil))
  )
(princ)
)

(setq DiLEADER_onEDIT nil)
(getDIMLDRBLKvariable)

(setq DIMTXSTY (getvar "DIMTXSTY"))  
  
(if (not DLtextMode)
  (setq DLtextMode "MTEXT")
)
(initget "Text Mtext")
(setq DLtextMode1 (getkword (strcat "\n(T)ext/(M)text ?<" (substr DLtextMode 1 1) ">: ")))
(if DLtextMode1    
  (setq DLtextMode (strcase DLtextMode1))
)

  
(setq DQobject nil
      iib nil)
  (setq DL_textsize (* (getvar "DIMSCALE") (getvar "DIMTXT")))
  (setvar "CMDECHO" 0)

  (while (not DQobject)
    (setq DQobject (nentsel "\nSelect Object..."))    
  )

(setq TextHandle nil)  
(if (> (length DQobject) 2)
  (progn
  (setq VLAoB (vlax-ename->vla-object (caar (reverse DQobject))))
  (setq ObjectHandle (vla-get-Handle VLAoB))
  (setq InsideBlockHandle (cdr (assoc 5 (entget (car DQobject)))))
  )
  (progn
  (setq VLAoB nil)
  (setq ObjectHandle (vla-get-Handle (vlax-ename->vla-object (car DQobject))))
  (setq InsideBlockHandle "nil")
  )
)

  
  (if DQobject
    (progn
      (princ "\nPick Text location...")     
      
      (DleaderWhile
	nil
	(vlax-ename->vla-object (car DQobject))
	nil
      )

      (while
	(and
	  (/= (car input) 25)		;RIGHT CLICK
	  (/= (car input) 11)
	  (/= (car input) 12)
	  (/= (car input) 3)		;LEFT CLICK
	  (not (and (= (car input) 2) (= (cadr input) 32))) ;ESCAPE
	  (not (and (= (car input) 2) (= (cadr input) 13))) ;ENTER
	)

	 (DleaderWhile
	   nil
	   (vlax-ename->vla-object (car DQobject))
	   nil
	 )
      )
    )
  )

(princ "\nDileader Finish.")
(princ)
  
)



;|								;;
	DLEADER While Loop					;;
								|;
								;;

(defun DleaderWhile
		    (TEXTEename
		     VLAo
		     Lleadprop
		     / NewMod Ltext)
(vla-StartUndoMark (vla-get-ActiveDocument (vlax-get-ACAD-Object)))

  
  (Setq DLactive T)
  (while (and DLactive
	      (setq input (grread t 4 4))
	      (or
					;Cursor
		(= (car input) 5)
					;PickPoint
		(= (car input) 3)

                (and (= (car input) 2) (= (cadr input) 9)) ;TAB
                (and (= (car input) 2) (/= (cadr input) 9));ALL OTHER KEY
	      )
	 )


    (if	(= (car input) 5)
      (setq DQLcursorPosition (cadr input))
    )
    

;;TAB
(if (and (= (car input) 2) (= (cadr input) 9))
  (progn
(if (>=
  (setq ArrowPos (vl-position DIMLDRBLK ArrowBlockList))
  (1- #Arrow))  
  (setq DIMLDRBLK (nth 0 ArrowBlockList))
  (setq DIMLDRBLK (nth (setq ArrowPos (1+ ArrowPos)) ArrowBlockList))
)

(command "._DIMLDRBLK" DIMLDRBLK)
)
)
    
    (if	(and VLAo DQLcursorPosition (/= (car input) 2))
      (DLEADER_C&M_OBJECT DQLcursorPosition
                          TEXTEename
                          VLAo
                          Lleadprop
                          )
    )
    
    (if	(= (car input) 3)
      (progn
	(setq DLactive nil)
	(if (and (not DiLEADER_onEDIT)(eq DLtextMode "MTEXT"))
	  (command "._MTEDIT" (cdar (entmod (subst (cons 1 "") (assoc 1 (entget Ltext)) (entget Ltext)))))
	)
        (if (and (not DiLEADER_onEDIT)(eq DLtextMode "TEXT"))
	  (command "._DDEDIT" (cdar (entmod (subst (cons 1 "") (assoc 1 (entget Ltext)) (entget Ltext)))) "")
	)
      )
    )

  ) ;_while

  
(if Llead      
(DLEADERPUTXDATA
      Llead
      "DLEADER_TEXT"		;;BlockObject	TEXT	;;ItemObject
      (vl-prin1-to-string (list ObjectHandle (vl-princ-to-string TextHandle) InsideBlockHandle))
    )
)

(vla-EndUndoMark (vla-get-ActiveDocument (vlax-get-ACAD-Object)))  
  (setq DLactive nil)
)


								;;
;|								;;
	DLEADER While Loop					;;
								|;








;|								;;
	GET INT POINT FOR BLOCKS				;;
								|;
								;;


(defun getblockint (vlaobjx curspx nentse / dl_dist dl_ang cursp2 binsertion insbase coordx coordy)
  (setq insbase (getvar "insbase"))
  (setq binsertion (vlax-safearray->list
                     (vlax-variant-value (vla-get-insertionpoint vlaobjx))
                   )
  )
  (setq dl_dist (distance insbase binsertion))
  (setq dl_ang (angle insbase binsertion))
  (setq cursp2 (polar curspx (angle binsertion insbase) dl_dist))
  (setq nentse (vlax-ename->vla-object nentse))
  (setq cursp2 (polar insbase
                      (- (angle insbase cursp2) (vla-get-rotation vlaobjx))
                      (distance insbase cursp2)
               )
  )
  (setq coordx (/ (nth 0 cursp2) (vla-get-xscalefactor vlaobjx)))
  (setq coordy (/ (nth 1 cursp2) (vla-get-yscalefactor vlaobjx)))
  (setq cursp2 (list coordx coordy (nth 2 insbase)))
  (setq int (vlax-curve-getclosestpointto nentse cursp2))
  (setq coordx (+ (nth 0 insbase)
                  (* (vla-get-xscalefactor vlaobjx)
                     (- (nth 0 int) (nth 0 insbase))
                  )
               )
  )
  (setq coordy (+ (nth 1 insbase)
                  (* (vla-get-yscalefactor vlaobjx)
                     (- (nth 1 int) (nth 1 insbase))
                  )
               )
  )
  (setq int (list coordx coordy (nth 2 insbase)))
  (setq int (polar insbase
                   (+ (angle insbase int) (vla-get-rotation vlaobjx))
                   (distance insbase int)
            )
  )
  (setq int (polar int dl_ang dl_dist))
)


								;;
;|								;;
	GET INT POINT FOR BLOCKS				;;
								|;






;|								;;
	DLEADER UPDATE for REACTOR				;;
								|;
								;;
  
(defun dleader_updated_by_reactor (cursp cenpoint vlaobj lleadprop txtobj isblock)
  (if isblock
    (getblockint vlaobj cursp isblock)
    (setq int (vlax-curve-getclosestpointto vlaobj cursp))
  )
  (if txtobj
    (if (entget txtobj)
      (if (not (read (cdr (assoc 1 (entget txtobj)))))
        (setq txtobj nil)
      )
      (setq txtobj nil)
    )
  )
  (if (and int lleadprop cursp)
    (progn (setq 10list nil)
           (foreach n (entget lleadprop)
             (if (eq (car n) 10)
               (setq 10list (append 10list (list (cdr n))))
             )
           )
           (if txtobj
             (progn (setq dl_textsize (vla-get-height
                                        (setq vla_txtobj (vlax-ename->vla-object txtobj))
                                      )
                    )
                    (vla-getboundingbox vla_txtobj 'x 'y)
                    (setq dql_llpoint (vlax-safearray->list x))
                    (setq dql_urpoint (vlax-safearray->list y))
                    (setq dql_ulpoint (list (nth 0 dql_llpoint) (nth 1 dql_urpoint) 0.0))
                    (setq dql_umpoint (polar dql_ulpoint
                                             (angle dql_ulpoint dql_urpoint)
                                             (/ (distance dql_ulpoint dql_urpoint) 2)
                                      )
                    )
                    (setq lref (polar dql_umpoint
                                      (angle dql_ulpoint dql_llpoint)
                                      (/ dl_textsize 2)
                               )
                    )
                    (setq ld_2 (distance lref
                                         (polar lref
                                                (angle (nth 2 10list) (nth 1 10list))
                                                (+ (distance dql_umpoint dql_ulpoint) dl_textsize)
                                         )
                               )
                    )
                    (setq ld_3 (distance lref
                                         (polar lref
                                                (angle (nth 2 10list) (nth 1 10list))
                                                (+ (distance dql_umpoint dql_ulpoint) (/ dl_textsize 3.0))
                                         )
                               )
                    )
             )
             (progn (setq dl_textsize (* (getvar "DIMSCALE") (getvar "DIMTXT")))
                    (setq lref (nth 2 10list))
                    (setq ld_2 (/ dl_textsize 1.5))
                    (setq ld_3 0)
             )
           )
           (if (> (distance (nth 1 10list) int) (distance lref int))
             (progn (setq arw_end (polar lref (angle (nth 1 10list) (nth 2 10list)) ld_3))
                    (setq arw_mid (polar lref (angle (nth 1 10list) (nth 2 10list)) ld_2))
                    (if isblock
                      (getblockint vlaobj arw_mid isblock)
                      (setq int (vlax-curve-getclosestpointto vlaobj arw_mid))
                    )
                    (setq new10list nil)
                    (setq new10list (list (cons 10 int) (cons 10 arw_mid) (cons 10 arw_end)))
             )
             (progn (setq arw_end (nth 2 10list))
                    (setq arw_mid (nth 1 10list))
                    (if isblock
                      (getblockint vlaobj arw_mid isblock)
                      (setq int (vlax-curve-getclosestpointto vlaobj arw_mid))
                    )
                    (setq new10list nil)
                    (setq new10list (list (cons 10 int)
                                          (cons 10 (nth 1 10list))
                                          (cons 10 (nth 2 10list))
                                    )
                    )
             )
           )
           (setq lleadpropdata (entget lleadprop))
           (repeat 3
             (setq lleadpropdata (vl-remove (assoc 10 lleadpropdata) lleadpropdata))
           )
           (setq lleadpropdata (entmod (append lleadpropdata new10list)))
           (setq int nil
                 lleadprop nil
                 lleadpropdata nil
           )
    ) ;_if
  ) ;_if  
) ;_defun

								;;
;|								;;
	DLEADER UPDATE for REACTOR				;;
								|;




;|								;;
	DLEADER CREATE & MODIFY OBJECT				;;
								|;
								;;

(defun dleader_c&m_object (cursp dltext vlao lleadprop / dl_dist dl_ang binsertion insbase cursp2 coordx coordy)
  (setq insbase (getvar "insbase"))
  (if iib
    (setq vlao iib)
  )
  ;;IF BLOCK
  (if vlaob
    (progn (setq binsertion (vlax-safearray->list
                              (vlax-variant-value (vla-get-insertionpoint vlaob))
                            )
           )
           (setq dl_dist (distance insbase binsertion))
           (setq dl_ang (angle insbase binsertion))
           (setq cursp2 (polar cursp (angle binsertion insbase) dl_dist))
           (setq cursp2 (polar insbase
                               (- (angle insbase cursp2) (vla-get-rotation vlaob))
                               (distance insbase cursp2)
                        )
           )
           (setq coordx (/ (nth 0 cursp2) (vla-get-xscalefactor vlaob)))
           (setq coordy (/ (nth 1 cursp2) (vla-get-yscalefactor vlaob)))
           (setq cursp2 (list coordx coordy (nth 2 insbase)))
           (setq int (vlax-curve-getclosestpointto vlao cursp2))
           (setq coordx (+ (nth 0 insbase)
                           (* (vla-get-xscalefactor vlaob)
                              (- (nth 0 int) (nth 0 insbase))
                           )
                        )
           )
           (setq coordy (+ (nth 1 insbase)
                           (* (vla-get-yscalefactor vlaob)
                              (- (nth 1 int) (nth 1 insbase))
                           )
                        )
           )
           (setq int (list coordx coordy (nth 2 insbase)))
           (setq int (polar insbase
                            (+ (angle insbase int) (vla-get-rotation vlaob))
                            (distance insbase int)
                     )
           )
           (setq int (polar int dl_ang dl_dist))
    )
    (setq int (vlax-curve-getclosestpointto vlao cursp))
  )
  (if dileader_onedit
    (dleader_m_object)
    (dleader_c_object)
  )
)
								;;
;|								;;
	DLEADER CREATE & MODIFY OBJECT				;;
								|;








;|								;;
	DLEADER MODIFY TEXT AND LEADER OBJECT			;;
								|;
								;;
(defun dleader_m_object ()
  
;;MODIFICATION MTEXT
      (if (eq DLtextMode "MTEXT")
        (progn          
           (setq newmod (entmod (setq Tmod (subst (cons 10 cursp) (assoc 10 (entget dltext)) (entget dltext)))))           
           (setq newmod (cdar
                          (entmod (subst (cons 71 v71) (assoc 71 newmod) newmod))
                        )
           )
          )
      )

      (if (eq DLtextMode "TEXT")  
        (progn           
           (setq newmod (entmod (setq Tmod (subst (cons 10 cursp) (assoc 10 (entget dltext)) (entget dltext)))))
           (setq newmod (entmod (subst (cons 11 cursp) (assoc 11 Tmod) Tmod)))
          (setq newmod (cdar
                          (entmod (subst (cons 72 v72) (assoc 72 newmod) newmod))
                        )
           )
          )
        )
      
;;MODIFICATION MTEXT




;;Create leader point
  (if (> (car cursp) (car int))
    (setq v71 1
          v72 0          
          co1 (list (- (car cursp) dl_textsize)
                    (- (nth 1 cursp) (/ dl_textsize 2))
                    (nth 2 cursp)
              )
          co2 (list (- (car cursp) (/ dl_textsize 3))
                    (- (nth 1 cursp) (/ dl_textsize 2))
                    (nth 2 cursp)
              )
    )
    (setq v71 3
          v72 2
          co1 (list (+ (car cursp) dl_textsize)
                    (- (nth 1 cursp) (/ dl_textsize 2))
                    (nth 2 cursp)
              )
          co2 (list (+ (car cursp) (/ dl_textsize 3))
                    (- (nth 1 cursp) (/ dl_textsize 2))
                    (nth 2 cursp)
              )
    )
  )
;;Create leader point

  
;;MODIFICATION WITH DLEDIT  
  (if (and int lleadprop)
    (progn      
           (repeat 3
             (setq lleadprop (vl-remove (assoc 10 lleadprop) lleadprop))
           )
           (setq lleadprop (entmod (append lleadprop
                                           (list (cons 10 int))
                                           (list (cons 10 co1))
                                           (list (cons 10 co2))
                                   )
                           )
           )
           (setq llead (cdar
                         (entmod (subst (cons 71 v71) (assoc 71 lleadprop) lleadprop)
                         )
                       )
           )
      (vla-put-ArrowheadBlock
        (vlax-ename->vla-object llead)
        DIMLDRBLK
        )
    )
  )
;;MODIFICATION WITH DLEDIT
)
								;;
;|								;;
	DLEADER MODIFY TEXT AND LEADER OBJECT			;;
								|;






;|								;;
	DLEADER CREATE TEXT AND LEADER OBJECT			;;
								|;
								;;

(defun  dleader_c_object ()

;;Create Mtext
  (if ltext
    (progn (command "._erase" ltext "") (setq ltext nil))
  )

;;Create leader point
  (if (> (car cursp) (car int))
    (setq v71 1
          v72 0          
          co1 (list (- (car cursp) dl_textsize)
                    (- (nth 1 cursp) (/ dl_textsize 2))
                    (nth 2 cursp)
              )
          co2 (list (- (car cursp) (/ dl_textsize 3))
                    (- (nth 1 cursp) (/ dl_textsize 2))
                    (nth 2 cursp)
              )
    )
    (setq v71 3
          v72 2
          co1 (list (+ (car cursp) dl_textsize)
                    (- (nth 1 cursp) (/ dl_textsize 2))
                    (nth 2 cursp)
              )
          co2 (list (+ (car cursp) (/ dl_textsize 3))
                    (- (nth 1 cursp) (/ dl_textsize 2))
                    (nth 2 cursp)
              )
    )
  )

;;CREATE MTEXT
  (if (and int (not dltext))
    (if (eq dltextmode "MTEXT")
      (progn (setq ltext (entmakex (list (cons 0 "MTEXT")
                                         (cons 100 "AcDbEntity")
                                         (cons 100 "AcDbMText")
                                         (cons 1 "NOTE")
                                         (cons 7 dimtxsty)
                                         (cons 62 (getvar "DIMCLRT"))
                                         (cons 10 cursp)
                                         (cons 40 dl_textsize)
                                         (cons 50 0.0)
                                         (cons 71 v71)
                                         (cons 72 5)
                                   )
                         )
             )
             (setq texthandle (cdr (assoc 5 (entget ltext))))
      )
      (progn (setq ltext (entmakex (list (cons 0 "TEXT")
                                         (cons 100 "AcDbEntity")
                                         (cons 100 "AcDbText")
                                         (cons 1 "NOTE")
                                         (cons 62 (getvar "DIMCLRT"))
                                         (cons 10 cursp)
                                         (cons 11 cursp)
                                         (cons 40 dl_textsize)
                                         (cons 73 3)
                                         (cons 72 v72)
                                   )
                         )
             )
             (setq texthandle (cdr (assoc 5 (entget ltext))))
      )
    )
  )
;;CREATE MTEXT
  
  ;;Create Leader
  (if (and int (not lleadprop))
    (progn (if llead
             (progn (command "._erase" llead "") (setq llead nil))
           )
           (setq llead (entmakex (list (cons 0 "LEADER")
                                       (cons 100 "AcDbEntity")
                                       (cons 100 "AcDbLeader")
                                       (cons 10 int)
                                       (cons 10 co1)
                                       (cons 10 co2)
                                 )
                       )
           )
           (vla-put-arrowheadsize
             (vlax-ename->vla-object llead)
             (* (getvar "DIMSCALE") (getvar "DIMASZ"))
           )

      (vla-put-ArrowheadBlock
        (vlax-ename->vla-object llead)
        DIMLDRBLK
        )
    )
  )
)



								;;
;|								;;
	DLEADER CREATE & MODIFY OBJECT				;;
								|;





;|						;;
	X D A T A				;;
						|;
						;;

(defun dleaderputxdata (item xdataname tag / ent type1 valeur)
  (setq ent (vlax-ename->vla-object item))
  (setq type1  (vlax-make-safearray vlax-vbinteger (cons 0 1))
        valeur (vlax-make-safearray vlax-vbvariant (cons 0 1))
  )
  (vlax-safearray-put-element type1 0 1001)
  (vlax-safearray-put-element valeur 0 xdataname)
  (vlax-safearray-put-element type1 1 1000)
  (vlax-safearray-put-element valeur 1 tag)
  (setq type1  (vlax-make-variant type1)
        valeur (vlax-make-variant valeur)
  )
  (vla-setxdata ent type1 valeur)
)
						;;
;|						;;
	X D A T A				;;
						|;




;|						;;
	ARROW BLOCKS				;;
						|;
						;;
(defun getDIMLDRBLKvariable ()
(setq ArrowBlockList
       (list
	 "."
	 "_DOT"
	 "_DOTSMALL"
	 "_DOTBLANK"
	 "_ORIGIN"
	 "_ORIGIN2"
	 "_OPEN"
	 "_OPEN90"
	 "_OPEN30"
	 "_CLOSED"
	 "_SMALL"
	 "_NONE"
	 "_OBLIQUE"
	 "_BOXFILLED"
	 "_BOXBLANK"
	 "_CLOSEDBLANK"
	 "_DATUMFILLED"
	 "_DATUMBLANK"
	 "_INTEGRAL"
	 "_ARCHTICK"
	 )
 )

(if (eq
      (setq DIMLDRBLK (strcat "_" (strcase (getvar "DIMLDRBLK"))))
      "_"
      )
  (setq DIMLDRBLK ".")
)
  
(if (not (member DIMLDRBLK ArrowBlockList))  
    (setq ArrowBlockList (append ArrowBlockList (list (strcase (getvar "DIMLDRBLK")))))
) 
(setq #Arrow (length ArrowBlockList))
)
						;;
;|						;;
	ARROW BLOCKS				;;
						|;





;|						;;
		DiLEADER ICON			;;
						|;
						;;
(defun dileadericon (pointer entd / ra iangle p1 p2 p3 p4 viewsize)
  (setq iangle (+ (angle pointer (cdr (assoc 10 entd)))
                  (* pi (/ 90.0 180.0))
               )
  )
  (setq viewsize (getvar "VIEWSIZE"))
  (if (and (> (/ (* iangle 180) pi) 90.0)
           (< (/ (* iangle 180) pi) 180.0)
      )
    (setq ra 135)
    (setq ra 45)
  )
  (setq p1 (polar pointer ra (/ viewsize 70.0)))
  (setq p2 (polar p1 (* pi 0.5) (/ viewsize 90.0)))
  (setq p3 (polar p1 0.0 (/ viewsize 90.0)))
  (setq p4 (polar p1 (* pi 0.25) (/ viewsize 50.0)))
  (redraw)
  (grdraw p1 p2 gripColor 0)
  (grdraw p1 p3 gripColor 0)
  (grdraw p1 p4 gripColor 0)
)
						;;
;|						;;
		DiLEADER ICON			;;
						|;





;|						;;
		E D I T O R			;;
						|;
						;;
(defun c:diledit (/ sbs        llead      lleadprop  vla-qdlitem           ltextdata  valeur     type1      ltexti
                    fltext     ent        #val1      #lwxd      ent        sbs        xdata#1    xdata#2    xdata#3
                    dileader_onedit       iib        vlaob
                   )
  (setq co1_dist nil
        co1_angl nil
        co2_dist nil
        co2_angl nil
        co1_coorl nil
        co2_coorl nil
        co1_coorr nil
        co2_coorr nil
        thandle nil
        textdata nil
        entdata nil
        ltext nil
        dltextmode nil
        tmod nil
        cursp nil
        xdata#1 nil
        xdata#2 nil
        xdata#3 nil
  )


(setq gripColor (getvar "GripColor"))
  (setq dileader_onedit t)
  (princ "\nSelect Text/Leader Object...")
  (setq input (grread t 4 2)) ;(while (not xdata#1)
  (while (and (not xdata#1)
              (setq input (grread t 4 2))
              (or (= (car input) 3) (= (car input) 5) (= (car input) 11))
         )
    (redraw)
    (if (= (car input) 11)
      (exit)
    )
    ;;ICON
    (if (setq iconsel (car (nentselp (cadr input))))
      (progn (setq iconseldata (entget iconsel))
             (setq lent (vlax-ename->vla-object iconsel))
             (vla-getxdata lent "DLEADER_TEXT" 'type1 'valeur)
             ;;si Contient le Xdata de Dileader
             (if valeur
               (dileadericon (cadr input) iconseldata)
             )
      )
    )
    (setq entname nil)
    ;;SELECTION
    (if (= (car input) 3)
      (setq entname (car (nentselp (cadr input))))
    )
    (getdimldrblkvariable)
    ;;si une entité a été sélectionné
    (if entname
      (progn (setq vla_entname (vlax-ename->vla-object entname))
             ;;si c'est un Leader
             (if (eq (vla-get-objectname vla_entname) "AcDbLeader")
               (progn (setq valeur nil)
                      (setq leader_entdata (entget entname))
                      (vla-getxdata vla_entname "DLEADER_TEXT" 'type1 'valeur)
                      ;;si Contient le Xdata de Dileader
                      (if valeur
                        (progn (setq lent vla_entname)
                               (setq dimldrblk (strcat "_" (strcase (vla-get-arrowheadblock vla_entname))))
                               (if (eq dimldrblk "_")
                                 (setq dimldrblk ".")
                               )
                               (setq sbs (vlax-variant-value (nth 1 (vlax-safearray->list valeur))))
                               (setq xdata#1 (nth 0 (read sbs)))
                               ;;Block or Item Handle
                               (setq xdata#2 (nth 1 (read sbs)))
                               ;;Text Handle
                               (setq xdata#3 (nth 2 (read sbs)))
                               ;;sub-Object Handle
                        )
                      )
               )
             )
             ;;si c'Est un Text ou Mtext
             (if (or (eq (vla-get-objectname vla_entname) "AcDbText")
                     (eq (vla-get-objectname vla_entname) "AcDbMText")
                 )
               (progn (setq thand (cdr (assoc 5 (entget entname))))
                      (setq leader_with_xdata (ssget "X" '((0 . "LEADER") (-3 ("DLEADER_TEXT")))))
                      (if leader_with_xdata
                        (progn (setq #lwxd (sslength leader_with_xdata))
                               (setq #val1 (- #lwxd 1))
                               (repeat #lwxd
                                 (setq lent_x (vlax-ename->vla-object
                                                (setq lename (ssname leader_with_xdata #val1))
                                              )
                                 )
                                 (vla-getxdata lent_x "DLEADER_TEXT" 'type1 'valeur)
                                 (if valeur
                                   (progn (setq sbs (vlax-variant-value (nth 1 (vlax-safearray->list valeur))))
                                          (setq xdata#1_x (nth 0 (read sbs)))
                                          ;;Item Handle
                                          (setq xdata#2_x (nth 1 (read sbs)))
                                          ;;Text Handle
                                          (setq xdata#3_x (nth 2 (read sbs)))
                                          ;;Object Handle
                                          (if (eq (strcase xdata#2_x) (strcase thand))
                                            (progn (setq leader_entdata (entget lename))
                                                   (setq dimldrblk (strcat "_" (strcase (vla-get-arrowheadblock lent_x))))
                                                   (if (eq dimldrblk "_")
                                                     (setq dimldrblk ".")
                                                   )
                                                   (setq lent    lent_x
                                                         xdata#1 xdata#1_x
                                                         xdata#2 xdata#2_x
                                                         xdata#3 xdata#3_x
                                                   )
                                            )
                                          )
                                   )
                                 )
                                 (setq #val1 (1- #val1))
                               ) ;_repeat
                        ) ;_progn
                      ) ;_if
               ) ;_progn
             ) ;_if
      ) ;_progn
    ) ;_if
  ) ;_while
  (setq dltextmode nil)
  (if (handent xdata#2)
    (if (entget (handent xdata#2))
      (progn (setq dltextmode (cdr (assoc 0 (entget (handent xdata#2)))))
             (setq dl_textsize (cdr (assoc 40 (entget (handent xdata#2)))))
      )
    )
  )
  
  (setq vlaob nil
        iib nil
  )
  (setq objecthandle xdata#1)
  (setq texthandle xdata#2)
  (setq insideblockhandle xdata#3)
  (if (read xdata#3)
    (if (handent xdata#1)
      (if (entget (handent xdata#1))
        (progn (setq vlaob (vlax-ename->vla-object (handent xdata#1)))
               (setq iib (vlax-ename->vla-object (handent xdata#3)))
        )
      )
    )
  )
  (redraw)
  (dleaderwhile (handent xdata#2)  
                (vlax-ename->vla-object (handent xdata#1))
                leader_entdata
  )
) ;_defun

						;;
;|						;;
		E D I T O R			;;
						|;


  

;|					;;
	R E A C T O R S			;;
					|;
					;;
(defun Dleader_ObjectWasEdited (isGrip / DLobj DLobj_data Llead)
(setvar "CMDECHO" 0)
  
(if isGrip
  (setq DLobj (cadr (ssgetfirst)))
  (setq DLobj (ssget "_P"))
)

(if DLobj
  (progn
      (setq sscount (sslength DLobj))
      (setq val1 (- sscount 1))
    
        (repeat sscount
          (setq DLobj_data (entget (ssname DLobj val1)))
          (setq DLobj_5 (cdr (assoc 5 DLobj_data)))



          (setq Leader_with_Xdata (ssget "X"
			     '((0 . "LEADER")			  
			       (-3 ("DLEADER_TEXT"))
			      )
		      )
          )


          (if Leader_with_Xdata
	(progn
          
	  (setq #LWXD (sslength Leader_with_Xdata))	   
	  (setq #val1 (- #LWXD 1))
	   
	  (repeat #LWXD
	    (setq ent (vlax-ename->vla-object (ssname Leader_with_Xdata #val1)))
           ;; (setq ent (vlax-ename->vla-object (Car (entsel))))
	    (vla-getxdata ent "DLEADER_TEXT" 'type1 'valeur)
	    (setq sbs (vlax-variant-value
			(nth 1 (vlax-safearray->list valeur))
		      )
	    )	    
            (setq Xdata#1 (nth 0 (read sbs)));;Item Handle
	    (setq Xdata#2 (nth 1 (read sbs)));;Text Handle
            (setq Xdata#3 (nth 2 (read sbs)));;Object Handle

            (if Xdata#2
              (setq TEXTobj (handent Xdata#2))
              (setq TEXTobj nil)
            )

            (if (read Xdata#3)
              (setq BOHand (handent Xdata#3))
              (setq BOHand nil)
            )

	    (if	(eq Xdata#1 DLobj_5)
              (progn                
                (setq cursp (cdr (assoc 10 (reverse (entget (ssname Leader_with_Xdata #val1))))))

(Dleader_Updated_By_reactor
                    cursp
                    cen
                    (vlax-ename->vla-object (ssname DLobj val1))
                    (ssname Leader_with_Xdata #val1)
                    TEXTobj
                    BOHand  ;is block
                    )

              )
            )            
	    (setq #val1 (1- #val1))
	  ) ;_repeat
	) ;_progn
      )

          (setq val1 (1- val1))

       );_repeat
    );_progn
  );_if
);_defun








;;Reactor on ended MOVE
(defun *dleader_object_modification* (call-reactor sci /)
  (if (member (car sci)
              (list "GRIP_MOVE" "GRIP_STRETCH" "GRIP_SCALE" "GRIP_ROTATE")
      )
    (dleader_objectwasedited t)
  )
  (if (member (car sci) (list "MOVE" "STRETCH" "ROTATE" "SCALE"))
    (dleader_objectwasedited nil)
  )
)


(defun dleader_run_reac ()
  (if dleader_object_modification
    (progn (vlr-remove dleader_object_modification)
           (setq dleader_object_modification nil)
    )
  )
  (setq dleader_object_modification
         (vlr-command-reactor nil
                              '((:vlr-commandended . *dleader_object_modification*))
         )
  )
)

(DLEADER_run_reac)
					;;
;|					;;
	R E A C T O R S			;;
					|;

