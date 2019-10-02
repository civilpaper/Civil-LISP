;;;=======================[ MakeEntmake.lsp ]==================================
;;; Author:  Charles Alan Butler Copyright© 2005-2012 
;;; Version: 1.5 Nov. 10, 2012
;;; Purpose: To create a lisp that will recreate the objects selected
;;;          Will not process xref or nested blocks, blocks are created
;;;          from Inserts but no insert is created, you must insert
;;; Output;  A lisp file with the name of the drawing contailing the lisp code
;;; Sub_Routines:
;;;          _replace -  replace in string
;;;          dxfstrip - Strip dxf from list
;;;          make_complex -  Write additional lines needed for complex objects
;;;          dxf -  return value from a dotted pair
;;;          ToString -  convert item to a string by MP
;;; Requirements: -None
;;; Returns: -None
;;; Original Thread:  http://www.theswamp.org/index.php?topic=4814.0
;;; Latest Version: http://www.theswamp.org/index.php?topic=31145
;;;===========================================================================
;;;  Note: No error checking at this time
;;;  This routine is a major rewrite of a routine found on the internet
;;;  and the author of the original code is unkown
;;;  Objects supported
;;;     "3DFACE" "3DSOLID" "ARC" "ATTDEF" "CIRCLE" "DIMENSION"
;;;     "ELLIPSE" "HATCH" "LEADER" "LWPOLYLINE" "LINE" "MTEXT"
;;;     "POINT" "RAY" "REGION" "SHAPE" "SOLID" "SPLINE" "TEXT"
;;;     "TRACE" "XLINE" "WIPEOUT" 
;;;     "TABLE" ; untested
;;;  Objects not supported at this time
;;;     MLine  Raster  Tolerance  XRecord  
;;;===========================================================================
;;;
;;;   THIS SOFTWARE IS PROVIDED "AS IS" WITHOUT EXPRESS OR IMPLIED     ;
;;;   WARRANTY.  ALL IMPLIED WARRANTIES OF FITNESS FOR ANY PARTICULAR  ;
;;;   PURPOSE AND OF MERCHANTABILITY ARE HEREBY DISCLAIMED.            ;
;;;                                                                    ;
;;;  You are hereby granted permission to use, copy and modify this    ;
;;;  software without charge, provided you do so exclusively for       ;
;;;  your own use or for use by others in your organization in the     ;
;;;  performance of their normal duties, and provided further that     ;
;;;  the above copyright notice appears in all copies and both that    ;
;;;  copyright notice and the limited warranty and restricted rights   ;
;;;  notice below appear in all supporting documentation.              ;
;;;
(defun C:MakeEntMake (/ idx dwg path ent entl fn sset dxfstrip 
                      rvflag tmp
                     )

;;; -=<*>=- -=<*>=- -=<*>=- -=<*>=- -=<*>=- -=<*>=- -=<*>=- -=<*>=-
;;;                      Local Functions                           
;;; -=<*>=- -=<*>=- -=<*>=- -=<*>=- -=<*>=- -=<*>=- -=<*>=- -=<*>=-


  ;;  Strip these dxf code pairs from original list
  (defun dxfstrip (ent / entl dxfx)
    (setq entl (entget ent)
          dxfx '(-2 -1 5 102 300 330 331 340 350 360 410)
          ;;  additional codes to strip
          dxfx (cond ((= (cdr (assoc 0 entl)) "DIMENSION")
                      (append dxfx '(2 210))
                     )
                     ((= (cdr (assoc 0 entl)) "HATCH")
                      (append dxfx '(67))
                     )
                     ((= (cdr (assoc 0 entl)) "INSERT")
                      (append dxfx '(40 41 42 43 44 45 50 71 210))
                     )
                     (t (append dxfx '(210)))
               )
          entl (vl-remove-if '(lambda (pair) (member (car pair) dxfx)) entL)
    )
    ;;   Added additional items to be removed in simple objects
    (if (vl-position (cdr(assoc 0 entl)) '("LINE""CIRCLE""ARC"))
      (SETQ entl (vl-remove-if '(lambda (pair) (= (car pair) 67)) entL)
            entl (vl-remove-if '(lambda (pair)
                                  (vl-position pair '((100 . "AcDbEntity")
                                                      (100 . "AcDbLine")
                                                      (100 . "AcDbCircle")
                                                      (100 . "AcDbArc")
                                                      ))) entL))
    )
    
    ;;  CORRECTIONS FOR SPECIAL OBJECTS
    (cond
      ;;  Make Hatch Non-associative
      ((= (cdr (assoc 0 entl)) "HATCH")
       (setq entl (subst '(71 . 0) '(71 . 1) entl))
       (setq entl (subst '(97 . 0) (assoc 97 entl) entl))
       (prompt "\n***  Warning Hatch created is Non-associative  ***")
      )
      ((= (cdr (assoc 0 entl)) "LEADER")
       (prompt "\n***  Warning Leader created is Non-associative  ***")
      )
    )
    ;; filter out objects that can not be created at this time
    entl
  ) ; defun

  ;;  Write additional lines needed for complex objects
  (defun make_complex (ent fn / entl)
    (while (and (setq ent (entnext ent))
                (setq vlst (entget ent))
                (member (cdr (assoc 0 vlst)) '("VERTEX" "SEQEND"))
           )
      (setq entl (dxfstrip ent))
      (write-line (strcat "  (entmake '" (ToString entl) ")") fn)
    )

  ) ; defun

  (defun xrefp (elist / blist)
    (and
      (setq blist (entget (tblobjname "BLOCK" (cdr (assoc 2 elist)))))
      (= 4 (logand (cdr (assoc 70 blist)) 4))
    ) ; and
  ) ; end defun

  ;; return value from a dotted pair
  (defun dxf (x data) (cdr (assoc x data)))
  
;;  CAB 11.04.07, updated 01.11.08
;;  This one is fool proof
(defun _replace (oldtext newtext textstring / i n)
  (setq n (strlen newtext))
  (while (setq i (vl-string-search oldtext textstring i))
    (setq textstring (vl-string-subst newtext oldtext textstring i)
          i    (+ i n))
  )
  textstring
)
  
;;;================================================================
;;;             S t a r t   o f   R o u t i n e                    
;;;================================================================

  ;;  Create the LISP file with the same name as the Drawing
  (setq path (getvar "DWGPREFIX")
        dwg  (strcat path (vl-filename-base (getvar "DWGNAME")) ".lsp")
        idx  0
  )
  ;;  Create a Revision Flag Using current Date & Time
  (setq tmp      (rtos (getvar 'cdate) 2 4))
  (setq rvflag (strcat (substr tmp 5 2)  "/"  (substr tmp 7 2) "/"
                       (substr tmp 1 4)  " @"  (substr tmp 10 2)  ":" (substr tmp 12 2)))

  ;;  Objects supported
  (setq include '("3DFACE" "3DSOLID" "ARC" "ATTDEF" "CIRCLE" "DIMENSION"
                  "ELLIPSE" "HATCH" "LEADER" "LWPOLYLINE" "LINE" "MTEXT"
                  "POINT" "RAY" "REGION" "SHAPE" "SOLID" "SPLINE" "TEXT"
                  "TRACE" "XLINE" "WIPEOUT" 
                  "TABLE" ; untested
                 )
  )
  ;;  Objects not supported at this time
  ;;  MLine Raster Tolerance XRecord  


  (cond
    ((setq sset (ssget)) ; get objects in selection set
     (prompt (strcat "\n" (itoa (sslength sset)) " primary objects selected."))
     (setq fn  (open dwg "w"))
     (write-line (strcat ";; Revision :" rvFlag) fn)
     (write-line (strcat "(defun c:eMake ()") fn)
     (repeat (sslength sset) ; Loop --------------
       (setq ent (ssname sset idx))
       (setq entl (dxfstrip ent))

       (cond
         ;;=========================================================
         ;; Process Complex Objects
         ;;=========================================================
         ((member (cdr (assoc 0 entl)) '("POLYLINE")) ; and 3DPOLY 
          (write-line (strcat "(entmake '" (ToString entl) ")") fn)
          (make_complex ent fn)
         )
         ;; Process Blocks
         ((and (member (cdr (assoc 0 entl)) '("INSERT"))
               (not (xrefp entl))
          ) ; exclude x-ref
          ;;  fix layer to 0 in header
          (setq entl (subst '(8 . "0") (assoc 8 entl) entl))
          (setq entl (subst '(0 . "BLOCK") '(0 . "INSERT") entl))
          (setq entl (subst '(100 . "AcDbBlockReference")
                              '(100 . "AcDbBlockBegin")  entl ))
          (setq entl (subst '(10 0.0 0.0 0.0) (assoc 10 entl) entl))
          
          ;; --------------------   CAB Nov. 10, 2010   -----------------------
          ;;  Problem with (entget (tblobjname not returning a valid -2 entity
          ;;  where the entity name is invalid
          ;;  (setq data (entget (tblobjname "block" (dxf 2 entl))))
          (setq data (tblsearch "block" (dxf 2 entl)))
          ;; --------------------   CAB Nov. 10, 2010   -----------------------
          
          (setq entl (subst (assoc 70 data) '(70 . 0) entl)) ; get flag

          (write-line (strcat "  (entmake '" (ToString entl) ")") fn)

          (setq data (dxf -2 data)) ; get first entity
          (setq data (entget data '("*"))) ; get assoc list
          (while data
            (cond
              ((= (dxf 0 data) "INSERT")
               (alert "Nested Blocks Not Supported")
               (close fn)
               (exit)
              )
              (t
               ;; process the object
               (setq entl (dxfstrip (DXF -1 data)))
               (write-line (strcat "  (entmake '" (ToString entl) ")") fn)
               (if (> (cdr (assoc 66 entl)) 0) ; if  complex entity
                 (make_complex ent fn)
               ) ; endif
               ;; get next object
               (and (setq data (entnext (dxf -1 data)))
                    (setq data (entget data '("*"))) ; get assoc list
               )
              )
            ) ; end cond stmt
          ) ;while

          ;;     This is the end of block marker          
          (write-line
            "  (entmake '((0 . \"ENDBLK\") (100 . \"AcDbBlockEnd\") (8 . \"0\")))"
            fn
          )
         )


         ;;=========================================================
         ;;  Process all other Objects 
         ;;=========================================================
         ((member (cdr (assoc 0 entl)) include)
          (write-line (strcat "  (entmake '" (ToString entl) ")") fn)
         )

         ;;=========================================================
         ;;  Rejected Objects 
         ;;=========================================================
         (t
          (prompt
            (strcat "\n***  Object not supported. [ " (cdr (assoc 0 entl)) " ]  ***") )
         )
         ;;=========================================================
       ) ; end cond stmt 

       (setq idx (1+ idx))
     ) ; repeat  End Loop  ---------------------
     (write-line (strcat "  (princ)") fn)
     (write-line (strcat ") ; end eMaker") fn)
     (close fn)
     (prompt (strcat "\nFile Created: " dwg))
    )
  ) ; end cond stmt
  (princ)
)

;;  Subroutine by Michael Pucket 04/15/2005
;;  Returns a string
(defun ToString (x / typex)
  ;;  convert item to a string, if x is a real use 
  ;;  the highest possible precision, if x is a 
  ;;  string double quote it, if x is a list process 
  ;;  each item in the list appropriatel, otherwise 
  ;;  just hammer item with vl-princ-to-string

  ;;  add a leading zero if needed
  ;;  this is caused by DIMZIN supressing leading zeros
  (defun addzero(st)
    (if (= (substr st 1 1) ".")
      (strcat "0" st)
      st
    )
  )

  (cond
    ;;  it's a string, return it double quoted
    ;;  CAB added _replace to catch imbeded \\ & quotes within the string
    ((eq 'str (setq typex (type x)))
     (setq x (_replace "\\" "\\\\" x))
     (strcat "\"" (_replace "\"" "\\\"" x) "\"")
    )
    ;;  if n.0 do not add extra 0's
    ((and (eq 'real typex) (= (- x (fix x)) 0.0))
     (addzero (rtos x 2 1))
    )
    ;;  it's a real, covert to the highest possible 
    ;;  resolution string equivalent 
    ((eq 'real typex)
     (vl-string-right-trim "0" ; by CAB 
       (addzero  (rtos x 2 (if (zerop (- x (fix x))) 1 15))))
    )

    ;;  it's a list 
    ((eq 'list typex)
     (if (vl-list-length x)
       ;;  it's a normal list 
       (strcat
         (chr 40)
         (ToString (car x))
         (apply 'strcat
                (mapcar  '(lambda (x) (strcat " " (ToString x))) (cdr x))
         )
         (chr 41)
       )

       ;;  it's a dotted pair 
       (strcat (chr 40) (ToString (car x)) " . " (ToString (cdr x)) (chr 41))
     )
    )

    ;;  hammer down on everything else 
    ((vl-princ-to-string x))
  )
)

(princ "\nEntity to lisp file loaded, Enter MakeEntMake to run.")
(princ)

