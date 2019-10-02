
;; By Joe Burke

;; Comments and bug reports may be sent to lowercase@hawaii.rr.com.

;; What does CC2 do which ExpressTools extrim, AKA CookieCutter, doesn't?
;; Works with blocks, hatches and regions by exploding them.
;; Other object types which cannot be trimmed are left intact.
;; Works with objects which do not use a Continuous linetype.
;; Offers an option to delete all objects on visible layers either
;; inside or outside the selected trim object.

;; The interface is similar to extrim.

;; First extrim prompt: 
;; Pick a POLYLINE, LINE, CIRCLE, ARC, ELLIPSE, IMAGE or TEXT for cutting edge...
;; Select objects:
;; Confusing because the routine does not allow multiple object selection.
;; Plus it works with some object types not mentioned, like splines. 

;; First CookieCutter2 prompt:
;; Select circle or closed polyline, ellipse or spline for trimming edge:
;; The object must be closed or appear to be closed.

;; Second extrim prompt:
;; Specify the side to trim on:

;; Second CookieCutter2 prompt:
;; Pick point on side to trim:

;; Third CookieCutter2 prompt:
;; One of the following depending on whether the point picked is inside
;; or outside the trim object.
;;   Erase all objects inside? [Yes/No] <N>:
;;   Erase all objects outside? [Yes/No] <N>:
;;   If Yes, all objects on visible layers are erased. If No it behaves
;;   like extrim.

;; Both CC2 and extrim only operate on objects on visible layers.

;; The routine will display an additional prompt if one or more solid 
;; hatches intersects the trim object.
;;   Convert solid hatch to lines? [Yes/No] <N>:
;;   If Yes, solid hatches are converted to lines using the ANSI31 pattern 
;;   and the lines are trimmed. If No, solid hatches are not trimmed.

;; Miscellaneous Notes:

;; The routine may be used to simply erase all objects inside or 
;; outside the trim object.

;; The routine does not trim annotation objects such as text, mtext,
;; dimensions, leaders, mleaders and tables. The user may choose to 
;; explode some of these objects types before running the routine.

;; It ignores xrefs. Bind xrefs beforehand if those block objects 
;; should be trimmed.

;; Some cleanup may be needed after the routine ends.

;; The routine offsets the selected trim object inside or outside in
;; order to determine trim points. The offset distance is a variable 
;; which depends on the size if the trim object. Likewise, if solid
;; hatches are converted to lines, the scale of the ANSI31 pattern 
;; depends on the same variable.

;; The routine will end (exit) if offset fails or offset creates more
;; than one new object. Message at the command line:
;; "Problem detected with selected object. Try another. Exiting... "

;; Self-intersecting trim objects are not allowed. The select object
;; part of the routine checks for this and cycles if a self-intersecting
;; object is selected.

;; Version history:

;;; Version 1.0 posted at theswamp 8/26/2008.

;;; Version 1.1 9/25/2008. Minor bug fix to set the correct layer of
;;; an attribute converted to text after exploding a block.

;;; Version 1.2 posted at theswamp 11/28/2008. Fix a bug reported by cjw. 
;;; Example, the delete all inside or outside option is chosen.
;;; The end of a line which is inside or outside the trim object
;;; is on the edge of the trim object. The line should be deleted.
;;; It was not before.

;;; Version 1.2 posted at theswamp 09/21/2006 by VVA (Vladimir Azarko).
;;; Updated version for localized (and not only) version of AutoCAD


(defun CookieCutter2 ( / *error* *acad* doc ps osm as om emode pmode offd 
                          elev locked typ typlst e d notclosed splinetyp 
                          i o intpts lst sc minpt maxpt hidelst dellst 
                          offsetename offsetobj trimename trimobj curcoord 
                          mark postlst coord reg selfinter ext UCSpkpt
                          UCStrimobjpts WCStrimobjpts delother side 
                          ssinside ssall sscross ssoutside ssintersect  
                          solidflag solidans solidlst sskeep sstest testename 
                          WCSoffsetobjpts UCSoffsetobjpts 
                          CC:GetScreenCoords CC:TraceObject CC:GetInters 
                          CC:SpinBar CC:AfterEnt CC:CommandExplode 
                          CC:ExpNestedBlock CC:FirstLastPts CC:GetBlock 
                          CC:AttributesToText CC:UniformScale 
                          CC:SSVLAList CC:Inside CC:UnlockLayers 
                          CC:RelockLayers CC:ZoomToPointList Extents)

  (defun *error* (msg)
    (cond
      ((not msg))
      ((wcmatch (strcase msg) "*QUIT*,*CANCEL*"))
      (T (princ (strcat "\nError: " msg)))
    )
    (setvar "pickstyle" ps)
    (setvar "osmode" osm)
    (setvar "autosnap" as)
    (setvar "edgemode" emode)
    (setvar "projmode" pmode)
    (setvar "orthomode" om)
    (setvar "elevation" elev)
    (setvar "offsetdist" offd)
    (setvar "cmdecho" 1)
    (if (and offsetobj (not (vlax-erased-p offsetobj)))
      (vla-delete offsetobj)
    )
    (if testename (entdel testename))
    (foreach x hidelst 
      (if (not (vlax-erased-p x))
        (vlax-put x 'Visible acTrue)
      )
    )
    (if (and trimobj (not (vlax-erased-p trimobj)))
      (vla-highlight trimobj acFalse)
    )
    (CC:RelockLayers locked)
    (vla-EndUndoMark doc)
    (princ)
  ) ;end error

  ;;;; START SUB-FUNCTIONS ;;;;

  ;; by Tony Tanzillo
  ;; Returns the lower left and upper right corners of a point list.
  (defun Extents (plist)
     (list
        (apply 'mapcar (cons 'min plist))
        (apply 'mapcar (cons 'max plist))
     )
  ) ;end

  ;; Argument: WCS point list.
  ;; In lieu of (command "zoom" "object"...) which requires 2005 or later.
  (defun CC:ZoomToPointList (pts)
    (setq pts (Extents pts))
    (vlax-invoke *acad* 'ZoomWindow (car pts) (cadr pts))
    (vlax-invoke *acad* 'ZoomScaled 0.85 acZoomScaledRelative)
  ) ;end

  ;; Unlock any locked layers in the active file.
  ;; Returns a list of unlocked layers if any.
  (defun CC:UnlockLayers (doc / laylst)
    (vlax-for x (vla-get-Layers doc)
      ;filter out xref layers
      (if 
        (and 
          (not (vl-string-search "|" (vlax-get x 'Name)))
          (eq :vlax-true (vla-get-lock x))
        )
        (progn
          (setq laylst (cons x laylst))
          (vla-put-lock x :vlax-false)
        )
      )
    )
    laylst
  ) ;end

  ;; Argument: a list of layer objects from CC:UnlockLayers.
  (defun CC:RelockLayers (lst)
    (foreach x lst
      (vl-catch-all-apply 'vla-put-lock (list x :vlax-true))
    )
  ) ;end

  ;Returns the coordinates of the current view, lower left and upper right.
  ;Works in a rotated view.
  (defun CC:GetScreenCoords ( / ViwCen ViwDim ViwSiz VptMin VptMax)
   (setq ViwSiz (/ (getvar "VIEWSIZE") 2.0)
         ViwCen (getvar "VIEWCTR")
         ViwDim (list
                 (* ViwSiz (apply '/ (getvar "SCREENSIZE")))
                 ViwSiz
                )
         VptMin (mapcar '- ViwCen ViwDim)
         VptMax (mapcar '+ ViwCen ViwDim)
   )
   (list VptMin VptMax)
  ) ;end

  ;; By John Uhden. Return T if point is inside point list.
  ;; Check how many intersections found with an "infinite" line (like a ray).
  ;; If the number intersections is odd, point is inside.
  ;; If the number intersections is even, point is outside. 
  (defun CC:Inside (p ptlist / p2 i n #) 
     ;; define a point at a sufficiently large distance from p... 
     (setq p2 (polar p 0.0 (distance (mapcar '+ (getvar "extmin") '(0 0)) ;_ Mod by VVA
				     (mapcar '+ (getvar "extmax") '(0 0)) ;_ Mod by VVA
				     )))
     ;; Make sure the ptlist is closed... 
     (if (not (equal (car ptlist) (last ptlist) 1e-10))
       (setq ptlist (append ptlist (list (car ptlist))))
     ) 
     (setq i 0 # 0 n (1- (length ptlist)))
     (while (< i n)
        (if (inters p p2 (nth i ptlist)(nth (1+ i) ptlist))
           (setq # (1+ #))
        )
        (setq i (1+ i))
     )
     (not (zerop (rem # 2)))
  ) ; end CC:Inside 

  ;Argument: selection set.
  ;Returns: list of VLA objects.
  (defun CC:SSVLAList (ss / obj lst i)
    (setq i 0)
    (if ss
      (repeat (sslength ss)
        (setq obj (vlax-ename->vla-object (ssname ss i))
              lst (cons obj lst)
              i (1+ i)
        )
      )
    )
    (reverse lst)
  ) ;end

  ;; Returns a list of primary enames after ename ent.
  ;; Filter out sub-entities and entities not in current space. 
  (defun CC:AfterEnt (ent / lst entlst)
    (while (setq ent (entnext ent))
      (setq entlst (entget ent))
      (if 
        (and
          (not (wcmatch (cdr (assoc 0 entlst)) "ATTRIB,VERTEX,SEQEND"))
          (eq (cdr (assoc 410 entlst)) (getvar "ctab"))
        )
        (setq lst (cons ent lst))
      )
    )
    (reverse lst)
  ) ;end

  (defun CC:SpinBar (sbar)
    (cond ((= sbar "\\") "|")
          ((= sbar "|") "/")
          ((= sbar "/") "-")
          (t "\\")
    )
  ) ;end

  (defun CC:TraceObject (obj / typlst typ ZZeroList TracePline 
                               TraceCE TraceSpline)

    ;;;; start trace sub-functions ;;;;

    ;; Argument: 2D or 3D point list.
    ;; Returns: 3D point list with zero Z values.
    (defun ZZeroList (lst)
      (mapcar '(lambda (p) (list (car p) (cadr p) 0.0)) lst)
    )

    ;; Argument: vla-object, a heavy or lightweight pline.
    ;; Returns: WCS point list if successful.
    ;; Notes: Duplicate adjacent points are removed.
    ;; The last closing point is included given a closed pline.
    (defun TracePline (obj / param endparam anginc tparam pt blg 
                             ptlst delta inc arcparam flag)

      (setq param (vlax-curve-getStartParam obj)
            endparam (vlax-curve-getEndParam obj)
            ;anginc (* pi (/ 7.5 180.0)) ;;;; note 7.5 here vs 2.5 at circle
            anginc (* pi (/ 2.5 180.0)) ;; the two should be the same
      )

      (while (<= param endparam)
        (setq pt (vlax-curve-getPointAtParam obj param))
        ;Avoid duplicate points between start and end.
        (if (not (equal pt (car ptlst) 1e-12))
          (setq ptlst (cons pt ptlst))
        )
        ;A closed pline returns an error (invalid index) 
        ;when asking for the bulge of the end param.
        (if 
          (and 
            (/= param endparam)
            (setq blg (abs (vlax-invoke obj 'GetBulge param)))
            (/= 0 blg)
          )
          (progn
            (setq delta (* 4 (atan blg)) ;included angle
                  inc (/ 1.0 (1+ (fix (/ delta anginc))))
                  arcparam (+ param inc)
            )
            (while (< arcparam (1+ param))
              (setq pt (vlax-curve-getPointAtParam obj arcparam)
                    ptlst (cons pt ptlst)
                    arcparam (+ inc arcparam)
              )
            )
          )
        )
        (setq param (1+ param))
      ) ;while

      (if (> (length ptlst) 1)
        (progn
          (setq ptlst (vl-remove nil ptlst))
          (ZZeroList (reverse ptlst))
        )
      )
    ) ;end

    ;; Argument: vla-object, an arc, circle or ellipse.
    ;; Returns: WCS point list if successful.
    (defun TraceCE (obj / startparam endparam anginc 
                           delta div inc pt ptlst)
      ;start and end angles
      ;circles don't have StartAngle and EndAngle properties.
      (setq startparam (vlax-curve-getStartParam obj)
            endparam (vlax-curve-getEndParam obj)
            ;;;;;;;;;;;;;; note change here, was using 7.5 ;;;;;;;;;;;;;
            ;anginc (* pi (/ 7.5 180.0))
            ;; This version is from SuperFlatten.
            ;; I think it returns a tighter trace.
            anginc (* pi (/ 2.5 180.0))   
      )

      (if (equal endparam (* pi 2) 1e-6)
        (setq delta endparam)
        ;added abs 6/23/2007, testing
        (setq delta (abs (- endparam startparam)))
      )

      ;Divide delta (included angle) into an equal number of parts.
      (setq div (1+ (fix (/ delta anginc)))
            inc (/ delta div)
      )

      ;Or statement allows the last point on an open ellipse
      ;rather than using (<= startparam endparam) which sometimes
      ;fails to return the last point. Not sure why.
      (while
        (or
          (< startparam endparam)
          (equal startparam endparam 1e-12)
          ;(equal startparam endparam)
        )
        (setq pt (vlax-curve-getPointAtParam obj startparam)
              ptlst (cons pt ptlst)
              startparam (+ inc startparam)
        )
      )
      (ZZeroList (reverse ptlst))
    ) ;end

    (defun TraceSpline (obj / startparam endparam ncpts inc param 
                              fd ptlst pt1 pt2 ang1 ang2 a)
      (setq startparam (vlax-curve-getStartParam obj)
            endparam (vlax-curve-getEndParam obj)
            ncpts (vlax-get obj 'NumberOfControlPoints)
            inc (/ (- endparam startparam) (* ncpts 6))
            param (+ inc startparam)
            fd (vlax-curve-getfirstderiv obj param)
            ptlst (cons (vlax-curve-getStartPoint obj) ptlst)
      )

      (while (< param endparam)
        (setq pt1 (vlax-curve-getPointAtParam obj param)
              ang1 (angle pt1 (mapcar '+ pt1 fd))
              param (+ param inc)
              pt2 (vlax-curve-getPointAtParam obj param)
              fd (vlax-curve-getfirstderiv obj param)
              ang2 (angle pt2 (mapcar '+ pt2 fd))
              a (abs (@delta ang1 ang2))
        )
        (if (> a 0.00436332)
          (setq ptlst (cons pt1 ptlst))
        )
      )
      ;add last point and check for duplicates
      (if 
        (not 
          (equal 
            (setq pt1 (vlax-curve-getEndPoint obj)) (car ptlst) 1e-8))
        (setq ptlst (cons pt1 ptlst))
      )
      (ZZeroList (reverse ptlst))
    ) ;end

    ;;;; primary trace function ;;;;
    (setq typlst '("AcDb2dPolyline" "AcDbPolyline" "AcDbSpline" 
                   "AcDbCircle" "AcDbEllipse")
    )
    (or 
      (eq (type obj) 'VLA-OBJECT)
      (setq obj (vlax-ename->vla-object obj))
    )

    (setq typ (vlax-get obj 'ObjectName))

    (if (vl-position typ typlst)
      (cond
         ((or (eq typ "AcDb2dPolyline") (eq typ "AcDbPolyline")) 
           (cond
             ((or
                (not (vlax-property-available-p obj 'Type))
                (= 0 (vlax-get obj 'Type))
               )
               (TracePline obj)
             )
           )
         )
         ((or (eq typ "AcDbCircle") (eq typ "AcDbEllipse"))
           (TraceCE obj)
         )
         ((eq typ "AcDbSpline")
           (TraceSpline obj)
         )
      )
    )
  ) ;end CC:TraceObject

  ; Arguments: 
  ;  firstobj: first object - ename or vla-object
  ;  nextobj: second object - ename or vla-object
  ;  mode - extend options
  ;   acExtendNone: extend neither object
  ;   acExtendThisEntity: extend first object
  ;   acExtendOtherEntity: extend second object
  ;   acExtendBoth: extend both objects
  ; Returns a WCS point list or nil if intersection not found.
  (defun CC:GetInters (firstobj nextobj mode / coord ptlst)
    (if (= (type firstobj) 'ENAME)
      (setq firstobj (vlax-ename->vla-object firstobj)))
    (if (= (type nextobj) 'ENAME)
      (setq nextobj (vlax-ename->vla-object nextobj)))
    (if
      (not 
        (vl-catch-all-error-p 
          (setq coord (vl-catch-all-apply 'vlax-invoke 
            (list firstobj 'IntersectWith nextobj mode)))
        )
      )
      (repeat (/ (length coord) 3)
        (setq ptlst (cons (list (car coord) (cadr coord) (caddr coord)) ptlst))
        (setq coord (cdddr coord))
      )
    )
    (reverse ptlst)
  ) ;end

  ;; Note 7/24/2008, saw the annonymous *E81 block thing again as in
  ;; SuperFlatten. It happens when trying to explode an NUS block.
  ;; In this case a grid block (was xref bound) was NUS. The grid lines
  ;; were exploded, but the column blocks inside it were not.
  ;; All of them were placed in the *E81 block.
  ;; I suppose there might be a report about this. At the end you could
  ;; check the blocks which remain in the drawing. If any has a name
  ;; like *E81, report, "A non-uniformly scaled block could not be exploded."
  (defun CC:CommandExplode (obj / lay mark attlst name exlst newattlst)
    (setq mark (entlast))
    (if 
      (and
        (not (vlax-erased-p obj))
        (eq "AcDbBlockReference" (vlax-get obj 'ObjectName))
      )
      (progn
        (setq lay (vlax-get obj 'Layer)
              attlst (vlax-invoke obj 'GetAttributes)
        )
        (vl-cmdf "._explode" (vlax-vla-object->ename obj))
        ;; Is this still fixing error in error handler?
        ;; Yes it is IMPORTANT!
        (command)
        (if 
          (and 
            (not (eq mark (entlast)))
            (setq exlst (CC:SSVLAList (ssget "_p")))
          )
          (progn
            (setq newattlst (CC:AttributesToText attlst))
            (foreach x exlst
              (if (eq "AcDbAttributeDefinition" (vlax-get x 'ObjectName))
                (vla-delete x)
              )
            )
            (setq exlst (vl-remove-if 'vlax-erased-p exlst))
            (if newattlst (setq exlst (append exlst newattlst)))
            ;If an exlpoded object is on layer 0, put it on the
            ;layer of the exploded object. If its color is byBlock, 
            ;change color to byLayer.
            (foreach x exlst
              (if (eq "0" (vlax-get x 'Layer))
                (vlax-put x 'Layer lay)
              )
              (if (zerop (vlax-get x 'Color))
                (vlax-put x 'Color 256)
              )
            )
          )
        )
      )
    ) ;if 

    ;(setq exlst (vl-remove-if 'vlax-erased-p exlst))
    (foreach x exlst
      (if 
        (and
          (not (vlax-erased-p x))
          (eq "AcDbBlockReference" (vlax-get x 'ObjectName))
        )
        (CC:ExpNestedBlock x)
      )
    )
  ) ;end CC:CommandExplode

  ;; Argument: block reference vla-object.
  ;; Explode the block passed and any nested blocks.
  ;; Doesn't deal with attributes yet. Convert to text.
  ;; Based on code by TW-Vacation at theswamp.
  ;; Leave this function as is. Trying to condense it 
  ;; will only cause problems.
  (defun CC:ExpNestedBlock (obj / lay lst attlst)
    ;; Do SpinBar here because exploding many blocks is what
    ;; causes the routine to take a long time in some cases.
    (princ 
      (strcat "\rProcessing blocks... " 
        (setq *sbar (CC:SpinBar *sbar)) "\t")
    )
    (if 
      (and 
        obj
        (not (vlax-erased-p obj))
      )
      (cond
        ((not (CC:UniformScale obj))
          (CC:CommandExplode obj)
        )    
        (T
          (setq lay (vlax-get obj 'Layer))
          (if (eq "AcDbBlockReference" (vlax-get obj 'ObjectName))
            (setq attlst (CC:AttributesToText (vlax-invoke obj 'GetAttributes)))
          )
          ;; This is primarily intended to catch NUS blocks which
          ;; the explode method can't handle.
          (setq lst (vl-catch-all-apply 'vlax-invoke (list obj 'Explode)))
          (if attlst (setq lst (append lst attlst)))
          (if (listp lst)
            (foreach x lst
              ;; This update call is important!
              (vla-update x) ;testing
              (if (eq "AcDbBlockReference" (vlax-get x 'ObjectName))
                (CC:ExpNestedBlock x)
                (progn
                  (if 
                    (and 
                      (not (vlax-erased-p x))
                      (eq "0" (vlax-get x 'Layer))
                    )
                    (vlax-put x 'Layer lay)
                  )
                  ;; If color is byblock, change to bylayer.
                  (if 
                    (and 
                      (not (vlax-erased-p x))
                      (zerop (vlax-get x 'Color))
                    )
                    (vlax-put x 'Color 256)
                  )
                  (if 
                    (and
                      (not (vlax-erased-p x))
                      (eq "AcDbAttributeDefinition" (vlax-get x 'ObjectName))
                    )
                    (vla-delete x)
                  )
                )
              )
            )
          )
          (vla-delete obj)
        )
      ) ;cond
    ) ;if
  ) ;end

  ;; Allow an object which is not closed, but has equal first and last points, 
  ;; to pass the test.
  (defun CC:FirstLastPts (obj / p1 p2)
    (setq p1 (vlax-curve-getPointAtParam obj (vlax-curve-getStartParam obj)))
    (setq p2 (vlax-curve-getPointAtParam obj (vlax-curve-getEndParam obj)))
    (equal p1 p2 1e-10)
  )

  (defun CC:GetBlock ()
    (vlax-get (vla-get-ActiveLayout doc) 'Block)
  ) ;end

  ;; Convert a list of attribute reference objects to text objects.
  ;; The list returned is used in CC:ExpNestedBlock.
  (defun CC:AttributesToText (attlst / elst res)
    (foreach x attlst
      (setq elst (entget (vlax-vla-object->ename x)))
      (if
        (entmake
          (list
            '(0 . "TEXT")
            (cons 1 (vlax-get x 'TextString))
            (cons 7 (vlax-get x 'StyleName))
            (cons 8 (vlax-get x 'Layer))
            (cons 10 (vlax-get x 'InsertionPoint))
            (cons 11 (vlax-get x 'TextAlignmentPoint))
            (cons 40 (vlax-get x 'Height))
            (cons 41 (vlax-get x 'ScaleFactor))
            (cons 50 (vlax-get x 'Rotation))
            (cons 51 (vlax-get x 'ObliqueAngle))
            (cons 62 (vlax-get x 'Color))
            (cons 67 (cdr (assoc 67 elst)))
            (cons 71 (cdr (assoc 71 elst)))
            (cons 72 (cdr (assoc 72 elst)))
            (cons 73 (cdr (assoc 73 elst)))
            (cons 410 (cdr (assoc 410 elst)))
          )
        ) ;make
        (setq res (cons (vlax-ename->vla-object (entlast)) res))
      )
    )
    res
  ) ;end

  ;; Return T ig block is uniformly scaled within fuzz range.
  (defun CC:UniformScale (obj / x y z)
    (and
      (or
        (= (type obj) 'VLA-object)
        (if (= (type obj) 'ENAME)
          (setq obj (vlax-ename->vla-object obj))
        )
      )
      (or 
        (wcmatch (vlax-get obj 'ObjectName) "*Dimension")
        (and
          (= "AcDbBlockReference" (vlax-get obj 'ObjectName))
          (setq x (vlax-get obj 'XScaleFactor))
          (setq y (vlax-get obj 'YScaleFactor))
          (setq z (vlax-get obj 'ZScaleFactor)) 
          (and
            ;; this fuzz 1e-8 seems sufficient for this application
            ;; it does not involve transformby which seems more sensitive
            ;; to NUS blocks
            (equal (abs x) (abs y) 1e-8)
            (equal (abs y) (abs z) 1e-8)
          )
        )
      )
    )
  ) ;end

  ;; Added 7/28/2008
  ;; Arguments: ename or vla-object and an intersection point list.
  ;; Returns: the original point list if an error occurs due to object type.
  ;; Otherwise the point list sorted by param at point along the curve.
  ;; Notes: the order of the point list returned by IntersectWith is
  ;; unpredictable. Sorting the point list allows multiple trim operations
  ;; on an object to occur in more predictable fashion.
  (defun SortInterPoints (obj pts / lst)
    (if 
      (vl-catch-all-error-p
        (vl-catch-all-apply 'vlax-curve-getEndParam (list obj))
      )
      pts
      (progn
        (setq lst (mapcar '(lambda (y) (vlax-curve-getParamAtPoint obj y)) pts)
              lst (mapcar '(lambda (y z) (list y z)) lst pts)
              lst (vl-sort lst '(lambda (a b) (< (car a) (car b))))
        )
        (mapcar 'cadr lst)
      )
    )
  ) ;end

  ;;;; END SUB-FUNCTIONS ;;;;

  ;;;; START MAIN FUNCTION ;;;;

  (vl-load-com)
  (setq *acad* (vlax-get-acad-object)
        doc (vla-get-ActiveDocument *acad*)
  )
  (vla-StartUndoMark doc)

  (setq locked (CC:UnlockLayers doc))

  ;; Avoid problems with groups.
  (setq ps (getvar "pickstyle"))
  (setvar "pickstyle" 0)
  (setvar "cmdecho" 0)
  (setq elev (getvar "elevation"))
  ;; So the Z value of the point picked (inside or outside)
  ;; is not at the current elevation.
  (setvar "elevation" 0.0)
  (setq osm (getvar "osmode"))
  (setvar "osmode" 0)
  ;; polar and ortho should be off too?
  (setq as (getvar "autosnap"))
  (setvar "autosnap" 0)
  (setq om (getvar "orthomode"))
  (setvar "orthomode" 0)
  ;; These following added 8/14/2008
  (setq emode (getvar "edgemode"))
  (setvar "edgemode" 0)
  (setq pmode (getvar "projmode"))
  (setvar "projmode" 0)
  (setq offd (getvar "offsetdist"))

  (sssetfirst)
  
  (setq typlst '("AcDbCircle" "AcDbPolyline" "AcDb2dPolyline" 
                 "AcDbEllipse" "AcDbSpline"))

  (setvar "errno" 0)

  (while 
    (or
      (not (setq e (car (entsel 
        "\nSelect circle or closed polyline, ellipse or spline for trimming edge: "))))
      (not (setq trimobj (vlax-ename->vla-object e)))
      (not (vl-position (setq typ (vlax-get trimobj 'ObjectName)) typlst))
      (and 
        (not (CC:FirstLastPts trimobj))
        (setq notclosed T)
      )
      (and
        (wcmatch typ "*Polyline")
        (vlax-property-available-p trimobj 'Type)
        (not (zerop (vlax-get trimobj 'Type)))
        (setq splinetyp T)
      )
      ;; Test for self-intersecting pline or spline.
      ;; Concept by Tony Tanzillo. If region fails the object
      ;; probably intersects itself. Seems reliable so far.
      (and
        (wcmatch typ "*Polyline,AcDbSpline")
        (vl-catch-all-error-p
          (setq reg 
            (vl-catch-all-apply 'vlax-invoke 
              (list (CC:GetBlock) 'AddRegion (list trimobj))
            )
          )
        )
        (setq selfinter T)
      )
    )
    (cond
      ((= 52 (getvar "errno"))
        (exit)
      )
      ((not e)
        (princ "\n Missed pick. ")
      )
      (selfinter
        (princ "\n Selected object intersects itself, try again. ")
        (setq selfinter nil)
      )
      (notclosed
        (princ "\n Selected object is not closed, try again. ")
        (setq notclosed nil)
      )
      (splinetyp
        (princ "\n Polyline spline selected, try again. ")
        (setq splinetyp nil)
      )
      (typ
        (princ (strcat "\n " (substr typ 5) " selected, try again. "))
        (setq typ nil)
      )
    )
  )

  ;; Delete region if one was created.
  (if 
    (and 
      reg 
      (not (vl-catch-all-error-p reg))
    )
    (vla-delete (car reg))
  )

  (setq trimename (vlax-vla-object->ename trimobj))

  ;; View to restore at end.
  (setq curcoord (CC:GetScreenCoords))

  ;; Highlighting the trim object helps in crouded situations.
  (vla-highlight trimobj acTrue)

  (initget 1)
  (setq UCSpkpt (getpoint "\nPick point on side to trim: "))
  (setq WCStrimobjpts (CC:TraceObject trimobj))
  (setq UCStrimobjpts 
    (mapcar '(lambda (x) (trans x 0 1)) WCStrimobjpts)
  )
  (if (CC:Inside UCSpkpt UCStrimobjpts)
    (setq side "inside")
    (setq side "outside")
  )

  (setq ext (Extents WCStrimobjpts))
  (setq d (distance (car ext) (cadr ext)))
  ;; d is used below to specify offset distance.
  (setq d (/ d 1500.0))
  
  ;; testing for decimal units
  ;; initial test indicates this may be needed
  ;; An exploded hatch was trimmed better with this.
  ;; Keep this for now.
  (if (= 2 (getvar "lunits")) 
    (setq d (/ d 12.0))
  )

  (setq mark (entlast))
  
  (vl-cmdf "._offset" d (vlax-vla-object->ename trimobj) UCSpkpt "_exit")

  (setq offsetename (entlast))

  (if (/= 1 (length (setq dellst (CC:AfterEnt mark))))
    (progn
      (princ "\nProblem detected with selected object. Try another. Exiting... ")
      ;; If offset created multiple objects they need to be deleted. 
      ;; This can happen with a spline.
      ;; Also exit if offset failed.
      (foreach x dellst (entdel x))
      (exit)
    )
  )

  (setq offsetobj (vlax-ename->vla-object offsetename))
  (vlax-put offsetobj 'Visible 0)
  (setq hidelst (cons offsetobj hidelst))

  (initget "Yes No")
  (setq delother (getkword (strcat "\nErase all objects " side "? [Yes/No] <N>: ")))
  (if (not delother) (setq delother "No"))

  (vlax-invoke *acad* 'ZoomExtents)
  (setq sc (CC:GetScreenCoords))
  ;; These are 2D points.
  (setq minpt (car sc))
  (setq maxpt (cadr sc))

  ;; This must follow zoom extents.
  (vlax-put trimobj 'Visible 0)
  (setq hidelst (cons trimobj hidelst))

  ;; Explode blocks which intersect the trim object first. 
  ;; Deal with hatches and regions afterwards.
  (setq sscross (ssget "_cp" UCStrimobjpts '((0 . "INSERT"))))
  (if (not (setq ssinside (ssget "_wp" UCStrimobjpts '((0 . "INSERT")))))
    (setq ssinside (ssadd))
  )

  (setq i 0)
  (if sscross
    (repeat (sslength sscross)
      (setq e (ssname sscross i))
      (if 
        (and
          (not (ssmemb e ssinside))
          (setq o (vlax-ename->vla-object e))
          (not (vlax-erased-p o))
          (vlax-property-available-p o 'Path)
        )
        (progn
          ;; Hiding true xrefs here. If the block was not 
          ;; erased/explode above then hide it. The reason for
          ;; this nonsense method is sometimes after an xref
          ;; is bound, AutoCAD thinks it is still an xref.
          ;; There's no way to test for this condition AFAIK.
          ;; (Command "explode"...) can explode a false xref.
          ;; So this cond passes all xref blocks to the 
          ;; CommandExplode function. If it fails to explode
          ;; then make the xref invisible. Note, there will be a 
          ;; non-fatal message generated within the CommandExplode
          ;; function when the block is really an xref.
          ;; "The object is an external reference." 
          ;; Just have to live with that.
          ;; Also, the explode method cannot be used on false xrefs.
          ;; The reason for attention to this problem is the user
          ;; may bind xrefs before running the routine.
          (CC:CommandExplode o)
          (if (not (vlax-erased-p o))
            (progn
              (vlax-put o 'Visible 0)
              (setq hidelst (cons o hidelst))
            )
          )
        )
        ;else
        (CC:ExpNestedBlock o)
      )
      (setq i (1+ i))
    )
  )

  ;; Solid hatches...
  (setq i 0 sscross nil ssinside nil)
  (setq sscross (ssget "_cp" UCStrimobjpts '((0 . "HATCH"))))
  (if (not (setq ssinside (ssget "_wp" UCStrimobjpts '((0 . "HATCH")))))
    (setq ssinside (ssadd))
  )
  ;; Just check for solid hatces.
  (if sscross
    (repeat (sslength sscross)
      (setq e (ssname sscross i))
      (if 
        (and
          (not (ssmemb e ssinside))
          (setq o (vlax-ename->vla-object e))
          (eq "AcDbHatch" (vlax-get o 'ObjectName))
          (eq "SOLID" (vlax-get o 'PatternName))
        )
        (setq solidflag T 
              solidlst (cons e solidlst)
        )
      )
      (setq i (1+ i))
    )
  ) ;if

  (if solidflag
    (progn
      (initget "Yes No")
      (setq solidans (getkword "\nConvert solid hatch to lines? [Yes/No] <N>: "))
      (if (eq "Yes" solidans)
        (foreach x solidlst
          ;; check for erased?
          (command "._-hatchedit" x
             "_properties" "ANSI31" (* d 8) 0.0)
          ;; Prevent message, "Hatch boundary associativity removed."
          (vlax-put (vlax-ename->vla-object x) 'AssociativeHatch 0)
          (command "._explode" x)
        )
      )
    )
  )

  ;; Now regions and not solid hatches.
  (setq i 0 sscross nil ssinside nil)
  (setq sscross (ssget "_cp" UCStrimobjpts '((0 . "HATCH,REGION"))))
  (if (not (setq ssinside (ssget "_wp" UCStrimobjpts '((0 . "HATCH,REGION")))))
    (setq ssinside (ssadd))
  )
  ;; Ignore solid hatches. If any still exist the user answered No to question.
  (if sscross
    (repeat (sslength sscross)
      (setq e (ssname sscross i))
      (if 
        (and
          (not (ssmemb e ssinside))
          (not (vl-position e solidlst))
        )
        (progn
          ;; Prevent message, "Hatch boundary associativity removed."
          (setq o (vlax-ename->vla-object e))
          (if (vlax-property-available-p o 'AssociativeHatch)
            (vlax-put o 'AssociativeHatch 0)
          )
          (command "._explode" e)
        )
      )
      (setq i (1+ i))
    )
  )

  (setq sscross nil ssinside nil)

  ;; Note: xrefs and the trim object are invisible at this point
  ;; so they are not included in following selections.
  (setq ssall (ssget "_c" minpt maxpt))
  ;; Selection set of objects completely inside trimobj.
  (if (not (setq ssinside (ssget "_wp" UCStrimobjpts)))
    (setq ssinside (ssadd))
  )
  ;; Selection set of all objects crossing trimobj.
  (if (not (setq sscross (ssget "_cp" UCStrimobjpts))) ;var added
    (setq sscross (ssadd))
  )

  ;; now ssoutside can be set
  (setq i 0)
  (setq ssoutside (ssadd))
  (repeat (sslength ssall)
    (setq e (ssname ssall i))
    (if (not (ssmemb e sscross))
      (ssadd e ssoutside)
    )
    (setq i (1+ i))
  )

  ;; ssintersect - objects which intersect the trim object.
  (setq i 0)
  (setq ssintersect (ssadd))
  (repeat (sslength sscross)
    (setq e (ssname sscross i))
    (if 
      (and
        (not (ssmemb e ssinside))
        (not (vl-position e solidlst))
        ;; Added intersect test 8/7/2008.
        ;; Was removed, put back 8/19/2008. Seems OK.
        ;; If the following returns nil then trim will fail.
        ;; "Cannot TRIM this object." This can happen with
        ;; some unusual spline objects.
        (CC:GetInters e trimobj acExtendNone)
        ;(not (eq e trimename))
      )
      (ssadd e ssintersect)
      (ssadd e ssinside)
    )
    (setq i (1+ i))
  )

  ;; Added check 8/22/2008.
  ;; Likely only applies to an ellipse as trim object.
  ;; An ellipse is converted to a spline when offset.
  ;; For some unknown reason the trim object may be 
  ;; included in the objects which are erased. It should not
  ;; happen since the trim object is invisible as this point.
  ;; Regardless, this check fixes a bug which may cause the
  ;; trim object to be erased. Which in turn causes other problems.
  (if (eq "Yes" delother)
    (cond
      ((eq side "inside")
        (ssdel trimename ssinside) ;check
        (command "._erase" ssinside "")
      )
      ((eq side "outside")
        (ssdel trimename ssoutside) ;check
        (command "._erase" ssoutside "")
      )
    )
  )

  ;; List of VLA-objects which intersect the trim object.
  (setq lst (CC:SSVLAList ssintersect))

  ;; Remove these object types from list to trim.
  ;; There is error checking elsewhere which should prevent
  ;; errors with other object types which cannot be trimmed.
  ;; Note 8/17/2008 - the only hatches which still exist are
  ;; solid hatches which the user chose not to convert to lines.
  ;; So hatches can be added here.
  (setq lst
    (vl-remove-if
      '(lambda (x)
        (setq typ (vlax-get x 'ObjectName))
        (or
          (eq "AcDbText" typ)
          (eq "AcDbMText" typ)
          (eq "AcDbLeader" typ)
          (wcmatch typ "*Dimension")
          (eq "AcDbHatch" typ)  ;; added 8/17/2008
          (eq "AcDbSolid" typ)
          (eq "AcDbTrace" typ)
          (eq "AcDbMLeader" typ)
          ;; Likely not needed, Added 8/22/2008.
          (eq trimobj x)
        )
      )
      lst
    )
  )

  (CC:ZoomToPointList WCStrimobjpts)

  ;;; Start primary loop ;;;
  
  (foreach x lst
    ;; Helps with trimming closed plines.
    (if (not (vlax-erased-p x))
      (progn
        (setq typ (vlax-get x 'ObjectName))
        (cond
          ((and
            (eq "AcDbPolyline" typ)
            (= -1 (vlax-get x 'Closed))
           )
            (vlax-put x 'Closed 0)
            (setq coord (vlax-get x 'Coordinates))
            (vlax-put x 'Coordinates 
              (append coord (list (car coord) (cadr coord)))
            )
            (vla-update x)
          ) 
          ((and
            (eq "AcDb2dPolyline" typ)
            (= -1 (vlax-get x 'Closed))
           )
            (vlax-put x 'Closed 0)
            (setq coord (vlax-get x 'Coordinates))
            (vlax-put x 'Coordinates 
              (append coord (list (car coord) (cadr coord) (caddr coord)))
            )
            (vla-update x)
          )
        )
      )
    )

    (if (setq intpts (CC:GetInters offsetobj x acExtendNone))
      (progn
        ;; More than two points seems good here and below.
        (if (> (length intpts) 2)
          (setq intpts (SortInterPoints x intpts))
        )
        (foreach p intpts
          (setq mark (entlast))
          (if 
            (and
              (not (vl-catch-all-error-p 
                (vl-catch-all-apply 'vlax-curve-getParamAtPoint (list x p)))
              )
              (vlax-curve-getParamAtPoint x p)
            )
            (vl-cmdf "._trim" trimename "" 
              (list (vlax-vla-object->ename x) (trans p 0 1)) "")
          )
          (if (not (eq mark (entlast)))
            (setq postlst (cons (entlast) postlst))
          )
        )
      )
    )
  )

  ;;; End primary loop ;;;

  ;; This part trims any new objects created above.
  (while postlst 
    (setq intpts nil)
    (foreach x postlst
      (if (setq intpts (CC:GetInters offsetobj x acExtendNone))
        (progn 
          (if (> (length intpts) 2)
            (setq intpts (SortInterPoints x intpts))
          )
          (foreach p intpts
            (setq mark nil) ; is this needed?
            (setq mark (entlast))
            (if 
              (and
                (not (vl-catch-all-error-p 
                  (vl-catch-all-apply 'vlax-curve-getParamAtPoint (list x p)))
                )
                (vlax-curve-getParamAtPoint x p)
              )
              (vl-cmdf "._trim" trimename "" (list x (trans p 0 1)) "")
            )
            (setq postlst (vl-remove x postlst))
            (if (not (eq mark (entlast)))
              (setq postlst (cons (entlast) postlst))
            )
          )
          (setq postlst (vl-remove x postlst))
        )
        (setq postlst (vl-remove x postlst))
      )
    )
  )

  ;; Following code added in version 1.2 which deals with left over
  ;; objects either inside or outside the trim object which should be erased.
  (if 
    (and 
      (eq "Yes" delother)
      trimobj
      offsetobj
      (not (CC:GetInters offsetobj trimobj acExtendNone))
    )
    (cond
      ((and
         (eq side "inside")
         (setq WCSoffsetobjpts (CC:TraceObject offsetobj))
         (setq UCSoffsetobjpts 
           (mapcar '(lambda (x) (trans x 0 1)) WCSoffsetobjpts)
         )
       )
        (if (setq sstest (ssget "_cp" UCSoffsetobjpts))
          (command "._erase" sstest "")
        )
      )
      ((eq side "outside")
        (setq mark (entlast))
        ;; multiply be 2 or 3?
        (vl-cmdf "._offset" (* d 3) offsetename UCSpkpt "_exit")
        (if 
          (and 
            (not (eq mark (setq testename (entlast))))
            (not (CC:GetInters testename trimobj acExtendNone))
            (setq WCSoffsetobjpts (CC:TraceObject testename))
            (setq UCSoffsetobjpts 
              (mapcar '(lambda (x) (trans x 0 1)) WCSoffsetobjpts)
            )
          )
          (progn
            (setq sskeep (ssget "_wp" UCSoffsetobjpts))
            (vlax-invoke *acad* 'ZoomExtents)
            (setq sc (CC:GetScreenCoords)
                  minpt (car sc)
                  maxpt (cadr sc)
                  sstest (ssget "_c" minpt maxpt)
                  i 0
            )
            (if 
              (and 
                sskeep
                sstest
                (> (sslength sstest) (sslength sskeep))
              )
              (repeat (sslength sstest)
                (setq e (ssname sstest i))
                (if (not (ssmemb e sskeep))
                  (entdel e)
                )
                (setq i (1+ i))
              )
            )
          )
        )
      )
    ) ;cond
  ) ;if
  (if (setq sscross nil sscross   (ssget "_cp" UCStrimobjpts))
;;;Select sscross _previous    
    (command "_select" sscross "")
    )
  ;; Zoom to original view.
  (command "._zoom" "_window" (car curcoord) (cadr curcoord))
  (*error* nil)
) ;end

;------------------------------------
;shortcut
(defun c:CC () (CookieCutter2))
;------------------------------------
