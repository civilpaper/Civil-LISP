;*******************************************************************************
;*                             Home tab                                        *
;*******************************************************************************
;************
;*Draw Panel*
;************
;; CVPOLY.LSP Copyright 1996, 1997  Tony Tanzillo  all rights reserved
;;
;; Converts 3D polylines to 2D polylines that lie on
;; the XY plane of the current UCS.

(setq filter:3dpoly
  '(  (-4 . "<AND")
      (0 . "POLYLINE")
      (-4 . "&=")
      (70 . 8)
      (-4 . "AND>")
   )
)

(defun get (key alist)
   (cdr (assoc key alist))
)

(defun UCSZdir ()
   (trans '(0 0 1) 0 1)
)

(defun UCSPoint2D (wcspoint)
   (  (lambda (p)
         (list (car p) (cadr p) 0.0)
      )
      (trans wcspoint 0 1)
   )
)

(defun mapvertex (e function / e d rslt)
   (while (/= "SEQEND" (get 0 (setq d (entget (setq e (entnext e))
'("*")))))
      (setq rslt
         (cons
            (  (lambda (function data / e d rslt)
                  (apply function (list data))
               )
               function
               d
            )
            rslt
         )
      )
   )
   (reverse rslt)
)

(defun cvpoly (pline / data ucs)
   (setq data (entget pline '("*")))
   (entmake
      (subst
         (cons 70 (logand (get 70 data) (~ 8)))
         (assoc 70 data)
         (subst
            (cons 210 (setq ucs (ucszdir)))
            (assoc 210 data)
            (subst
               '(100 . "AcDb2dPolyline")
               '(100 . "AcDb3dPolyline")
               data
            )
         )
      )
   )

   (mapvertex pline
     '(lambda (data)
         (if (zerop (logand (get 70 data) 9))
            (entmake
               (list
                 '(0 . "VERTEX")
                 '(100 . "AcDbEntity")
                  (assoc 8 data)
                 '(100 . "AcDbVertex")
                 '(100 . "AcDb2dVertex")
                  (cons 10 (UCSPoint2d (get 10 data)))
                  (cons 70 (logand (get 70 data) (~ 32)))
               )
            )
         )
      )
   )

   (entmake '((0 . "SEQEND")))

   (entdel pline)
)

(defun C:CVPOLY ( / ss i)
   (cond
      (  (not (setq ss (ssget filter:3dpoly)))
         (princ "\nNo 3D Polylines selected."))
      (t (setvar "CMDECHO" 0)
         (command "._UNDO" "_Begin")
         (repeat (setq i (sslength ss))
            (cvpoly (ssname ss (setq i (1- i))))
         )
         (command "._UNDO" "_En")
      )
   )
   (princ)
)

(princ "\nCVPOLY command loaded.")
(princ)
;; END CVPOLY.LSP
;************
;*Layers Panel*
;************
(defun c:0 ()
(setvar "clayer" "0")
(princ)
) 


