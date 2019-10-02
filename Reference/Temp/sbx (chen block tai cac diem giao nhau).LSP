(defun c:sbx ( / )
  (progn
    (setq ent (car (entsel "\nSelect main line: ")))
    (if ent
      (progn
 (princ "\nSelect crossing line(s): ")
 (if (setq ss (ssget))
   (progn
     (setq count 0
    obj (vlax-ename->vla-object ent)
    pointlist nil
    )
     (repeat (sslength ss)
       (setq xent (ssname ss count)
      xobj (vlax-ename->vla-object xent)
      )
       (if (setq int (vla-IntersectWith obj xobj acExtendNone))
  (progn
    (setq int (vlax-safearray->list (vlax-variant-value int))
   pointlist (append pointlist (list int))
   )
    )
  )
       (setq count (1+ count))
       )
     (if (null (tblobjname "BLOCK" "SBblock"))
       (progn
  (entmake (list (cons 0 "BLOCK") (cons 2 "SBblock") (cons 70 0) (list 10 0.0 0.0 0.0)))
  (entmake '((0 . "LWPOLYLINE") (100 . "AcDbEntity") (8 . "0") (100 . "AcDbPolyline") (90 . 2) (70 . 1) (43 . 1.0) (38 . 0.0) (39 . 0.0)
      (10 2.5 0.0) (40 . 1.0) (41 . 1.0) (42 . 1.0) (91 . 0)
      (10 -2.5 0.0) (40 . 1.0) (41 . 1.0) (42 . 1.0) (91 . 0)
      (210 0.0 0.0 1.0)
      )
    )
  (setq blockname (entmake '((0 . "ENDBLK"))))
  )
       )
     (foreach pt_nth pointlist
       (entmake (append
    '((0 . "INSERT") (100 . "AcDbEntity") (8 . "0") (100 . "AcDbBlockReference") (2 . "SBblock"))
    (list (cons 10 pt_nth))
    '((41 . 1.0) (42 . 1.0) (43 . 1.0) (50 . 0.0) (70 . 0) (71 . 0) (44 . 0.0) (45 . 0.0) (210 0.0 0.0 1.0))
    )
         )
       )
     )
   )
 )
      )
    )
  (princ)
  )