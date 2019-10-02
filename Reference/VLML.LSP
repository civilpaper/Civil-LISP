    (defun c:vlMl (/ p1 p2 label input *lead)
     (vl-load-com)
     
     (if (and (setq label (getstring T "\nSpecify label text"))
      (setq p1 (getpoint "\nPick Mulitleader start point"))
         )
     
     (progn (setq p2 p1)
    (princ "\nPick label location")
    (while	(and p2
        (setq input (grread t 9 0))
        (or (= (car input) 5) (= (car input) 3) (= (car input) 2))
    )
      (cond ((= (car input) 5) (mkmleader (cadr input))) ;tracking coordinates returned
     
    ((= (car input) 2) (setq p2 nil) (if *lead (vla-delete *lead))) ;keyboard input - exit
     
    ((= (car input) 3) (setq p2 nil));point picked - exit while loop
     
    (T (setq p2 nil)(if *lead (vla-delete *lead)))
      )
    )
       (princ)
     )
    )
     )
     
    (defun mkmleader ( p2 / pts)
     
     (if (not *lead);create the multileader if it doesn't exist
        (progn (setq pts (list (trans p1 1 0) (trans p2 1 0))
     pts (gc:3dPointListToVariant pts)
       )
      (setq *lead (vla-AddMLeader
    (vla-get-modelspace (vla-get-activedocument (vlax-get-acad-object)));get modelspace object
    pts
    0
         )
      )
      (vla-put-textstring *lead label)
      (vla-rotate
        *lead
        (vlax-3d-point (trans p2 1 0))
        (angle (trans '(0 0 0) 1 0) (trans '(1 0 0) 1 0))
      )
         )
       ;the object already exists so just update it
       (progn
         (setq pts (list (trans p1 1 0) (trans p2 1 0))
       pts (gc:3dPointListToVariant pts))
     
         (vla-SetLeaderLineVertices *lead 0 pts);redefine the vertices
         (vla-SetDogLegDirection *lead 0 (vlax-3D-point (trans (list (if (> (car p2) (car p1)) 1 -1) 0 0) 1 0 T)));change direction of dogleg according to cursor position relative to start point
         (vla-update *lead)
       )
     )
    )
     
    ;; Thanks to Gile!
    ;;
    ;; Return a variant of 3d coordinates
    ;;
    ;; Argument: a 3d points list -type (x y z)-
     
    (defun gc:3dPointListToVariant (lst)
     (vlax-make-variant
       (vlax-safearray-fill
         (vlax-make-safearray
           vlax-VbDouble
           (cons 0 (1- (* 3 (length lst))))
         )
         (apply 'append lst)
       )
     )
    )
     