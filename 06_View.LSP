;*******************************************************************************
;*                             View tab                                        *
;*******************************************************************************

;;;;;;;;;;; This command Tile Vertical Multiple Dwgs ;;;;;;;;;;;
(defun c:TV nil
  (vl-load-com)
  (vlax-for x (vla-get-documents (vlax-get-acad-object))
    (vla-put-windowstate x acNorm)
  )
  (command "syswindows" "vert")
  (princ)
)

    (defun c:DelObjectsNotOnAnyViewport ()
     (setq ssview (ssadd))
     (setvar 'ctab "MODEL")
     (setq app (vlax-get-acad-object))
     (vlax-for lay                         ; for each layout
                   (vla-get-layouts (vla-get-activedocument (vlax-get-acad-object)))
       (setq id1 nil)                      ; ignore the first vp
       (if (eq :vlax-false (vla-get-modeltype lay))
         (progn (vlax-for obj (vla-get-block lay) ; for each obj in layout
                  (if (and (= (vla-get-objectname obj) "AcDbViewport")
                           (or id1 (not (setq id1 t))) ; ignore first viewport because that is the viewport tab itself
                      )
                    (progn (vla-GetBoundingBox obj 'LPVP 'UPVP)
                           (setq LPVP (vlax-safearray->list LPVP))
                           (setq UPVP (vlax-safearray->list UPVP))
                           (setq LPMODEL (PCS2WCS LPVP (vlax-vla-object->ename obj)))
                           (setq UPMODEL (PCS2WCS UPVP (vlax-vla-object->ename obj)))
                           (setq minx (car LPMODEL))
                           (setq maxx (car UPMODEL))
                           (setq miny (cadr LPMODEL))
                           (setq maxy (cadr UPMODEL))
                           (setq pt1 (list minx miny))
                           (setq pt2 (list maxx miny))
                           (setq pt3 (list maxx maxy))
                           (setq pt4 (list minx maxy))
                           (vla-zoomwindow app (vlax-3d-point pt1) (vlax-3d-point pt3))
                           (if (setq ss (ssget "_CP" (list pt1 pt2 pt3 pt4) (list (cons 410 "MODEL"))))
                             (setq ssview (kdub:ssunion ssview ss))
                           )
                    )
                  )
                )
         )
       )
     )
     (setq ssall (ssget "_X" (list (cons 410 "MODEL"))))
     (setq sstodel (kdub:sssubtract ssall ssview))
     (repeat (setq n (sslength sstodel)) (setq ent (ssname sstodel (setq n (1- n)))) (entdel ent))
    )
     
     
    ;;; Union of two selection sets
    (defun kdub:ssunion (ss1 ss2 / ss index)
    ;;; Source : http://www.theswamp.org/index.php?topic=46652.0
     (setq ss (ssadd))
     (cond ((and ss1 ss2)
            (setq index -1)
            (repeat (sslength ss1) (ssadd (ssname ss1 (setq index (1+ index))) ss))
            (setq index -1)
            (repeat (sslength ss2) (ssadd (ssname ss2 (setq index (1+ index))) ss))
           )
           (ss1 (setq ss ss1))
           (ss2 (setq ss ss2))
           (t (setq ss nil))
     )
     ss
    )
     
    ;; Subtracts one selection set from another and returns their difference
    ;; NOT optimal because it changes the previous/last selection set.
    (defun kdub:sssubtract (ss1 ss2 / ss)
    ;;; Source : http://www.theswamp.org/index.php?topic=46652.0
     (cond ((and ss1 ss2) (vl-cmdf "._Select" ss1 "_Remove" ss2 "") (setq ss (ssget "_P")))
           (ss1 (setq ss ss1))
           (t (setq ss nil))
     )
     ss
    )
     
    (defun PCS2WCS (pnt ent / ang enx mat nor scl)
    ;;; Source : http://forums.autodesk.com/t5/Visual-LISP-AutoLISP-and-General/Auto-Update-of-XY-coord-in-Model-Space-onto-Paper-Space-Layout/td-p/4591789/page/2
     (setq pnt (trans pnt 0 0)
           enx (entget ent)
           ang (- (cdr (assoc 51 enx)))
           nor (cdr (assoc 16 enx))
           scl (/ (cdr (assoc 45 enx)) (cdr (assoc 41 enx)))
           mat (mxm (mapcar (function (lambda (v) (trans v 0 nor t))) '((1.0 0.0 0.0) (0.0 1.0 0.0) (0.0 0.0 1.0)))
                    (list (list (cos ang) (- (sin ang)) 0.0) (list (sin ang) (cos ang) 0.0) '(0.0 0.0 1.0))
               )
     )
     (mapcar '+
             (mxv mat (mapcar '+ (vxs pnt scl) (vxs (cdr (assoc 10 enx)) (- scl)) (cdr (assoc 12 enx))))
             (cdr (assoc 17 enx))
     )
    )
     
    ;; Matrix Transpose  -  Doug Wilson
    ;; Args: m - nxn matrix
     
    (defun trp (m) (apply 'mapcar (cons 'list m)))
     
    ;; Matrix x Matrix  -  Vladimir Nesterovsky
    ;; Args: m,n - nxn matrices
     
    (defun mxm (m n) ((lambda (a) (mapcar '(lambda (r) (mxv a r)) m)) (trp n)))
     
    ;; Matrix x Vector  -  Vladimir Nesterovsky
    ;; Args: m - nxn matrix, v - vector in R^n
     
    (defun mxv (m v) (mapcar '(lambda (r) (apply '+ (mapcar '* r v))) m))
     
    ;; Vector x Scalar  -  Lee Mac
    ;; Args: v - vector in R^n, s - real scalar
     
    (defun vxs (v s) (mapcar '(lambda (n) (* n s)) v))
     
     