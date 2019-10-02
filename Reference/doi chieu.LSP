;LWPOLYLINE
(defun revlwpline (e / footer done vertices header flag)
					;reverse lightweight polyline
  (foreach item	(reverse (entget e))
    (cond
      ((not done)
       (cond
	 ((= (car item) 40)
	  (setq	footer (cons (cons 41 (cdr item)) footer) ;swap width
		done   t
	  )
	 )
	 ((= (car item) 41)
	  (setq footer (cons (cons 40 (cdr item)) footer)) ;swap width
	 )
	 ((= (car item) 42)
	  (setq footer (cons (cons 42 (- (cdr item))) footer))
					;negate bulge
	 )
	 ((= (car item) 210)
	  (setq footer (cons item footer))
	 )
       )
      )
      ((= (car item) 10)
       (setq vertices (cons item vertices))
      )
      ((= (car item) 40)
       (setq vertices (cons (cons 41 (cdr item)) vertices)) ;swap width
      )
      ((= (car item) 41)
       (setq vertices (cons (cons 40 (cdr item)) vertices)) ;swap width
      )
      ((= (car item) 42)
       (setq vertices (cons (cons 42 (- (cdr item))) vertices))
					;negate bulge
      )
      (t (setq header (cons item header)))
    )
  )
  (setq flag (assoc 70 header))
  (if (< (cdr flag) 128)		;turn on linetype generation
    (setq header (subst (cons 70 (+ (cdr flag) 128)) flag header))
  )
  (entmod (append header (reverse vertices) footer))
)
;------------------------------------------------------------------------------
;POLYLINE
(defun revhwpline (e / oldname old ent1 buldge end start ent tangent radians
                       vertex vertices flag)
  ;reverse heavyweight polyline
  (setq oldname  e
        old   (entget oldname)
        e     (entnext e)
        ent1  (entget e)                 ;get first vertex
        bulge (cdr (assoc 42 ent1))
        end   (cdr (assoc 41 ent1))
        start (cdr (assoc 40 ent1))
        e     (entnext e)
        ent   (entget e)                 ;get second vertex
  )
  (while (= (cdr (assoc 0 ent)) "VERTEX")
    (if (= (logand (cdr (assoc 70 ent)) 2) 2)
      (setq tangent (assoc 50 ent)
            radians (- (cdr tangent) pi) ;reverse tangent
            ent     (subst (cons 50 radians) tangent ent)
      )
    )
    (setq vertex   (subst (cons 42 (- bulge))(assoc 42 ent) ent)    ;negate bulge
          vertex   (subst (cons 41 start)    (assoc 41 ent) vertex) ;swap width
          vertex   (subst (cons 40 end)      (assoc 40 ent) vertex) ;swap width
          bulge    (cdr  (assoc 42 ent))
          end      (cdr  (assoc 41 ent))
          start    (cdr  (assoc 40 ent))
          vertices (cons vertex vertices)
          e        (entnext e)
          ent      (entget e)            ;get next vertex or seqend
    )
  )
  (setq flag (assoc 70 old))
  (if (< (cdr flag) 128)                 ;turn on linetype generation
    (setq old (subst (cons 70 (+ (cdr flag) 128)) flag old))
  )
  (entmake old)                          ;make new polyline
  (foreach ent vertices (entmake ent))   ;make new vertices
  (if (= (logand (cdr (assoc 70 ent1)) 2) 2)
    (setq tangent (assoc 50 ent1)
          radians (- (cdr tangent) pi)   ;reverse tangent
          ent1    (subst (cons 50 radians) tangent ent1)
    )
  )
  (setq ent1 (subst (cons 42 (- bulge))(assoc 42 ent1) ent1) ;negate bulge
        ent1 (subst (cons 41 start)    (assoc 41 ent1) ent1) ;swap width
        ent1 (subst (cons 40 end)      (assoc 40 ent1) ent1) ;swap width
  )
  (entmake ent1)                         ;make last new vertex
  (entmake ent)                          ;make new seqend
  (entdel oldname)                       ;delete old polyline
)
;;;;Bat dau ham
(defun c:rv (/ EN)
  (if (setq EN (car (entsel "- Chon duong Polyline de doi chieu")))
    (cond
      ((= "POLYLINE" (cdr (assoc 0 (entget EN))))
       (revhwpline EN)
       (prompt " - OK!")
      )
      ((= "LWPOLYLINE" (cdr (assoc 0 (entget EN))))
       (revlwpline EN)
       (prompt " - OK!")
      )
      (t (prompt "\n Doi tuong khong phai Polyline"))
    )
  )
  ;;Het ham
  (princ)
)
(c:rv)




