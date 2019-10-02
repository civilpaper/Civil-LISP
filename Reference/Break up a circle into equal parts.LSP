;;;Break up a circle into equal parts 
(defun c:CBrk (/ ss tmp i j ent elst inc)
  (vl-load-com)

  (or Cbrk:num (setq Cbrk:num 2))

  (cond ((setq ss (ssget '((0 . "CIRCLE"))))
         
         (initget 6)
         (and (setq tmp (getint (strcat "\nSpecify Number of Sections <" (itoa Cbrk:num) "> : ")))
              (setq Cbrk:num tmp))

         (setq i -1)
         (while (setq ent (ssname ss (setq i (1+ i))))
           (setq elst (entget ent) inc (/ (* 2 pi) cBrk:num) j -1)

           (repeat cbrk:num
             (entmake (list (cons   0 "ARC")
                            (assoc  8  elst)
                            (assoc 10  elst)
                            (assoc 40  elst)
                            (cons  50 (* (setq j (1+ j)) inc))
                            (cons  51 (* (1+ j) inc)))))
         (entdel ent))))

  (princ))

;;;;;;Break up a circle into equal parts and
;;;;;;have lines drawn from the center to the edge of each arc.
;;;(defun c:CBrk (/ ss tmp i j ent elst inc)
;;;  (vl-load-com)
;;;
;;;  (or Cbrk:num (setq Cbrk:num 2))
;;;
;;;  (cond ((setq ss (ssget '((0 . "CIRCLE"))))
;;;         
;;;         (initget 6)
;;;         (and (setq tmp (getint (strcat "\nSpecify Number of Sections <" (itoa Cbrk:num) "> : ")))
;;;              (setq Cbrk:num tmp))
;;;
;;;         (setq i -1)
;;;         (while (setq ent (ssname ss (setq i (1+ i))))
;;;           (setq elst (entget ent) inc (/ (* 2 pi) cBrk:num) j -1)
;;;
;;;           (repeat cbrk:num
;;;             (entmake (list (cons   0 "ARC")
;;;                            (assoc  8  elst)
;;;                            (assoc 10  elst)
;;;                            (assoc 40  elst)
;;;                            (cons  50 (* (setq j (1+ j)) inc))
;;;                            (cons  51 (* (1+ j) inc))))
;;;             
;;;             (entmake (list (cons   0 "LINE")
;;;                            (assoc  8   elst)
;;;                            (assoc 10   elst)
;;;                            (cons  11 (polar (cdr (assoc 10 elst))
;;;                                             (* j inc)
;;;                                             (cdr (assoc 40 elst)))))))
;;;         (entdel ent))))
;;;
;;;  (princ))