;;;; Chuyen vong tron thanh polyline
;;;; Co the chon nhieu doi tuong cung luc


(defun c:C2P (/ ss)
  ;; Circles to LWPolylines
  ;; Alan J. Thompson, 10.26.10
  (if (setq ss (ssget "_:L" '((0 . "CIRCLE"))))
    ((lambda (i / e d l)
       (while (setq e (ssname ss (setq i (1+ i))))
         (setq l nil)
         (foreach x (setq d (entget e)) (and (member (car x) '(6 8 39 48 62)) (setq l (cons x l))))
         (entmakex
           (append
             (list (cons 0 "LWPOLYLINE")
                   (cons 100 "AcDbEntity")
                   (cons 100 "AcDbPolyline")
                   (cons 90 2)
                   (cons 70 1)
             )
             l
             (list (cons 10 (polar (cdr (assoc 10 d)) 0. (cdr (assoc 40 d))))
                   '(42 . 1)
                   (cons 10 (polar (cdr (assoc 10 d)) pi (cdr (assoc 40 d))))
                   '(42 . 1)
             )
           )
         )
         (entdel e)
       )
     )
      -1
    )
  )
  (princ)
)