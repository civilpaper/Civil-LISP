;;;;;;Tac dung: Zoom mot cua so trong Layout trung voi cua so cua viewport
;;;Thuong ap dung khi muon zoom mot hinh chu nhat cua ben model trung voi duong vien cua viewport


;;;(defun c:cyclvp ( / ss i entvp )
;;;  (setvar 'tilemode 0)
;;;  (command "_.mspace")
;;;  (setq ss (ssget "_X" (list (cons 0 "VIEWPORT") (cons 410 (getvar 'ctab)))))
;;;  (setq i (- (sslength ss) 1))
;;;  (while (eq (getstring "\nENTER to cycle - any key+ENTER to exit : ") "")
;;;    (if (eq (setq i (1- i)) -1) (setq i (- (sslength ss) 2)))
;;;    (setq entvp (ssname ss i))
;;;    (setvar 'cvport (cdr (assoc 69 (entget entvp))))
;;;  )
;;;  (princ)
;;;)



(defun c:zw (/ pt1 pt2)
  (if
    (and
      (setq pt1 (getpoint "\nSelect First Window Point: "))
      (setq pt1 (osnap pt1 "_end"))
      (setq pt2 (getpoint "\nSelect Second Window Point: "))
      (setq pt2 (osnap pt2 "_end"))
    )
     (command "._zoom" "_w" "_non" pt1 "_non" pt2) ; end zoom
  )
  (princ)
)					; end program