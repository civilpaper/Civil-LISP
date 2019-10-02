(setvar "cmdecho" 0)
(defun DXF (code elist)
  (cdr (assoc code elist))
)

;-------------------------------------------------------------------
(Defun c:dthh( / X Y pt)
  (if (= tl nil) (progn
    (setq tl (getreal "\nDrawing scale : "))
    (setq ntl (/ 1000 tl))
    (setq tl2 (* ntl ntl))
    (setq tl3 (* tl2 ntl))
    (setq tl4 (* tl3 ntl))
    )
  );if TL

  (setq pt (getpoint "\n Select internal point :"))
  (command "-boundary" pt "")
  (setq ed (entget (entlast)))  
  (command "erase" "l" "")
  (setq n 1)
  (while (< n (length ed))
     (if (= (car (nth n ed)) 10)
       (progn 
         (setq pt (nth n ed)) 
         (setq X (append X (list (cadr pt))))
         (setq Y (append Y (list (nth 2 pt))))
       ) 
     )
     (setq n (+ n 1))
  )
  (setq X (append X (list (car X))))
  (setq Y (append Y (list (car Y))))
  (setq F1 0.0
        X1 0.0
        y1 0.0  
        Jx1 0.0
        Jy1 0.0
        i 0
        j 1
  )
  (while (< j (length X))
;----------------------------------------------tinh dien tich
     (setq F1 (+ F1 (* (- (nth i X) (nth j X)) (+ (nth i Y) (nth j Y)))))
;----------------------------------------------Tinh toa do trong tam theo phuong y
    (setq y1 (+ y1 (* (- (nth i X) (nth j X)) (+ (expt (nth i Y) 2) (* (nth i Y) (nth j Y)) (expt (nth j Y) 2))
                    )
              )
     ) 
;----------------------------------------------Tinh toa do trong tam theo phuong x
     (setq x1 (+ x1 (* (- (nth j Y) (nth i Y)) (+ (expt (nth i X) 2) (* (nth i X) (nth j X)) (expt (nth j X) 2))
                    )
              )
     ) 
;----------------------------------------------Tinh mo men quan tinh theo phuong x
    (setq Jx1 (+ Jx1 (* (- (nth i X) (nth j X)) (+ (expt (nth i Y) 3) (* (expt (nth i Y) 2) (nth j Y)) (* (nth i Y) (expt (nth j Y) 2)) (expt (nth j Y) 3))
                    )
              )
     ) 
;----------------------------------------------Tinh mo men quan tinh theo phuong y
     (setq Jy1 (+ Jy1 (* (- (nth J Y) (nth i Y)) (+ (expt (nth i X) 3) (* (expt (nth i X) 2) (nth j X)) (* (nth i X) (expt (nth j X) 2)) (expt (nth j X) 3))
                    )
              )
     ) 

     (setq i (+ i 1)
           j (+ j 1)
     )
  ) 
  (setq pt2 (getvar "ucsorg"))
  (setq F (/ F1 2))
  (setq Yc2 (/ y1 (* 6 F)))  
  (setq Xc2 (/ x1 (* 6 F)))  
  (setq Yc (- Yc2 (cadr pt2)))
  (setq Xc (- Xc2 (car pt2)))
  (setq Sx (* Yc F))  
  (setq Sy (* Xc F))  
  (setq Jx (/ (- (/ Jx1 12) (* (expt Yc2 2) F)) tl4))
  (setq Jy (/ (- (/ Jy1 12) (* (expt Xc2 2) F)) tl4))
  (setq F (/ F tl2))

  (prompt "\n Area F      :") 
  (princ (rtos F 2 6))
  (prompt "\n Centroid :")
  (prompt "\n         + Yc:") 
  (princ (rtos Yc 2 4)) 
  (prompt "\n         + Xc:") 
  (princ (rtos Xc 2 4)) 
  (prompt "\n Moment of inertia :")
  (prompt "\n         + Sx:") 
  (princ (rtos Sx 2 4)) 
  (prompt "\n         + Sy:") 
  (princ (rtos Sy 2 4)) 
  (prompt "\n Principal moments about centroid :")
  (prompt "\n         + Jx:") 
  (princ (rtos Jx 2 6)) 
  (prompt "\n         + Jy:") 
  (princ (rtos Jy 2 6)) 
  (prompt "\n")

//them
  (setq vsize ( /(getvar "VIEWSIZE") 5))
  (setq ptcen (list Xc Yc)
        ptw (list (car ptcen) (+ (cadr ptcen) 100)) )
  (command "insert" "centroid" ptcen "" "" "")  
  (if (/= 0 (DXF 40 (tblsearch "STYLE" (getvar "TEXTSTYLE"))))
    (command "text" "m" ptw "0" (strcat "F: " (rtos F 2 6)))
    (command "text" "m" (list (car ptcen) (+ (cadr ptcen) 100)) (/ vsize 6) 0 (strcat "F: " (rtos F 2 6)) )
  );if
  (princ)

)
(prompt "\n DTHH : Tinh dac trung hinh hoc")



