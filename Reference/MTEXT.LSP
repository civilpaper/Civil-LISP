(prompt "\n\nL O A D I N G    MT E X T . L S P")

(defun DXF (code elist)
   (cdr (assoc code elist))
)
(defun CDXF (tt elist)
   (car (nth tt elist))
)
;CDXF - xac dinh thu tu cua tieu danh sach
(defun xdtt (code elist)
   (setq i 0)
   (while (/= code (cdxf i elist))
      ;(progn
      (setq i (+ 1 i))
   );while
)
(defun RAD (degree)
   (setq radian (/ (* degree pi) 180))
   radian
)
(defun DEG (Rad)
   (setq degree (/ (* rad 180) pi))
   degree
)
(defun tangent (Rad)
   (setq tang (/ (sin rad) (cos rad)))
   tang
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mtext_u_ssget ( / ssl  nsset temp ed )
   (setq sset (ssget))
   (setq ssl (sslength sset) 
      nsset (ssadd)
   )
   (print ssl)
   (princ "entities found. ")  
   (princ "\nVerifying the selected entities ... please wait. ")
   (while (> ssl 0)
      (progn
         (setq temp (ssname sset (setq ssl (1- ssl))))
         (setq ed (entget temp))
         (if (= (DXF 0 ed) "MTEXT") (ssadd temp nsset))
      )
   )
   (setq ssl (sslength nsset)
      sset nsset
   )
   (print ssl)
   (princ "MULTI-TEXT entities found. ")
   (princ)
   (print)
);defun u_ssget
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun block_u_ssget ( / ssl  nsset temp ed )
   (setq sset (ssget))
   (setq ssl (sslength sset) 
      nsset (ssadd)
   )
   (print ssl)
   (princ "entities found. ")  
   (princ "\nVerifying the selected entities -- please wait. ")
   (while (> ssl 0)
      (progn
         (setq temp (ssname sset (setq ssl (1- ssl))))
         (setq ed (entget temp))
         (if (= (DXF 0 ed) "INSERT") (ssadd temp nsset))
      )
   )
   (setq ssl (sslength nsset)
      sset nsset
   )
   (print ssl)
   (princ "BLOCK entities found. ")
   (princ)
   (print)
);defun block_u_ssget
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(princ "\n		CM : To update MTEXT to style 2 and current size")
(princ)
(defun c:CM( / sset ssl temp ed old new )
   (Mtext_u_ssget)
   (if (= scale nil) (setq scale (getreal "\nInput current scale : ")))
   (setq ssl (sslength sset))
   (while (> ssl 0)
      (progn
         (setq temp (ssname sset (setq ssl (1- ssl))))
         
         (setq ed (entget temp))
         (progn
            (setq old (cons 7 (DXF 7 ed))
               sty "2"
               new (cons 7 sty)
               ed (subst new old ed)
            )
            (setq old (cons 40 (DXF 40 ed))
               H1 (DXF 40 ed)
               size (* 1.6 scale)
               new (cons 40 size)
               ed (subst new old ed)
            )
            (setq old (cons 41 (DXF 41 ed))
               Wtext (DXF 42 ed)
               size (* 1.6 scale (/ 1.0 H1) Wtext)
               new (cons 41 size)
               ed (subst new old ed)
            )
         )  
         
         (entmod ed)  
      )
   )
   (princ)
);defun cm
