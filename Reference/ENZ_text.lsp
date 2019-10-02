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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun text_num_ssget ( / ssl  nsset temp ed)
   
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
         (if (= (DXF 0 ed) "TEXT") 
            (if (or (= (type (read (DXF 1 ed))) 'REAL) (= (type (read  (DXF 1 ed))) 'INT))
               (ssadd temp nsset)
            )
         )
      )
   )
   (setq ssl (sslength nsset)
      sset nsset
   )
   (print ssl)
   (princ " numerical TEXT entities found. ")
   (princ)
);defun u_ssget
;------------------------------------------------------------------
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun fopen ( fname mode msg / fh )
   (setq fh (open fname mode))
   (defun *error* (msg)
      (princ msg)
      (princ)
   )
   (while (= fh nil)
      (progn
         (Prompt (strcat "\nFile " msg " Not Found !"))
         (setq fname (getfiled "Enter output Point Filename" "d:\\cuong\\" "pt" 33)
            fh (open fname mode)
         ) 
      )
   )
   fh
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun addzero (en)
   (if (= (substr (dxf 1 en) 1 1)  ".")
      (progn
         (setq old (cons 1 (DXF 1 en))
            new (cons 1 (strcat "0" (DXF 1 en)))
            en (subst new old en)
         )
         (entmod en)  
      );pro
      (progn
         (setq old (cons 1 (DXF 1 en))
            new (cons 1  (DXF 1 en))
            en (subst new old en)
         )
         (entmod en)  
      );pro
      
   );if
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(princ)
(prompt "\n \NET : Loc toa do cac TEXT cao do va ghi ra file - Lay DXF11")
(defun c:neT (/ sset ssl  no temp ed fh)
   (text_num_ssget)
   (setq ssl (sslength sset)
      no ssl
   )
   (print)
   (print)
   (setq fname (getfiled "Enter output Point Filename" "d:\\cuong\\" "pt" 33))
   (setq fh (fopen fname "a" fn))
   (setvar "DIMZIN" 0)
   (while (> ssl 0)
      (progn
         (setq temp (ssname sset (setq ssl (1- ssl))))       
         (setq ed (entget temp))
         (setq toado (DXF 11 ed))
         (setq x (nth 0 toado)
               y (nth 1 toado)
               h1 (dxf 1 ed)
         )
         (setq x1 (rtos x 2 4) y1 (rtos y 2 4))
         (write-line (strcat x1 "      " y1 "      " h1) fh)
      );progn
   );while
   (close fh)
   (princ)
);defun net()


