(defun DXF (code elist)
  (cdr (assoc code elist))
)
(defun CDXF (tt elist)
  (car (nth tt elist))
)

(defun xdtt (elist)
(setq i 0)
(while (/= 10 (cdxf i elist))
;(progn
  (setq i (+ 1 i))
);while
)

(defun refine ( / nsset en ed ssl)
  (setq ssl (sslength sset) 
        nsset (ssadd)
  )
  (print ssl)
  (princ "entities found. ")  
  (princ "\nVerifying the selected entities -- please wait. ")
  (while (> ssl 0)
    (progn
      (setq en (ssname sset (setq ssl (1- ssl))))
      (setq ed (entget en))
      (if (OR (= (DXF 0 ed) "LINE") (= (DXF 0 ed) "POLYLINE") (= (DXF 0 ed) "LWPOLYLINE")(= (DXF 0 ed) "ARC") (= (DXF 0 ed) "CIRCLE"))
         (ssadd en nsset)
      )
    )
  )
  (setq ssl (sslength nsset)
        sset nsset
  )
  (print ssl)
  (princ "Entities Found. ")

  (while (> ssl 0)
    (progn
      (setq en (ssname sset (setq ssl (1- ssl))))
      (setq ed (entget en))
      (if (= (DXF 0 ed) "LINE") 
      (progn
        (setq old (nth (XDTT ED) ed))
        (setq new (list (nth 0 old) (nth 1 old) (nth 2 old) 0.0))
        (setq ed (subst new old ed))

        (setq old (nth (+ 1 (XDTT ED)) ed))
        (setq new (list (nth 0 old) (nth 1 old) (nth 2 old) 0.0))
        (setq ed (subst new old ed))
        (entmod ed)  
      );progn
     );if LINE

      (if (= (DXF 0 ed) "LWPOLYLINE") 
      (progn
        (setq old (cons 38 (DXF 38 ED)))
        (setq new (cons 38 0.0))
        (setq ed (subst new old ed))

        (entmod ed)  
      );progn
     );if LWPOLYLINE

     (if (= (DXF 0 ed) "POLYLINE")
      (progn
;        (setq old (nth 8 ed))
;        (setq new (list (nth 0 old) (nth 1 old) (nth 2 old) 0.0))
         (setq old (nth (XDTT ED) ed))
         (setq new (list (nth 0 old) (nth 1 old) (nth 2 old) 0.0))
        (setq ed (subst new old ed))
        (entmod ed)  

        (setq en (entnext en))
        (while (/= (DXF 0 (entget en)) "SEQEND")
          (progn
            (setq ed (entget en))
            (setq old (nth (XDTT ED) ed))
            (setq new (list (nth 0 old) (nth 1 old) (nth 2 old) 0.0))
            (setq ed (subst new old ed))
          
            (entmod ed)  
            (setq en (entnext en))
          );progn
        );while
      );progn
    );if POLYLINE
    (if (= (DXF 0 ed) "CIRCLE") 
      (progn
        (setq old (nth (XDTT ED) ed))
        (setq new (list (nth 0 old) (nth 1 old) (nth 2 old) 0.0))
        (setq ed (subst new old ed))
        (entmod ed)  
      );progn
   );if CIRCLE

    (if (= (DXF 0 ed) "ARC") 
      (progn
        (setq old (nth (XDTT ED) ed))
        (setq new (list (nth 0 old) (nth 1 old) (nth 2 old) 0.0))
        (setq ed (subst new old ed))
        (entmod ed)  
      );progn
   );if ARC
 );progn
);while
);defun

(prompt "\nCZ: Change z coordinate to 0.0")
(prompt "\nNT Trong Le - Nguyen Quoc Cuong 12/1997")
(defun c:CZ()
(setq sset (ssget))
(refine)
)
