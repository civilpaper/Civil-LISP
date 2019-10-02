
(defun DXF (code elist)
  (cdr (assoc code elist))
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun text_u_ssget ( / ssl  nsset temp ed )
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
      (if (= (DXF 0 ed) "TEXT") (ssadd temp nsset))
    )
  )
  (setq ssl (sslength nsset)
        sset nsset
  )
  (print ssl)
  (princ "TEXT entities found. ")
  (princ)
);defun u_ssget

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(princ)
(prompt "\nTS : To subtract the selected text entities to a given value")
(defun c:TS()
; sset ssl temp ed oldval newval old new subval oldlay newlay)
(text_u_ssget)
(print)

(print)
(setvar "DIMZIN" 0)
(if (= #ACC nil) (setq #ACC (getint "Input the nuber of fraction: ")))
(setq subval (getreal "Input the subtract value : "))


(setq ssl (sslength sset))
  (while (> ssl 0)
    (progn
      (setq temp (ssname sset (setq ssl (1- ssl))))

       (setq ed (entget temp))
         (PROGN
           (setq oldval (read (DXF 1 ed)))
           (setq newval (rtos (- oldval subval) 2 #ACC))
           (setq old (cons 1 (DXF 1 ed))
                 new (cons 1 newval)
                 ed (subst new old ed)
           );setq
           (setq oldlay (cons 8 (DXF 8 ed))
                 newlay (cons 8 "4")
                 ed (subst newlay oldlay ed)
           );setq

         );PROGN

       (entmod ed)  

    );progn
  );while
 (princ)
);defun TS()
