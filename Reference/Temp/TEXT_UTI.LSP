; Text utilities by Trong Le NT- May 2, 2002
(prompt "\n\nL O A D I N G    T E X T _ U T I . L S P")

;------------------------------------------------------------------
(defun UKWORD (bit kwd msg def / inp)
     (if (and def (/= def ""))
          (setq msg (strcat "\n" msg "<" def ">: ")
               bit (* 2 (fix (/ bit 2)))
          )
          (if (= " " (substr msg (strlen msg) 1))
               (setq msg (strcat "\n" (substr msg 1 (1- (strlen msg))) ": "))
               (setq msg (strcat "\n" msg ": "))
          )
     )
     (initget bit kwd)
     (setq inp (getkword msg))
     (if inp inp def)
)
;------------------------------------------------------------------
(defun USTR (bit msg def spflag / inp nval)
     (if (and def (/= def ""))
          (setq msg (strcat "\n" msg "<" def ">: ")
               inp (getstring spflag msg)
               inp (if (= inp "") def inp)
          )
          (progn
               (if (= " " (substr msg (strlen msg) 1))
                    (setq msg (strcat "\n" (substr msg 1 (1- (strlen msg))) ": "))
                    (setq msg (strcat "\n" msg ": "))
               )
               (if (= bit 1)
                    (while (= "" (setq inp (getstring spflag msg)))
                         (prompt "\nInvalid String.")
                    )
                    (setq inp (getstring spflag msg))
               )
          )
     )
     inp
)
;------------------------------------------------------------------
(defun ETOS (arg / file)
     (if (= 'STR (type arg)) (setq arg (strcat "\"" arg "\"")))
     (setq file (open "TL" "w"))
     (princ arg file)
     (close file)
     (setq file (open "TL" "r"))
     (setq arg (read-line file))
     (close file)
     (close (open "TL" "w"))
     arg
)  
;------------------------------------------------------------------
(defun DXF (code elist)
     (cdr (assoc code elist))
)
;-------------------------------------------------------------
(defun Text_get ( / ssl  nsset temp ed )
     (setq #sset (ssget))
     (setq ssl (sslength #sset) 
           nsset (ssadd)
     )
     (print ssl)
     (princ "entities found. ")  
     (princ "\nVerifying the selected entities -- please wait. ")
     (while (> ssl 0)
          (progn
               (setq temp (ssname #sset (setq ssl (1- ssl))))
               (setq ed (entget temp))
               (if (= (DXF 0 ed) "TEXT") (ssadd temp nsset))
          )
     )
     (setq ssl (sslength nsset)
          #sset nsset
     )
     (print ssl)
     (princ "TEXT entities found. ")
     (princ)
);defun Text_get
;-------------------------------------------------------------
(prompt "\n		ChCase: Change case")
(defun c:chcase ( / code ssl)
     (Text_get)
     (setq code (getint "\nCode <BK1=2/VNI=3>: "))
     (setq ssl (sslength #sset))
     
     (while (> ssl 0)
          (progn
               (setq temp (ssname #sset (setq ssl (1- ssl)))
                    ed (entget temp)
                    old (cons 1 (DXF 1 ed))
                    #txt (DXF 1 ed)
               )
               ( ChangeCase Code)
               (setq new (cons 1 #txt)
                    ed (subst new old ed)
               )
          )
          (entmod ed)  
     )
     (princ)
);defun
;-------------------------------------------------------------------

(defun ChangeCase ( code )
     (setq txt1 "")
     (setq len (strlen #txt))
     (setq i 1)
     (if (= Code 3);   VNI
          (progn
               (repeat len
                    (setq char (substr #txt i 1))
                    (if (OR (AND  (> (ascii char) 96) (< (ascii char) 123)) (AND (> (ascii char) 223) (< (ascii char) 253)) ) 
                                    (setq char (chr (- (ascii char) 32)))
                    )
                    (setq txt1 (strcat txt1 char))
                    (setq i (+ i 1))
              );repeat
         );progn
     );if
     (if (= Code 2);   BK1
          (progn
               (repeat len
                    (setq char (substr #txt i 1))
                    ( cond
                         ((AND  (> (ascii char) 96) (< (ascii char) 123)) (setq char (chr (- (ascii char) 32))))
                         ((= (ascii char) 222) (setq char (chr 126)))
                         ((= (ascii char) 189) (setq char (chr 125)))
                         ((= (ascii char) 251) (setq char (chr 123)))
                         ((= (ascii char) 252) (setq char (chr 94)))
                         ((= (ascii char) 253) (setq char (chr 96)))
                         ((= (ascii char) 254) (setq char (chr 124)))
                         ((= (ascii char) 255) (setq char (chr 127)))
                         ((> (ascii char) 188) (setq char (chr (- (ascii char) 62))))
                    );cond
                    (setq txt1 (strcat txt1 char))
                    (setq i (+ i 1))
              );repeat
         );progn
     );if
     (setq #txt txt1) 
);defun
;-------------------------------------------------------------------
(load "ch_code.lsp")
;-------------------------------------------------------------------
(prompt "\n		ATEXT: Add something in front of text ")
(defun c:atxt ( / ssl ed No new temp old )
  (Text_get)
  (setq str (getstring "\nAdd what: "))
  (setq ssl (sslength #sset))

  (while (> ssl 0)
    (progn
      (setq temp (ssname #sset (setq ssl (1- ssl))))
      (setq ed (entget temp))
      (setq old (cons 1 (DXF 1 ed)))
      (setq No (DXF 1 ed))
      (setq No (strcat str No))
      (setq new (cons 1 No))
      (setq ed (subst new old ed))
      (entmod ed)  
    )
  )
)
;-----------------------------------------------------------

(prompt "\n		CHNO to +-*/ text entities with a number")
(defun c:CHNO ( / No temp ed old new acc fac operator ssl )
  (Text_get)
  (setq operator (getstring "\nInput operator (+ - * /):"))
  (setq fac (getreal "\nInput a number:"))
  (setq acc (getint "\nAccuracy:"))
  (setq DZ (getvar "DIMZIN"))
  (setvar "DIMZIN" 0)
  (setq ssl (sslength #sset))

  (while (> ssl 0)
    (progn
      (setq temp (ssname #sset (setq ssl (1- ssl))))
      (setq ed (entget temp))
      (setq old (cons 1 (DXF 1 ed)))

      (setq No (atof (DXF 1 ed)))
      (cond
         ((= operator "+") (setq No (+ No fac)))
         ((= operator "-") (setq No (- No fac)))
         ((= operator "*") (setq No (* No fac)))
         ((= operator "/") (setq No (/ No fac)))
      )
      (setq new (cons 1 (rtos No 2 acc)))
     
      (setq ed (subst new old ed))
      (entmod ed)  
    )
  )
(setvar "DIMZIN" DZ)
)
;-----------------------------------------------------------

(defun Cir_Text_get ( / ssl temp ed )
     (setq #sset (ssget))
     (setq ssl (sslength #sset) 
           #t_sset (ssadd)
           #c_sset (ssadd)
     )
     (print ssl)
     (princ "entities found. ")  
     (princ "\nVerifying the selected entities -- please wait. ")
     (while (> ssl 0)
          (progn
               (setq temp (ssname #sset (setq ssl (1- ssl))))
               (setq ed (entget temp))
               (if (= (DXF 0 ed) "TEXT") (ssadd temp #t_sset))
               (if (= (DXF 0 ed) "CIRCLE") (ssadd temp #c_sset))
          )
     )
     (setq t_ssl (sslength #t_sset)
           c_ssl (sslength #c_sset)
     )
     (print t_ssl)
     (princ "TEXT entities found. ")
     (print c_ssl)
     (princ "CIRTCLE entities found. ")
     (princ)
);defun Cir_Text_get
;-------------------------------------------------------------

(prompt "\n		CE : ")
(DEFUN C:CE ( / t_ssl c_ssl )
	(Cir_Text_get)
	(setq t_ssl (sslength #t_sset)
              c_ssl (sslength #c_sset)
	      ssl t_ssl	
	)

	(while (> c_ssl 0)
    	(progn
      		(setq temp (ssname #c_sset (setq c_ssl (1- c_ssl))))
      		(setq ed (entget temp))
		(setq cen (DXF 10 ed))
	  	(setq radius (DXF 40 ed))

		(setq ssl t_ssl)
	  
		(while (> ssl 0)
    		(progn
      			(setq t_temp (ssname #t_sset (setq ssl (1- ssl))))
      			(setq ed1 (entget t_temp))
			(setq in_point (DXF 10 ed1))
		  	(setq dist (distance in_point cen))
		  	(if (< dist radius)
			(progn
				(setq 	old (cons 10 (DXF 10 ed1))
        	              		new (cons 10 cen)
	                      		ed1 (subst new old ed1)
					old (cons 72 (DXF 72 ed1))
        	              		new (cons 72 4)
	                      		ed1 (subst new old ed1)

					old (cons 11 (DXF 11 ed1))
        	              		new (cons 11 cen)
	                      		ed1 (subst new old ed1)
				)
	        		(entmod ed1)
			  	(ssdel t_temp #t_sset)
			  	(setq t_ssl (- t_ssl 1))
			);progn
			);if
		); progn
  		);while
	);progn
	);while
  ) ;defun

;-------------------------------------------------------------------

(prompt "\n		MJ: Middle justify Text")
(defun c:MJ ( / code ssl)
     (Text_get)
     (setq pt (getpoint "\nInput Middle point"))
     (setq ssl (sslength #sset))
     
     (while (> ssl 0)
         (progn
               (setq temp (ssname #sset (setq ssl (1- ssl)))
                    ed (entget temp)

		    old (cons 72 (DXF 72 ed))
		    new (cons 72 1)
		    ed (subst new old ed)

		    old (cons 73 (DXF 73 ed))
		    new (cons 73 1)
		    ed (subst new old ed)


		    old (cons 10 (DXF 10 ed))
		    new (cons 10 (list (car pt) (cadr (dxf 10 ed)) 0.0)) 
		    ed (subst new old ed)

		    old (cons 11 (DXF 11 ed))
		    new (cons 11 (DXF 10 ed))
		    ed (subst new old ed)
		)
          (entmod ed)  
          )

     )
     (princ)
);defun
;-------------------------------------------------------------------

(defun Select_Sort( / ssl temp_name temp_y y_max name_max )

     (Text_get)

     (setq  Newsset (ssadd))
     (setq ssl (sslength #sset))

     (while (> ssl 0)
       (progn
         (setq ssl (sslength #sset))
         (setq temp_name (ssname #sset 0))
         (setq temp_y (cadr (dxf 10 (entget temp_name))))
         (setq y_max temp_y)
         (setq name_max temp_name)
         (prompt "\n") (princ ssl)
  
         (while (> ssl 1)
           (progn
              (setq temp_name (ssname #sset (setq ssl (1- ssl))))
	      (setq temp_y (cadr (dxf 10 (entget temp_name))))
	      (if (< temp_y y_max)
                (progn
                   (setq y_max temp_y)
	           (setq name_max temp_name)
	        )
	      );if
           );progn
         );while

         (ssadd name_max NewSset)

         (ssdel name_max #sset)
         (setq ssl (sslength #sset))
       
      );progn 
    ); while #sset
);defun Select_Sort

;---------------------
(prompt "\n		CTT: Lay SL cat ngang - TEXT")
(defun c:CTT ( / )

  (setq fn (getfiled "Input data file" "" "txt" 1))
  (setq f (open fn "w"))

  (setq TnName (car (entsel "\n\nPick CS Name")))
    
  (while TnName
     (progn
        (Select_Sort)
        (setq line (dxf 1 (entget TnName)))
	(setq ssln (sslength NewSset))
        (while (> ssln 0)
 	   (progn
              (setq temp (ssname Newsset (setq ssln (1- ssln))))
              (setq ed (entget temp))
              (setq value (dxf 1 ed))
	      (setq line (strcat line "\t" value))
	   );progn
        );while
	(write-line line f)
        (princ line)	  
	(setq TNname (car (entsel "\nPick SC Name")))     
    )
  );while TnName

  (close f) 
  
);defun
;-------------------------------------------------------------------



