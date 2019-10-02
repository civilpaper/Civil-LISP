(setvar "MODEMACRO" "Multiplot V4.02 -:- Cs_Group -:- Nk_long@yahoo.com -:- 0989 50 52 55")

;Ham DXF
(defun DXF (code elist)
  (cdr (assoc code elist))
);dxf

;=============================


(defun c:in(/ ss ssl i)

  (setq tmode (getvar "TILEMODE"))
  (If (= tmode 1) (Setvar "TILEMODE" 0))
  (setq ss (ssget "x"))
  (setq xrefss (ssadd))
  (setq ssl (sslength ss))
  

  (setq i 0)
  (while (< i ssl)
    (progn
      (setq ssi (ssname ss i))
      (setq list_ssi (entget ssi))
      (setq loai (dxf 0 (entget ssi)))
      (if (= loai "INSERT")
	(progn
	  (setq name (dxf 2 (entget ssi)))
	  (if (= name "xref") (setq xrefss (ssadd ssi xrefss)))
	)
      )
    )
    (setq i (+ i 1))
  )

  (setq xlist '())
  (setq ssl (sslength xrefss))
  (setq i 0)
  (while (< i ssl)
    (progn
      (setq ssx (ssname xrefss i))
      (setq ptx (dxf 10 (entget ssx)))
      (setq xlist (append xlist (list (car (dxf 10 (entget ssx))))))
    )
    (setq i (+ i 1))
  )

  (setq xlist (sort_num xlist))
  (setq xss (ssadd))
  (while (/= xlist nil)
    (setq x (car xlist))
    (setq i 0)
    (while (< i ssl)
      (progn
	(setq ssx (ssname xrefss i))
	(setq ptx (dxf 10 (entget ssx)))
	(if (= (car ptx) x) (setq xss (ssadd ssx xss)))
	
      )
      (setq i (+ i 1))
    )
    (setq xlist (cdr xlist))    
  )

  (setq i 0)
  (setq ssl (sslength xss))
  (while (< i ssl)
    (progn
      (setq d1 (DXF 10 (entget (ssname xss i))))
      (setq d1x (car d1))
      (setq d1y (cadr d1))
      (setq d2x (+ d1x 408))
      (setq d2y (+ d1y 285))
      (setq d2 (list d2x d2y))
      (command "-plot" "y" "" "" "A3" "m" "l" "n" "w" d1 d2 "" "" "y" "" "y" "n" "n" "n" "n" "y" "y")
      (command "delay" 1000)
    )
    (setq i (+ i 1))
  )

  (command ".zoom" "e")
  (command ".qsave")
  ;(alert (strcat (rtos ssl 2 0) " drawings already printed out"))


  
)
;=====================================

(defun sort_num (LLLL / LLLL nLLL sort_order flag1 )	;;given a list of numbers
							;;sorts and returns the list
							;;in decending order

(repeat (length LLLL)			
	  (setq sort_order 
		(append sort_order (list 
					(apply 'min LLLL)	;;get largest number in list
								;;append to new list
	  )     )                  )
          (setq flag1 nil)
 (foreach x LLLL 						
  
	    (if (or  	(not (= x (apply 'min LLLL)))		;;delete min from LLLL
		    flag1					
		 )					
						
		(setq nlll (append nlll (list x)) )				
								;;will return miltiule
		(setq flag1 1)					;;occurances of a number

           );if

  );foreach

	(setq llll nlll
      	      nlll nil )
);repeat
(setq sort_order sort_order)	;;return sorted list
)(princ)