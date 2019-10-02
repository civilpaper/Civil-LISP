
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
;=============================
;Ham lay chieu dai cua mot doi tuong
(defun addlen(eset / en enlist entype len tong_len cntr)
  (setvar "cmdecho" 0)
  (defun getArc(en)
    (command "lengthen" en "")
    (getvar "perimeter")
  )
  (defun getLine(en)
    (setq enlist(entget en))
    (distance (cdr(assoc 10 enlist)) (cdr(assoc 11 enlist)))
  )
  (defun getPoly(en)
    (command "area" "Object" en)
    (getvar "perimeter")
  )  
      (setq tong_len 0)
      (setq cntr 0)
      (while (< cntr (sslength eset))
        (setq en (ssname eset cntr))
        (setq enlist (entget en))
        (setq enType (cdr (assoc 0 enlist)))
        (cond
          ((= enType "ARC"       )(setq len(getArc en)))
          ((= enType "CIRCLE"    )(setq len(getPoly en)))
          ((= enType "ELLIPSE"   )(setq len(getPoly en)))
          ((= enType "LINE"      )(setq len(getLine en)))
          ((= enType "LWPOLYLINE")(setq len(getPoly en)))
          ((= enType "POLYLINE"  )(setq len(getPoly en)))
          ((= enType "SPLINE"    )(setq len(getPoly en)))
          (T (setq len 0.0))
        )
        (while (< (strlen enType) 12) (setq enType (strcat enType " ")))
        (setq tong_len (+ tong_len len))
        (setq cntr (+ cntr 1))
      )
  (setvar "cmdecho" 1)
  (setq totallen tong_len)
  (princ)
)
;==========================
;xuat file 
(defun c:len2file(/ ss l i f tf)
  (setq ss (ssget))
  (setq tf (getstring "\nFile name:"))
  (setq tf (strcat (getvar "DWGPREFIX") tf ".txt"))
  (setq dlist '())
  (setq l (sslength ss))
  (setq i 0)
  (while (< i l)
    (setq ssi (ssadd (ssname ss i)))
    (addlen ssi)
    (setq dai totallen)
    (setq dlist (append dlist (list dai)))
    (setq i (+ 1 i))

  )

  (setq dlist (sort_num dlist))

  
  
  (setq f (open tf "a")) 
  
  (while (/= dlist nil)
    (setq daix (car dlist))
    (write-line (rtos daix 2 3) f)
    (setq dlist (cdr dlist))

  )
  (close f)
)

(defun c:l2f() (c:len2file))