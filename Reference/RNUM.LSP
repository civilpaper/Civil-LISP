;;Have you ever whated to sort text items by there point values in Acad?  This lisp renumbers
;;text objects, it works well if you have some general notes, say 1 throught 15, on your drawing
;;and you want to add a new note between 5 and 6.  With this lisp you select all of the numbers
;;and it will reorder them starting wtih 1.


(DEFUN C:rnum (/ mtxt SS NUM cnt ent1 ss1)	;;main function


;;=========================================================

(defun sort_num (LLLL / LLLL nLLL sort_order flag1 )	;;given a list of numbers
							;;sorts and returns the list
							;;in decending order

(repeat (length LLLL)			
	  (setq sort_order 
		(append sort_order (list 
					(apply 'max LLLL)	;;get largest number in list
								;;append to new list
	  )     )                  )
          (setq flag1 nil)
 (foreach x LLLL 						
  
	    (if (or  	(not (= x (apply 'max LLLL)))		;;delete max from LLLL
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
;;=========================================================


(defun tsort (ss1 / elist1 txt_sort txt_sort1 cnt txt_sort2 num ss) 	;;function to re-order 
									;;selection set





(setq cnt 0)

(repeat (sslength ss1)

 (setq elist1 (append elist1 (list 							;;make assocation list
				(cons 							;;and dotted pair from
				     (caddr (assoc 10 (entget (ssname ss1 cnt)))) 	;;y-coord and entity name
				     (cdr (assoc -1 (entget (ssname ss1 cnt))))
				)
			      )
		)
 )
 (setq cnt (1+ cnt))

);repeat

(foreach x elist1								;;make list of 
		  (setq txt_sort (append txt_sort (list (car x))))		;;y-coords
)
(setq txt_sort1 (sort_num txt_sort))						;;re-order list						
(foreach x txt_sort1
		  (setq txt_sort2 (append txt_sort2 (list (cons 
						
			(cdr (assoc x elist1))
			x				  )			;;create new
						   )				;;assocation list
				  )
		  )
)
(setq ss nil)
(setq num 0)
(setq ss (ssadd (car (nth num txt_sort2)) ) )  					;;create new 
										;;selection set	
(repeat (- (length txt_sort2) 1)
  (setq num (1+ num))
  (setq ss (ssadd (car (nth num txt_sort2)) ss) ) 
)(princ)
(setq ss ss)
)						
;;==========================================================================================
		;;Primary Function
(setq ss1 nil)
(while (= ss1 nil)
(terpri)(princ "Pick Text")
(SETQ SS1 (SSGET '((0 . "TEXT"))) )
)
(setq ss (tsort ss1))
(setq 	num 0
	cnt 1)	
  
(repeat (sslength ss)
	(setq ent1 (entget (ssname ss num)))
	(setq ent1 (subst (cons 1 (strcat (rtos cnt 2 0) " ")) (assoc 1 ent1) ent1) )
	
	 (SETQ     num (1+ num)
	      cnt (1+ cnt)
	)
	(entmod ent1)
	
)		

;;==========================================================================================
		;Notify User

(TERPRI)(print num) (PRINC "....Entities Updated")(princ)
);defun
(TERPRI)(princ "Type  RNUM  to renumber notes ") (PRINC)   