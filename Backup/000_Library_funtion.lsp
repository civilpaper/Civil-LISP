(defun DXF (code elist)
  (cdr (assoc code elist))
)
;;;;;;;;;;;;;;;;;;;;;
(defun sort_num	(LLLL / LLLL nLLL sort_order flag1)
  ;;given a list of numbers
  ;;sorts and returns the list
  ;;in decending order

  (repeat (length LLLL)
    (setq sort_order
	   (append sort_order
		   (list
		     (apply 'min LLLL)
		     ;;get largest number in list
		     ;;append to new list
		   )
	   )
    )
    (setq flag1 nil)
    (foreach x LLLL

      (if (or (not (= x (apply 'min LLLL)))
	      ;;delete min from LLLL
	      flag1
	  )

	(setq nlll (append nlll (list x)))
	;;will return miltiule
	(setq flag1 1)
	;;occurances of a number

      )					;if

    )					;foreach

    (setq llll nlll
	  nlll nil
    )
  )					;repeat
  (setq sort_order sort_order)
  ;;return sorted list
)
(princ)
					;=============================

(vl-load-com)
