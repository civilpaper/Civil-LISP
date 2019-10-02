					;Funtion: offset multiobjects 
					;ver 7-3-2008
(defun c:offsetall ()
  (setq cs_offdis 0)
  (setq offf cs_offdis)
  (setq	cs_offdis (getreal (strcat "\nSpecify multioffset distance "
				   "<"
				   (rtos cs_offdis)
				   ">:"
			   )
		  )
  )
  (if (= cs_offdis nil)
    (setq cs_offdis offf)
  )
  (prompt "Select object to offset: ")
  (setq of (ssget))
  (setq pt (getpoint "\nSpecify point on side to offset: "))
  (setq i 0)
  (while (< i (sslength of))
    (setq ofi (ssname of i))
    (command ".offset" cs_offdis ofi pt ^c^c)

    (setq i (+ i 1))
  )


)
(defun c:oa ()
  (c:offsetall)
)