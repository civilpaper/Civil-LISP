;*******************************************************************************
;*                             View tab                                        *
;*******************************************************************************

;;;;;;;;;;; This command Tile Vertical Multiple Dwgs ;;;;;;;;;;;
(defun c:TV nil
  (vl-load-com)
  (vlax-for x (vla-get-documents (vlax-get-acad-object))
    (vla-put-windowstate x acNorm)
  )
  (command "syswindows" "vert")
  (princ)
)