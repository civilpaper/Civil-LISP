(defun c:TV nil;; This command Tile Vertical Multiple Dwgs
  (vl-load-com)
  (vlax-for x (vla-get-documents (vlax-get-acad-object))
    (vla-put-windowstate x acNorm)
  )
  (command "syswindows" "vert")
  (princ)
)