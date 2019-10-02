(defun c:model2layout (/ i aDoc LayoutColl MoveToLayout ss ObjToMove)
;;; pBeFeb2014 ;;;
;;;  Transferring objects from model space to paper space 
(vl-load-com)
(setq i -1
adoc (vla-get-activedocument (vlax-get-acad-object))
LayoutColl (vla-get-layouts adoc)
)
(if (and (setq MoveToLayout nil
ss (ssget "_:L")
)
(not (textscr))
(foreach itm (layoutlist)
(princ (strcat "\nInput "
(itoa (setq i (1+ i)))
" for layout "
itm
)
)
)
(setq lnum (getint "\nEnter Index value: "))
(setq NLNAme (nth lnum (layoutlist)))
)
(progn
(vlax-for x (setq ObjToMove (vla-get-activeselectionset aDoc))
(setq MoveToLayout (cons x MoveToLayout))
)
(vla-delete ObjToMove)
(vla-CopyObjects
ADoc
(vlax-make-variant
(vlax-safearray-fill
(vlax-make-safearray
vlax-vbObject
(cons 0 (1- (length MoveToLayout)))
)
MoveToLayout
)
)
(vla-get-block (vla-item LayoutColl NLNAme))
)
(foreach O MoveToLayout (vla-delete O))
)
)
(princ)
)(vl-load-com)