(defun c:renameb (/ btable badt)
  (setq btable '(("OLDNAME" . "NEWNAME")
                 ("LEG6"    . "LEG4")))

  (foreach b btable
    (cond ((and (not (tblsearch "BLOCK" (cdr b)))
                (tblsearch "BLOCK" (car b)))
           (command "_.RENAME" "_Block" (car b) (cdr b)))
          ((tblsearch "BLOCK" (cdr b))
           (setq badt (cons b badt)))))

  (and badt
      (prin1 badt)
      (alert "Unable To Rename All BLOCKs"))

  (prin1))