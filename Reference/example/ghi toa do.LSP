
(Defun C:GTD ()
      (prompt " GTD : ghi toa do mot diem so voi he truc tuong doi")
      (SETQ PT (GETPOINT "\nPick (0,0) point"))
      (COMMAND "ucs" "O" PT)
  )