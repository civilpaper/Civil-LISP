(DEFUN c:gt ()
	(SETVAR "CMDECHO" 0)
  	(Setq CC (Getpoint "\n Chon diem can ghi:")
	      G (Getstring "\n Nhap Ten can ghi :")
	)
  	(Command ".circle" CC "d" 3
		 ".text" "S" 3 "MC" CC 0 G
	)
  )