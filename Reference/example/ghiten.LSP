(DEFUN c:gt ()
	(SETVAR "CMDECHO" 0)
  	(Setq CC (Getpoint "\n Chon diem can ghi:")
	      G (Getstring "\n Nhap Ten can ghi :")
	)
  	(setvar "textstyle" "3")
  	(Command ".circle" CC "d" 3
		 ".text" "MC" CC 0 G "" 
	)
  )