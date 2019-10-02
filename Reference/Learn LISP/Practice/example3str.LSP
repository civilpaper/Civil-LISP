
(defun C:NameMix ()			; Dat ten chuong trinh la NameMix khong co bien so
  (setq strName (getstring T "\n Enter your name: "))
					; Gan bien co ten la strName la ket qua tra ve cua ham getstring voi tham so T
					; va chuoi hien thi la Enter your name
  (setq keepGoing 1)
  (while (<= keepGoing (strlen strName)))
  (setq char1 (substr strName 1 keepGoing))
					; gan bien char1 la ket qua tra ve cua ham substr
					;voi bien la strName thong so bat dau la 1 va bien keepGoing
  (setq char1 (strcase char1)); Dat lai bien char1 thanh chuoi voi cac ki tu in hoa
  (setq strName2 (strcat char (substr strName (+ keepGoing 1))))
  (setq keepGoing (+ keepGoing 1) )
  
)