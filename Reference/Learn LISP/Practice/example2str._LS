(defun myProg ()			; Ten chuong trinh la myProg ko co bien so
  (setq str1 "Jeffery")			; Dat bien str1 la chuoi Jeffery
  (setq str2 "P")			; Dat bien str2 la chuoi P
  (setq str4 (substr str1 1 4))		; Dat bien str4 la chuoi gom 4 ki tu dau cua bien str1
  (princ str4)				; Hien thi bien str4 ra dong lenh cua Autocad
  (setq str4 (strcase str4))		; Dat bien int1 thanh chuoi cac ki tu in hoa
  (setq int1 (strlen str4))
  (princ int1)
  (setq str5 (strcat str4 "" str2))
  (princ str5)
  (princ )
)