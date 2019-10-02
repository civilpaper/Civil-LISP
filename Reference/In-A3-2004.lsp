(defun c:in4()   
   (setq pt1 (getpoint "\nDiem goc trai khung in : "))
   (setq pt2 (getcorner pt1 "\nDiem goc phai khung in : "))
   (setq D (getdist "\nKhoang cach cac khung in : "))
   (setq n (getint "\nSo khung in : "))
   (setq i 0)
    (while (< i n)   
      (setq pt3 (polar pt1 0 (* i D)))
      (setq pt4 (polar pt2 0 (* i D)))
      ;;(command "plot" "y" "" "A3THANH" "A4 (210 x 297 mm)" "m" "l" "" "w" pt3 pt4 "F" "C" "" "TD_NV.ctb" "" "" "" "" "" "" "")
      (command "plot" "y" "" "mayinphong2.pc3" "ISO A4 (210.00 x 297.00 MM)" "m" "l" "" "w" pt3 pt4 "F" "C" "" "TD_NV.ctb" "" "" "" "" "" "" "")
      (setq i (+ i 1))
    );while
   (princ)
);defun
;-----------------------------------
(defun c:in3()   
   (setq pt1 (getpoint "\nDiem goc trai khung in : "))
   (setq pt2 (getcorner pt1 "\nDiem goc phai khung in : "))
   (setq D (getdist "\nKhoang cach cac khung in : "))
   (setq n (getint "\nSo khung in : "))
   (setq i 0)
    (while (< i n)   
      (setq pt3 (polar pt1 0 (* i D)))
      (setq pt4 (polar pt2 0 (* i D)))
      (command "plot" "y" "" "" "" "m" "l" "" "w" pt3 pt4 "1:1"  "" "" "" "" "" "" "" "" "" "")
      (command "plot" "y" "" "mayinphong2" "A3" "m" "l" "" "w" pt3 pt4 "1:1" "c" "" "TD_NV.ctb" "" "" "" "" "" "" "")
      (setq i (+ i 1))
    );while
   (princ)
);defun
;-----------------------------------
