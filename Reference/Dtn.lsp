(defun C:dtn(/ ena dis)
	(command "insert" "C:\\DTN.dwg" "0,0,0" "2" "" "")
	(command "erase" (entlast) "")
	(while (setq ena (car (entsel "\nSelect polyline")))
		(setq dis (getdist "\nKhoang cach 2 ky hieu"))
		(if (not dis) (setq dis 10.00))
		(command "measure" ena "B" "DTN" "" dis)
	)
)