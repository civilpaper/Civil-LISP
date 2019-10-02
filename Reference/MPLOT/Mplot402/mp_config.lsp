
	(defun btn1()
	  (setq dd1 "C:/Documents and Settings/")
	  (setq ddc_may "/Application Data/Autodesk/AutoCAD 2007/R17.0/enu/Plotters/")
	  (setq dd2 (getvar "LOGINNAME"))
	  (setq ddc_file "/Application Data/Autodesk/AutoCAD 2007/R17.0/enu/Plot Styles/")
	  (setq dd_may (strcat dd1 dd2 ddc_may))
	  (setq dd_file (strcat dd1 dd2 ddc_file))
	  
	  (setq mayin_dd (getfiled "Select the Plotter..." dd_may "pc3" 8))
	  (setq may_len (strlen mayin_dd))
	  (setq i may_len)
	  (while (> i 0)
	    (progn
	      (setq mayin_con (substr mayin_dd (+ i 1)))
	      (setq do (substr mayin_dd i 1))
	      (if (= do "\\") (setq i 1))
	      (setq i (- i 1))

	    )
	  )
	  (set_tile "edit_1" mayin_con)


	)



	(defun btn2()
	  (setq dd1 "C:/Documents and Settings/")
	  (setq ddc_may "/Application Data/Autodesk/AutoCAD 2007/R17.0/enu/Plotters/")
	  (setq dd2 (getvar "LOGINNAME"))
	  (setq ddc_file "/Application Data/Autodesk/AutoCAD 2007/R17.0/enu/Plot Styles/")
	  (setq dd_may (strcat dd1 dd2 ddc_may))
	  (setq dd_file (strcat dd1 dd2 ddc_file))
	  (setq filein_dd (getfiled "Select the Plot Style Table..." dd_file "ctb" 8))
	  (setq filein_len (strlen filein_dd))
	  (setq j filein_len)
	  (while (> j 0)
	    (progn
	      (setq filein_con (substr filein_dd (+ j 1)))
	      (setq do1 (substr filein_dd j 1))
	      (if (= do1 "\\") (setq j 1))
	      (setq j (- j 1))

	    )
	  )
	  (set_tile "edit_2" filein_con)
	  
	)
	;===================================================================
	(defun btn_ok()
	  

	  (done_dialog)
	  

	)
	;===================================================================


	(defun c:chi()
	  (setq DCL_ID (load_dialog "chi.dcl"))
	  (if (not (new_dialog "Chin" DCL_ID)) (exit))
	 

	  (action_tile "btn_1" "(btn1)")
	  
	  (action_tile "btn_2" "(btn2)")
	  (action_tile "accept" "(btn_ok)")

	  
	  (start_dialog)
	  (unload_dialog DCL_ID)
	  (setq tmode (getvar "TILEMODE"))
	  (If (= tmode 1) (Setvar "TILEMODE" 0))
	  (command "-plot" "y" "" mayin_con "A3" "m" "l" "n" "w" (list 0 0) (list 408 285) "1:1" "" "y" filein_con "y" "n" "n" "n" "n" "y" "n")

	  (command ".zoom" "e")
	  (princ)


	)