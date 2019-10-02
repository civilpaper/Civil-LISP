;;; PLOAD.LSP ver 1.0

;;;================================================================= 
(defun C:PLOAD (/ StarNum EndNum StrLoad Loop)


       (command "layer" "N" "3" "C" "green" "3" "")
       (command "layer" "N" "5" "C" "blue" "5" "")
       (command "layer" "N" "1" "C" "red" "1" "")
       (command "layer" "N" "7" "C" "white" "7" "")
       (command "layer" "N" "6" "C" "magenta" "6" "")
       (setvar "osmode" 0)

       (setq StarNum (getint "\nEnter Star Station for Load : "))
       (setq EndNum (getint "Enter End Station for Load : "))
       (setq  Loop StarNum)   
       (While 
	      (<= Loop EndNum)
              (progn
		(setq StrNum (itoa Loop))
                (setq StrLoad (strcat StrNum ".LSP"))
                (setvar "BLIPMODE" 0)
                (setvar "CMDECHO" 0)
                (princ (strcat "\nLoading "StrLoad" ..."))
                (LOAD StrLoad)
                (princ (strcat "\nPLOAD: "StrLoad" loaded"))
              )  
	      ( setq Loop (1+ Loop))
        )  	
   (princ "\n FINISHED !")  
)	

