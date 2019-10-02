;=============================================== 
;    UnAnon.Lsp                                   Jul 05, 1998 
;====================================== 
(princ "\nCopyright (C) 1998, Fabricated Designs, Inc.") 
(princ "\nLoading UnAnon v1.0 ") 
(setq uan_ nil lsp_file "UnAnon") 

;================== For Automated Calling From Another Program ========= 
(defun uan_auto (ar1) (UnAnon ar1)) 

;================== Macros ============================================= 
(defun PDot ()(princ ".")) 

(PDot);++++++++++++ Set Modes & Error ++++++++++++++++++++++++++++++++++ 
(defun uan_smd () 
 (SetUndo) 
 (setq olderr *error* 
      *error* (lambda (e) 
                (and (/= e "quit / exit abort") 
                     (princ (strcat "\nError: *** " e " *** "))) 
                (command "_.UNDO" "_END" "_.U") 
                (uan_rmd)) 
       uan_var '( 
  ("CMDECHO"   . 0) ("MENUECHO" . 0) ("MENUCTL"   . 0) ("MACROTRACE" . 0) 
  ("OSMODE"    . 0) ("SORTENTS" . 119)("MODEMACRO" . ".") 
  ("BLIPMODE"  . 0) ("EXPERT"   . 0) ("SNAPMODE"  . 1) ("PLINEWID"   . 0.0) 
  ("ORTHOMODE" . 1) ("GRIDMODE" . 0) ("ELEVATION" . 0) ("THICKNESS"  . 0) 
  ("FILEDIA"   . 0) ("FILLMODE" . 0) ("SPLFRAME"  . 0) ("UNITMODE"   . 0) 
  ("TEXTEVAL"  . 0) ("ATTDIA"   . 0) ("AFLAGS"    . 0) ("ATTREQ"     . 1) 
  ("ATTMODE"   . 1) ("UCSICON"  . 1) ("HIGHLIGHT" . 1) ("REGENMODE"  . 1) 
  ("COORDS"    . 2) ("DRAGMODE" . 2) ("DIMZIN"    . 1) ("PDMODE"     . 0) 
  ("CECOLOR"   . "BYLAYER") ("CELTYPE" . "BYLAYER"))) 
 (foreach v uan_var 
      (setq m_v (cons (getvar (car v)) m_v) 
            m_n (cons (car v) m_n)) 
      (setvar (car v) (cdr v))) 
 (princ (strcat (getvar "PLATFORM") " Release " (substr (ver) 18 2) 
   " -  Convert To Anonymous Blocks ....\n")) 
 (princ)) 

(PDot);++++++++++++ Return Modes & Error +++++++++++++++++++++++++++++++ 
(defun uan_rmd () 
  (setq *error* olderr) 
  (mapcar 'setvar m_n m_v) 
  (command "_.UNDO" "_END") 
  (prin1)) 

(PDot);++++++++++++ Set And Start An Undo Group ++++++++++++++++++++++++ 
(defun SetUndo () 
 (and (zerop (getvar "UNDOCTL")) 
      (command "_.UNDO" "_ALL")) 
 (and (= (logand (getvar "UNDOCTL") 2) 2) 
      (command "_.UNDO" "_CONTROL" "_ALL")) 
 (and (= (logand (getvar "UNDOCTL") 8) 8) 
      (command "_.UNDO" "_END")) 
 (command "_.UNDO" "_GROUP")) 

(PDot);++++++++++++ Get Entity Name ++++++++++++++++++++++++++++++++++++ 
(defun GetOne (/ st os) 
 (setq os (getvar "SNAPMODE") s nil) 
 (setvar "SNAPMODE" 0) 
 (while (not st) 
        (setq st (ssget))) 
 (while (> (sslength st) 1) 
        (setq st nil) 
        (princ "\nOnly 1 At A Time Please\n") 
        (while (not st) 
               (setq st (ssget)))) 
 (setvar "SNAPMODE" os) 
 (setq s (ssname st 0))) 

(PDot);++++++++++++ Convert An Anonymous Block To Named Block ++++++++++ 
(defun UnAnon (b / tdef en ed bc bn bd in)          ;Supply ename 
  (setq bn "TEMP1" bc 1) 
  (while (tblsearch "BLOCK" bn) 
         (setq bc (1+ bc) bn (strcat "TEMP" (itoa bc)))) 
  (and (= (type b) 'ENAME) 
       (setq bd (entget b) 
             in (cdr (assoc 2 bd)))) 
  (if (or (not bd) 
          (not in) 
          (/= "INSERT" (cdr (assoc 0 bd))) 
          (/= "*U" (substr in 1 2)) 
          (= (logand (cdr (assoc 70 (tblsearch "BLOCK" in)))  4)  4) 
          (= (logand (cdr (assoc 70 (tblsearch "BLOCK" in))) 16) 16) 
          (= (logand (cdr (assoc 70 (tblsearch "BLOCK" in))) 32) 32)) 
       (progn 
         (princ "*** Not An Anonomymous Block *** ") 
         (setq bn nil bc nil bd nil in nil b nil) 
         (exit))) 
  (setq tdef (tblsearch "BLOCK" in) 
          en (cdr (assoc -2 tdef)) 
          ed (entget en)) 
  (entmake (list (cons 0 "BLOCK") 
                 (cons 2 bn) 
                 (cons 70 0) 
                 (cons 10 (cdr (assoc 10 tdef))))) 
  (entmake ed) 
  (while (setq en (entnext en)) 
         (setq ed (entget en)) 
         (entmake ed)) 
  (entmake (list (cons 0 "ENDBLK"))) 
  (setq bd (subst (cons 2 bn) (assoc 2 bd) bd)) 
  (entmod bd) 
  (entupd b) 
  (princ (strcat "\n" bn))) 

(PDot);************ Main Program *************************************** 
(defun uan_ (/ m_v m_n olderr uan_var s) 
  (uan_smd) 
  (GetOne) 
  (UnAnon s) 
  (uan_rmd)) 

(defun c:UnAnonall (/ ss i) 
 (setq ss (ssget "X" (list (cons 0 "INSERT")(cons 67 (if (= (getvar "TILEMODE") 1) 0 1))))) 
 (and ss 
   (setq i (sslength ss)) 
   (while (not (minusp (setq i (1- i)))) 
          (setq en (ssname ss i)) 
          (if (= "*U" (substr (cdr (assoc 2 (entget en))) 1 2)) 
              (UnAnon en)))) 
 (prin1)) 

(PDot);************ Load Program *************************************** 
(defun C:UnAnon () (uan_)) 
(if uan_ (princ "\nUnAnon Loaded\n")) 
(prin1) 
;================== End Program ======================================== 
