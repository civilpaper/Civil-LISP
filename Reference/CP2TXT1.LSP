;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;	Copied from Autodesk Discussion Group
;	Date: 1/10/06
;
;	This Program was designed to copy text from a dwg
;	to notepad
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun c:TXOUT (/ elist en fn fname i ss txt)
(setvar "cmdecho" 0)
(prompt "\n* Text file written to directory of current drawing *")
(if (setq ss (ssget (list (cons 0 "TEXT"))))
(progn (setq fname (getstring "\n* Enter text file name: "))
(if (= fname "")
(setq fname (substr (getvar "dwgname") 1 (- (strlen (getvar 
"dwgname")) 4))))
(setq fn (open (strcat (getvar "dwgprefix") fname ".txt") "w"))
(setq i -1)
(repeat (sslength ss)
(setq i (1+ i))
(setq en (ssname ss i)
elist (entget en)
txt (cdr (assoc 1 elist)))
(write-line txt fn))
(close fn)))
(princ (strcat "\n* Text file " (getvar "dwgprefix") fname " has been 
created *"))
(setvar "cmdecho" 1)

(setq fn (strcat (getvar "dwgprefix") fname ".txt"))
(startapp (strcat "Notepad " (chr 34) fn (chr 34))) 
(princ))

;----------------------------------------------------------
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;	Copied from Autodesk Discussion Group
;	Date: 1/10/06
;
;	This Program was designed to copy text from a dwg
;	to notepad
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun c:TXOUT (/ elist en fn fname i ss txt)
(setvar "cmdecho" 0)
(if (setq ss (ssget (list (cons 0 "TEXT"))))
(progn
(setq fname (getfiled "Save Text File As:" "" "txt" 1))
(setq fn (open fname "w"))
(setq i -1)
(repeat (sslength ss)
(setq i (1+ i))
(setq en (ssname ss i)
elist (entget en)
txt (cdr (assoc 1 elist)))
(write-line txt fn))
(close fn)))
(princ (strcat "\n* Text file " fname " \n has been created *"))
(setvar "cmdecho" 1)

(setq fn fname)
(startapp (strcat "Notepad " (chr 34) fn (chr 34)))
(princ))