(princ "\nRe-space (RS)  Version 2.0  4-13-91  by Jon Shapiro")
;
;This program will take a series of text strings, selected individually
;and in sequence, and revise the spacing between each line.  This
;should be especially useful after running a lisp routine to revise the
;height of a group of text entities.  Each entity would need to have it's
;insert point shifted in accordance with the new text height.
;
;Version 2.0 has been improved to handle other types of text justification
;modes in addition to left justified (i.e. right, middle).

;----------------------------------
;Error handler
(defun MYERR (MSG)
   (if (and (/= MSG "Function cancelled") (/= MSG "console break"))
      (progn
         (princ "\nError: ")
         (princ MSG)
      )
   )
   (setq *error* OLDER)
   (setvar "cmdecho" 1)
   (princ)
)
;---------------------------
;
(defun c:rs (/ older tmpdis ss sslen ctr ent txtrot just code inspt
               xpt ypt entlis tdisp x y pt theta)
   (setvar "cmdecho" 0)
   (command ".undo" "m")
   (setq older *error*
         *error* myerr
   )
   (initget (+ 1 2 4)) ;disallow negative and zero responses
   (if (= dis nil)
      (progn
         (initget (+ 1 2 4)) ;disallow nul negative and zero responses
         (setq dis (getdist "\nDistance between text strings: "))
      )
      (progn
         (initget (+ 2 4)) ;disallow negative and zero responses
         (setq tmpdis dis
               dis (getdist (strcat "\nDistance between text strings <"
                         (rtos dis) ">: " )
                   )
         );close setq
         (if (= dis nil)
            (setq dis tmpdis)
         )
      );close progn
   );close if
   (princ "\nSelect text strings to respace in sequence: ")
   (setq ss (ssget)
         sslen (sslength ss)
         ctr 0  ;initialize counter
   );close setq
   (while (and (/= ss nil) (>= sslen 0) (< ctr sslen))
      (setq ent (ssname ss ctr))
      (if (= ctr 0)
         (progn
            (setq txtrot (cdr (assoc 50 (entget ent)))
                  just (cdr (assoc 72 (entget ent)))
            )
            (if (> just 0)
               (setq code 11)    ;non-left justified
               (setq code 10)    ;left justified
            )
            (setq inspt (cdr (assoc code (entget ent)))
                  xpt (car inspt)
                  ypt (cadr inspt)
            );close setq
         );close progn
         (progn
;------------------------------------------------------------------------
;lets take different action depending on rotation angle of text entities
;------------------------------------------------------------------------

;---------------------------------------------------
;Lets also check text justification mode.
;We should use group code 10 for left justified
;text.  All others we should use group code 11
;this will maintain justification
;---------------------------------------------------

            (cond
               (   (< txtrot (* 0.5 pi))       ;if rotation angle <90 deg
                   (setq entlis (entget ent)
                         just (cdr (assoc 72 entlis))
                   )
                   (if (> just 0)
                      (setq code 11)
                      (setq code 10)
                   )
                   (setq tdisp (* ctr dis)
                         x (+ xpt (* tdisp (sin txtrot)))
                         y (- ypt (* tdisp (cos txtrot)))
                         pt (list x y)
                         entlis (subst (cons code pt) (assoc code entlis) entlis)
                   );close setq
                   (entmod entlis)
               );close 1st cond

               (   (and (< txtrot pi) (>= txtrot (* 0.5 pi))) ;if rotation angle <180 & >= 90 deg
                   (setq entlis (entget ent)
                         theta (- txtrot (* 0.5 pi))
                         tdisp (* ctr dis)
                         x (+ xpt (* tdisp (cos theta)))
                         y (+ ypt (* tdisp (sin theta)))
                         pt (list x y)
                         entlis (subst (cons code pt) (assoc code entlis) entlis)
                   );close setq
                   (entmod entlis)
               );close 2nd cond

               (   (and (< txtrot (* 1.5 pi)) (>= txtrot pi)) ;if rot angle <270 & >= 180 deg
                   (setq entlis (entget ent)
                         theta (- txtrot pi)
                         tdisp (* ctr dis)
                         x (- xpt (* tdisp (sin theta)))
                         y (+ ypt (* tdisp (cos theta)))
                         pt (list x y)
                         entlis (subst (cons code pt) (assoc code entlis) entlis)
                   );close setq
                   (entmod entlis)
               );close 3rd cond

               (   (and (< txtrot (* 2.0 pi)) (>= txtrot (* 1.5 pi))) ;if rot angle <360 & >= 270 deg
                   (setq entlis (entget ent)
                         theta (- txtrot (* 1.5 pi))
                         tdisp (* ctr dis)
                         x (- xpt (* tdisp (cos theta)))
                         y (- ypt (* tdisp (sin theta)))
                         pt (list x y)
                         entlis (subst (cons code pt) (assoc code entlis) entlis)
                   );close setq
                   (entmod entlis)
               );close 4th cond
            );close cond text rotation angles
;-------------------------------------------------------------------------
         );close progn
      );close if
      (setq ctr (1+ ctr))  ;increment counter
   );close while
   (setvar "cmdecho" 1)
   (setq *error* older)
   (princ);quiet exit
);close defun respace
(princ);quiet load
