;-------------------------------------------------------------------------------
; Program Name: T2M - Text to Mtext
; Created By:   Terry Miller (Email: terrycadd@yahoo.com)
;               (URL: http://web2.airmail.net/terrycad)
; Date Created: 7-17-08
; Function:     Changes Text to Mtext
;-------------------------------------------------------------------------------
; Revision History
; Rev  By     Date    Description
;-------------------------------------------------------------------------------
; 1    TM   7-17-08   Initial version
;-------------------------------------------------------------------------------
; c:T2M - Text 2 Mtext
;-------------------------------------------------------------------------------
(defun c:T2M (/ Angle~ Cnt# Color# Dxf10 Dxf11 Dxf72 Dxf73 EntList@ EntName^ InsPt
  Justify# Layer$ MTextPts@ Osmode# P1 P2 SS& Text$ TextPts@ TextSize~ TextStyle$ Width~)
  (princ "\nSelect Text to change to Mtext.")
  (if (setq SS& (ssget (list '(0 . "TEXT"))))
    (progn
      (command "UNDO" "BEGIN")
      (setq Osmode# (getvar "OSMODE"))
      (setvar "OSMODE" 0)
      (setvar "CMDECHO" 0)
      (setq Cnt# 0)
      (repeat (sslength SS&)
        (setq EntName^ (ssname SS& Cnt#))
        (setq EntList@ (entget EntName^))
        (setq Text$ (cdr (assoc 1 EntList@)))
        (setq TextStyle$ (cdr (assoc 7 EntList@)))
        (setq Layer$ (cdr (assoc 8 EntList@)))
        (setq Dxf10 (cdr (assoc 10 EntList@)))
        (setq Dxf11 (cdr (assoc 11 EntList@)))
        (setq TextSize~ (cdr (assoc 40 EntList@)))
        (setq Angle~ (cdr (assoc 50 EntList@)))
        (setq Color# (cdr (assoc 62 EntList@)))
        (setq Dxf72 (cdr (assoc 72 EntList@)))
        (setq Dxf73 (cdr (assoc 73 EntList@)))
        (setq Justify#
          (cond
            ((and (= Dxf72 0)(= Dxf73 3)) 1);TL
            ((and (= Dxf72 1)(= Dxf73 3)) 2);TC
            ((and (= Dxf72 2)(= Dxf73 3)) 3);TR
            ((and (= Dxf72 0)(= Dxf73 2)) 4);ML
            ((and (= Dxf72 1)(= Dxf73 2)) 5);MC
            ((and (= Dxf72 2)(= Dxf73 2)) 6);MR
            ((and (= Dxf72 0)(= Dxf73 1)) 7);BL
            ((and (= Dxf72 1)(= Dxf73 1)) 8);BC
            ((and (= Dxf72 2)(= Dxf73 1)) 9);BR
            ((and (= Dxf72 4)(= Dxf73 4)) 5);MC
            ((and (= Dxf72 1)(= Dxf73 0)) 8);BC
            ((and (= Dxf72 2)(= Dxf73 0)) 9);BR
            (t                            7);BL
          );cond
        );setq
        (if (= Justify# 7)
          (setq InsPt Dxf10)
          (setq InsPt Dxf11)
        );if
        (setq TextPts@ (Text-Box EntName^))
        (setq Width~ (distance (nth 0 TextPts@) (nth 1 TextPts@)))
        (setq P1 (polar (nth 0 TextPts@)
          (angle (nth 0 TextPts@) (nth 2 TextPts@))
          (/ (distance (nth 0 TextPts@) (nth 2 TextPts@)) 2.0))
        );setq
        (command "ERASE" EntName^ "")
        (entmake (list
         '(0 . "MTEXT")
         '(100 . "AcDbEntity")
         '(67 . 0)
          (cons 410 (getvar "CTAB"));Layout name
          (cons 8 Layer$);Layer name
         '(100 . "AcDbMText")
          (cons 10 InsPt);Insertion
          (cons 40 TextSize~);Text size
         '(41 . 0.0)
          (cons 71 Justify#);Justification
         '(72 . 5)
          (cons 1 Text$);Text value
          (cons 7 TextStyle$);Text style
         '(210 0.0 0.0 1.0)
         '(11 1.0 0.0 0.0)
          (cons 42 Width~);Textbox width
          (cons 43 TextSize~);Text size
         '(50 . 0.0)
         '(73 . 1)
         '(44 . 1.0)
        ));list;entmake
        (if Color#
          (command "CHPROP" (entlast) "" "C" Color# "")
        );if
        (command "ROTATE" (entlast) "" InsPt (rtd Angle~))
        (setq MTextPts@ (Text-Box (entlast)))
        (setq P2 (polar (nth 0 MTextPts@)
          (angle (nth 0 MTextPts@) (nth 2 MTextPts@))
          (/ (distance (nth 0 MTextPts@) (nth 2 MTextPts@)) 2.0))
        );setq
        (command "MOVE" (entlast) "" P2 P1)
        (setq Cnt# (1+ Cnt#))
      );repeat
      (setvar "OSMODE" Osmode#)
      (command "UNDO" "END")
    );progn
  );if
  (princ)
);defun c:T2M
;-------------------------------------------------------------------------------
; rtd - Radians to degrees
;-------------------------------------------------------------------------------
(defun rtd (Rad~)
  (* 180.0 (/ Rad~ pi))
);defun rtd
;-------------------------------------------------------------------------------
; Text-Box - Function for Text, Mtext and Dimension entities
; Arguments: 1
;   Entity^ = Entity name of the Text, Mtext or Dimension to use
; Returns: A list of the four corners of the Text Box
;-------------------------------------------------------------------------------
(defun Text-Box (Entity^ / Ang~ AngEntity~ Corners: EntList@ EntNext^ EntType$
  First List@ MovePt NewPts@ Pt Return@ Textboxes@ X X1 X3 Y Y1 Y3 Zero)
  ;-----------------------------------------------------------------------------
  ; Corners: - Calculates the four corners of the Text Box
  ;-----------------------------------------------------------------------------
  (defun Corners: (Entity^ / Ang~ Corners@ Dist~ EntList@ Ins Pt Pt1 Pt2 Pt3 Pt4)
    (setq EntList@ (entget Entity^)
          Corners@ (textbox EntList@)
          Ang~ (cdr (assoc 50 EntList@))
          Ins (cdr (assoc 10 EntList@))
          Pt (mapcar '+ (car Corners@) Ins)
          Pt1 (polar Ins (+ Ang~ (angle Ins Pt)) (distance Ins Pt))
          Pt (mapcar '+ (cadr Corners@) Ins)
          Pt3 (polar Ins (+ Ang~ (angle Ins Pt)) (distance Ins Pt))
          Dist~ (* (distance (car Corners@) (cadr Corners@)) (cos (- (angle Pt1 Pt3) Ang~)))
          Pt2 (polar Pt1 Ang~ Dist~)
          Pt4 (polar Pt3 Ang~ (- Dist~))
    );setq
    (list Pt1 Pt2 Pt3 Pt4)
  );defun Corners:
  ;-----------------------------------------------------------------------------
  (setq EntList@ (entget Entity^)
        EntType$ (cdr (assoc 0 EntList@))
  );setq
  (cond
    ((= EntType$ "TEXT")
      (setq Return@ (Corners: Entity^))
    );case
    ((or (= EntType$ "MTEXT")(= EntType$ "DIMENSION"))
      (command "UNDO" "MARK")
      (setq EntNext^ (entlast))
      (command "EXPLODE" Entity^)
      (if (= EntType$ "DIMENSION")
        (command "EXPLODE" (entlast))
      );if
      (while (setq EntNext^ (entnext EntNext^))
        (if (= "TEXT" (cdr (assoc 0 (entget EntNext^))))
          (setq Textboxes@ (append Textboxes@ (list (Text-Box EntNext^))))
        );if
      );while
      (command "UNDO" "BACK")
      (setq AngEntity~ (angle (nth 0 (nth 0 Textboxes@))(nth 1 (nth 0 Textboxes@)))
            Zero (list 0 0)
            First t
      );setq
      (foreach List@ Textboxes@
        (foreach Pt List@
          (setq X (car Pt) Y (cadr Pt))
          (if First
            (setq First nil X1 X Y1 Y)
          );if
          (if (< X X1)(setq X1 X))
          (if (< Y Y1)(setq Y1 Y))
        );foreach
      );foreach
      (if (or (< X1 0)(< Y1 0))
        (progn
          (cond
            ((and (< X1 0)(< Y1 0))(setq MovePt (list X1 Y1)))
            ((< X1 0)(setq MovePt (list X1 0)))
            ((< Y1 0)(setq MovePt (list 0 Y1)))
          );cond
          (command "UCS" "M" MovePt)
        );progn
      );if
      (setq First t)
      (foreach List@ Textboxes@
        (foreach Pt List@
          (setq Ang~ (- (angle Zero Pt) AngEntity~))
          (setq Pt (polar Zero Ang~ (distance Zero Pt)))
          (setq X (car Pt) Y (cadr Pt))
          (if First
            (setq First nil X1 X X3 X Y1 Y Y3 Y)
          );if
          (if (< X X1)(setq X1 X))
          (if (< Y Y1)(setq Y1 Y))
          (if (> X X3)(setq X3 X))
          (if (> Y Y3)(setq Y3 Y))
        );foreach
      );foreach
      (command "UCS" "W")
      (setq NewPts@ (list (list X1 Y1)(list X3 Y1)(list X3 Y3)(list X1 Y3)))
      (foreach Pt NewPts@
        (setq Ang~ (+ (angle Zero Pt) AngEntity~))
        (setq Pt (polar Zero Ang~ (distance Zero Pt)))
        (setq Return@ (append Return@ (list Pt)))
      );foreach
    );case
  );cond
  Return@
);defun Text-Box
;-------------------------------------------------------------------------------
(princ);End of c:T2M.lsp
