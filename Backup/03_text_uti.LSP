(defun c:mattext (/ ss1 ss2 enam enam2 ent2 en eg count)
  (setvar "cmdecho" 0)
  (prompt "\nSource:")
  (setq ss2l 0)
  (while (/= ss2l 1)
    (setq ss2 (ssget))
    (setq ss2l (sslength ss2))
  )
  
  (prompt "\nDestination:")
  (setq ss1 (ssget '((0 . "TEXT"))))
  (setq enam2 (ssname ss2 0) ent2 (entget enam2))
  (setq count 0)
  (repeat (sslength ss1)
    (setq en (ssname ss1 count) eg (entget en) eg (subst (assoc 1 ent2) (assoc 1 eg) eg) count (1+ count))
    (entmod eg)
  )
  (princ)
  (setvar "cmdecho" 1)
)
;;;;;;;;;;;;;;;;;;;
(defun c:=() (c:mattext))
;;;;;;;;;;;;;;;;;;;;;
(setq ct1_height 1.8)
(defun c:ct1(/ ss1 count en eg)
  (prompt "\nSelect Texts:")
  (setq ss1 (ssget '((0 . "TEXT"))))
  (setq count 0)
  (repeat (sslength ss1)
    (setq en (ssname ss1 count))
    (setq eg (entget en))
    (setq eg (subst (cons 40 (* ct1_height ly_scale)) (assoc 40 eg) eg))
    (setq count (1+ count))
    (entmod eg)
  )
)
;;;;;;;;;;;;;;;;;;;;;
(defun ct1(ss1 / count en eg)
  (prompt "\nSelect Texts:")
  ;(setq ss1 (ssget '((0 . "TEXT"))))
  (setq count 0)
  (repeat (sslength ss1)
    (setq en (ssname ss1 count))
    (setq eg (entget en))
    (setq eg (subst (cons 40 (* ct1_height ly_scale)) (assoc 40 eg) eg))
    (setq count (1+ count))
    (entmod eg)
  )
)
;===================
(setq ct2_height 2.5)
(defun c:ct2(/ ss1 count en eg)
  (prompt "\nSelect Texts:")
  (setq ss1 (ssget '((0 . "TEXT"))))
  (setq count 0)
  (repeat (sslength ss1)
    (setq en (ssname ss1 count))
    (setq eg (entget en))
    (setq eg (subst (cons 40 (* ct2_height ly_scale)) (assoc 40 eg) eg))
    (setq count (1+ count))
    (entmod eg)
  )
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun c:explode_mtext(/ ss1 count en)
  (setq ss1 (ssget '((0 . "MTEXT"))))
  (if (= ss1 nil)
    (progn
      (prompt "\nNo Mtext ")
      (exit)
    )
  )
  (setq count 0)
  (repeat (sslength ss1)
    (setq en (ssname ss1 count))
    (command ".explode" en)
    (setq count (1+ count))
  )
  
)
(defun c:xt() (c:explode_mtext))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;(prompt "\n		MJ: Di chuyen chu cung COT voi nhau")
(defun c:MJ ( / code ssl #sset)
     ;(Text_get)
     (setq #sset (ssget '((0 . "TEXT"))))
     (setq pt (getpoint "\nInput Middle point"))
     (setq ssl (sslength #sset))
     
     (while (> ssl 0)
         (progn
               (setq temp (ssname #sset (setq ssl (1- ssl)))
                    ed (entget temp)

		    old (cons 72 (DXF 72 ed))
		    new (cons 72 1)
		    ed (subst new old ed)

		    old (cons 73 (DXF 73 ed))
		    new (cons 73 2)
		    ed (subst new old ed)


		    old (cons 10 (DXF 10 ed))
		    new (cons 10 (list (car pt) (cadr (dxf 10 ed)) 0.0)) 
		    ed (subst new old ed)

		    old (cons 11 (DXF 11 ed))
		    new (cons 11 (DXF 10 ed))
		    ed (subst new old ed)
		)
          (entmod ed)  
          )

     )
     (princ)
);defun
;-------------------------------------------------------------------

;;;You can hit Enter at the end of each line, and it will move down to the next line,
;;;and you can keep adding as many lines of Text as you like.  When you're done [Enter
;;;twice in a row, or Enter then Esc], it will re-set the Layer to what it was before you started.

(defun C:T (/ MYOLDLAYER)
  (setq MYOLDLAYER (getvar "clayer"))
  (command

    "_.layer" "t" "TEKST-1.5mm" "m" "TEKST-1.5mm" "" "c" "1" "" ""
    "_.dtext" "s" "YourZeroHeightStyle" pause (* (getvar 'dimscale) 2) 0 ; leaves you entering Text

  ); command
  (while (> (getvar 'cmdactive) 0) (command pause)); lets you finish command before:
  (setvar 'clayer MYOLDLAYER)
)
