;; Created by: Lee Ambrosius
;; Date Written: 12/11/03

;; Includes four commands which can be redefined.
;; NextLayout - Navigates to the right and will wrap around
;;              to the left side once the end is reached

;; PreviousLayout - Navigates to the left and will wrap around
;;                  to the right side once the end is reached

;; FirstLayout - Navigates to the left most tab

;; LstLayout - Navigates to the right most tab

;; Gets a listing of all Layouts in the drawing
(defun Get-Layout-List( / acadObj acDoc acDocLayouts layoutCount loopCount
                          layoutListLocal layoutListSorted layoutName layoutPosition
                          loopCountSorted)
  (vl-load-com)
  (setq acadObj (vlax-get-acad-object))
  (setq acDoc (vlax-get-property acadObj 'ActiveDocument))
  (setq acDocLayouts (vlax-get-property acDoc 'Layouts))

  (setq layoutCount (vlax-get-property acDocLayouts 'Count)
        loopCount 0
        layoutListLocal (list)
        layoutListSorted (list)
  )
  
  (while (> layoutCount loopCount)
    (setq layoutName (vlax-get-property (vlax-invoke-method acDocLayouts 'Item loopCount) 'Name))
    (setq layoutPosition (vlax-get-property (vlax-invoke-method acDocLayouts 'Item loopCount) 'TabOrder))

    (setq layoutListLocal (append layoutListLocal (list (list layoutPosition layoutName))))
    (setq loopCount (1+ loopCount))
  )

  ;; Resort listing by TabOrder
  (setq layoutCountSorted 0)
  (while (> (length layoutListLocal) (length layoutListSorted))
    (setq loopCountSorted 0)
    (foreach layoutLocation layoutListLocal
      (progn
        (if (and (= (car layoutLocation) (length layoutListSorted)) (= (car layoutLocation) layoutCountSorted))
          (progn
            (setq layoutListSorted (append layoutListSorted (cdr (nth loopCountSorted layoutListLocal))))
	    (setq layoutCountSorted (1+ layoutCountSorted))
          )
        )
        (setq loopCountSorted (1+ loopCountSorted))
      )
    )
  )
  layoutListSorted
)

(defun c:nextlayout ( / layout-mem-list layout-list layoutLocation)
  (setq layoutLocation 0)
  (setq layout-list (get-layout-list))
  (setq layout-mem-list (member (getvar "CTAB") layout-list))
  (if layout-mem-list
    (progn
      (setq layoutLocation (- (length layout-list) (length layout-mem-list)))
    )
    (setq layoutLocation (1+ layoutLocation))
  )
  (if (>= (1+ layoutLocation) (length layout-list))
    (setvar "CTAB" (nth 0 layout-list))
    (setvar "CTAB" (nth (1+ layoutLocation) layout-list))
  )
)

(defun c:previouslayout ( / layout-mem-list layout-list layoutLocation)
  (setq layoutLocation 0)
  (setq layout-list (get-layout-list))
  (setq layout-mem-list (member (getvar "CTAB") layout-list))
  (if layout-mem-list
    (progn
      (setq layoutLocation (- (length layout-list) (length layout-mem-list)))
    )
    (setq layoutLocation (1- layoutLocation))
  )
  (if (= layoutLocation 0)
    (setvar "CTAB" (nth (1- (length layout-list)) layout-list))
    (setvar "CTAB" (nth (1- layoutLocation) layout-list))
  )
)

(defun c:FirstLayout ( / layout-list layoutLocation)
  (setvar "CTAB" (nth 0 (get-layout-list)))
)

(defun c:LastLayout ( / layout-list)
  (setq layout-list (get-layout-list))

  (setvar "CTAB" (nth (- (length layout-list) 1) layout-list))
)

(princ)

(DEFUN C:TMN() (C:NEXTLAYOUT))
(DEFUN C:TMP() (C:PREVIOUSLAYOUT))
(DEFUN C:TMF() (C:FIRSTLAYOUT))
(DEFUN C:TML() (C:LASTLAYOUT))