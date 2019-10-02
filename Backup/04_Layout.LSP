;*******************************************************************************
;*                             Layout tab                                      *
;*******************************************************************************


;;;**************************ViewPort Lock & Unlock**************************
;;; Command
;;;    VPL ~ Lock selected viewport
;;;    VPU ~ Unlock selected viewport
;;;    VPLA ~ Lock ALL viewports
;;;    VPUA ~ Unlock ALL viewports


; By Lee-Mac found at the Augi forums

;; Lock Selected Viewport

(vl-load-com)
(defun c:vpl nil
  (if (SSVPLock	(ssget "_+.:E:S:L" '((0 . "VIEWPORT")))
		:vlax-true
      )
    (princ "\n--> Viewport Locked.")
  )
  (princ)
)
;; Unlock Selected Viewport
(defun c:vpu nil
  (if (SSVPLock	(ssget "_+.:E:S:L" '((0 . "VIEWPORT")))
		:vlax-false
      )
    (princ "\n--> Viewport Unlocked.")
  )
  (princ)
)

;; Lock All Viewports

(defun c:vpla nil

  (SSVPLock (ssget "_X" '((0 . "VIEWPORT"))) :vlax-true)

  (princ "\n--> All Viewports Locked.")

  (princ)

)

;; Unlock All Viewports

(defun c:vpua nil
  ;; changed "VPLU" to "VPUA" to be consistant with the above function

  (SSVPLock (ssget "_X" '((0 . "VIEWPORT"))) :vlax-false)

  (princ "\n--> All Viewports UnLocked.")

  (princ)

)

(defun SSVPLock	(ss lock / i)

  (if ss

    (repeat (setq i (sslength ss))

      (vla-put-displaylocked
	(vlax-ename->vla-object (ssname ss (setq i (1- i))))
	lock
      )
      t

    )

  )

)