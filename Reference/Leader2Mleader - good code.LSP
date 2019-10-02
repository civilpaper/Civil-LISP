;;;http://www.cadtutor.net/forum/showthread.php?39390-leader-to-mleader-routine-changes&s=94dc5e1279a7fa960a7792f8dcd04dda

(defun c:L2M (/            dxf          #OldCmdecho  #OldLayer
               #Leader      #LeaderEnt   #Mtext       #MtextEnt
               #Layer       #LeaderIns   #MtextIns    #MtextString
              )

  (defun dxf (code elist)
    (cdr (assoc code elist))
  ) ;_ defun

  ;; store cmdecho setting
  (setq #OldCmdecho (getvar "cmdecho")
        ;; store current layer
        #OldLayer   (getvar "clayer")
  ) ;_ setq

  ;; select leader & text object with a little error trapping
  ;; this way, the program won't continue if all required variables haven't been filled
  (if (and (setq #Leader (car (entsel "\nSelect leader: ")))
           ;; check to make "LEADER" selected and entget the selected leader
           (eq "LEADER" (dxf 0 (setq #LeaderEnt (entget #Leader))))
           ;; select mtext
           (setq #Mtext (car (entsel "\nSelect MText: ")))
           ;; check to make sure "MTEXT" selected and entget the selected mtext
           (eq "MTEXT" (dxf 0 (setq #MtextEnt (entget #Mtext))))
      ) ;_ and
    (progn
      ;; set cmdecho to 0, so 'command' execution is hidden
      (setvar "cmdecho" 0)
      ;; leader layer
      (setq #Layer       (dxf 8 #LeaderEnt)
            ;; insertion of leader
            #LeaderIns   (dxf 10 #LeaderEnt)
            ;; insertion of mtext
            #MtextIns    (dxf 10 #MtextEnt)
            ;; text inside mtext
            #MtextString (dxf 1 #MtextEnt)
      ) ;_ setq
      ;; set ucs to view
      (command "_.ucs" "_view")
      ;; set current layer to match selected leader
      (setvar "clayer" #Layer)
      ;; create mleader object (the "_non" are osnap overrides to ignore running osnaps)
      ;; the trans will translate the points to world ucs to ensure the mleader is drawn at same two points
      (command "_.mleader" "_non" (trans #LeaderIns 0 1) "_non" (trans #MtextIns 0 1) #MtextString)
      ;; delete leader & mtext
      (entdel #Leader)
      (entdel #Mtext)
      ;; reset ucs to previous
      (command "_.ucs" "_previous")
      ;; reset current layer
      (and #OldLayer (setvar "clayer" #OldLayer))
      ;; reset cmdecho
      (and #OldCmdecho (setvar "cmdecho" #OldCmdecho))
    ) ;_ progn
  ) ;_ if
  ;; exit quietly
  (princ)
) ;_ defun