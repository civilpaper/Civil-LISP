;;  LayerIsolateOnOff.lsp [command names: LIO, LUO]
;;  To Isolate and Unisolate only the On-Off condition of Layers of selected objects.
;;  LIO isolates Layers of selected objects, leaving those Layers on and turning all
;;  other Layers off that are not already off.  If repeated before LUO turns those
;;  Layers back on, makes further isolations, to as many levels as desired.
;;  LUO turns latest set of turned-off Layers back on, without undoing other Layer
;;  options that may have been used under isolated conditions [as happens with
;;  some (e.g. colors) if using AutoCAD's standard LAYERUNISO to return to un-
;;  isolated conditions after using LAYISO].  When repeated, steps back through
;;  as many isolations as were done with LIO [LAYISO can only step back once].
;;  Kent Cooper, August 2011

(vl-load-com)

(defun liV (sub); = build Variable name with subtype and current integer ending
  (read (strcat "li" sub (itoa liinc)))
); defun

(defun liG (sub); = Get what's in the above variable
  (eval (read (strcat "li" sub (itoa liinc))))
); defun

(defun C:LIO (/ ss cmde laysel layname lion layobj); = Layer Isolate -- On-Off condition only
  (prompt "\nTo designate Layer(s) to remain on,")
  (if (setq ss (ssget)); object selection
    (progn
      (setq cmde (getvar 'cmdecho))
      (setvar 'cmdecho 0)
      (command "_.undo" "_begin")
      (repeat (sslength ss); make list of Layer names to remain on
        (setq laysel (cdr (assoc 8 (entget (ssname ss 0))))); Layer name
        (if (not (member laysel lion)) (setq lion (cons laysel lion))); add if not already there
        (ssdel (ssname ss 0) ss)
      ); repeat
      (setq liinc (if liinc (1+ liinc) 1)); liinc is global; 1 for first time, etc.
      (if
        (set (liV "cur"); global variable(s), but need(s) to be:
          (if (not (member (getvar 'clayer) lion)); nil if current Layer kept on
            (vlax-ename->vla-object (tblobjname "layer" (getvar 'clayer)))
          ); if
        ); set
        (setvar 'clayer (nth 0 lion)); then - make some selected object's Layer current
      ); if
      (while (setq layname (cdadr (tblnext "layer" (not layname)))); step through Layers
        (if
          (and
            (not (member layname lion)); not among selected objects' Layers
            (> (cdr (assoc 62 (tblsearch "layer" layname))) 0); currently on
          ); and
          (progn
            (setq layobj (vlax-ename->vla-object (tblobjname "layer" layname)))
            (set (liV "off") (cons layobj (liG "off")))
              ; put in list of Layers turned off -- makes global variables lioff1, lioff2, etc.
            (vla-put-LayerOn layobj 0); turn off
          ); progn
        ); if
      ); while
      (prompt
        (strcat
          "\n"
          (itoa (length lion))
          " Layer(s) isolated, "
          (itoa (length (liG "off")))
          " Layer(s) turned off."
          (if (liG "cur")
            (strcat "  Layer " (getvar 'clayer) " has been made current."); then
            "" ; else - add nothing to prompt if current Layer remains on
          ); if
        ); strcat
      ); prompt
      (command "_.undo" "_end")
      (setvar 'cmdecho cmde)
    ); progn
    (prompt "\nNothing selected.")
  ); if
  (princ)
); defun

(defun C:LUO (/ cmde lugone lucur); = Layer Unisolate -- On-Off condition only
  (if (> liinc 0); at least one list of turned-off Layers exists
    (progn ; then
      (setq cmde (getvar 'cmdecho))
      (setvar 'cmdecho 0)
      (command "_.undo" "_begin")
      (foreach lay (liG "off"); latest numbered list
        (if (vlax-vla-object->ename lay); still in drawing
          (vla-put-LayerOn lay -1); then - turn on
          (progn ; else
            (vl-remove lay (liG "off")); to adjust number for prompt later
            (setq lugone (if lugone (1+ lugone) 1)); quantity of no-longer-present Layers
          ); progn
        ); if
      ); foreach
      (if ; restore Layer current at time of corresponding LIO if it was turned off
        (and
          (liG "cur"); nil if it wasn't
          (vlax-vla-object->ename (liG "cur")); Layer still in drawing, even if renamed
        ); and
        (progn
          (setq lucur (vla-get-Name (liG "cur"))); present name if renamed since its LIO
          (setvar 'clayer lucur); restore as current
        ); progn
      ); if
      (prompt
        (strcat
          "\n"
          (itoa (length (liG "off")))
          " Layer(s) turned back on."
          (if (liG "cur") ; corresponding LIO turned off current Layer at the time
            (strcat ; then
              "\nLayer "
              (if (vlax-vla-object->ename (liG "cur")); still in drawing
                (vla-get-Name (liG "cur")); then - name, even if renamed
                "current at time of LIO purged, and not"
              ); if
              " restored as current."
            ); strcat
            "" ; else - add nothing if corresponding LIO kept current Layer on
          ); if
          (if lugone (strcat "\n" (itoa lugone) " purged Layer(s) not turned back on.") "")
        ); strcat
      ); prompt
      (set (liV "off") nil); clear list ending with latest integer in use
      (set (liV "cur") nil); clear current-at-LIO-Layer-if-changed value with latest integer
      (setq liinc (1- liinc)); increment downward for next-earlier list
      (command "_.undo" "_end")
      (setvar 'cmdecho cmde)
    ); progn
    (prompt "\nNo Layers to Unisolate."); else
  ); if
  (princ)
); defun

(prompt "\nType LIO to Isolate Layers [On/Off only]; LUO to Unisolate.")