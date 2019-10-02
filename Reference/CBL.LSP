;;;;;;; Change selected objects to the color of their layer
(defun c:CBL () (c:ColorByLayer))
(defun c:ColorByLayer  (/ *error*)
  (princ "\rCOLORBYLAYER ")
  (vl-load-com)

  (defun *error*  (msg)
    (cond ((not msg) (if acDoc (vla-endundomark acDoc)))           ; Normal exit
          ((member msg '("Function cancelled" "quit / exit abort")))    ; <esc> or (quit)
          ((princ (strcat "\n** Error: " msg " ** "))))                 ; Fatal error, display it
    (princ))

  ((lambda (acDoc / ss oLayer oLayers oLayersList)
     (if (setq ss (ssget "_:L"))
       (progn
         (vla-startundomark acDoc)
         (vlax-for x (setq ss (vla-get-activeselectionset acDoc))
           (if (= :vlax-true
                  (vla-get-lock
                    (setq oLayer (vla-item (cond (oLayers)
                                                 ((setq oLayers
                                                         (vla-get-layers acDoc))))
                                           (vla-get-layer x)))))
             (progn
               (setq oLayersList (cons oLayer oLayersList))
               (vla-put-lock oLayer :vlax-false)))
           (vl-catch-all-apply
             'vla-put-color
             (list x (vla-get-color oLayer))))
         (vla-delete ss)
         (foreach oLayer  oLayersList
           (vla-put-lock oLayer :vlax-true))
         (*error* nil))
       (*error* "Nothing selected")))
    (vla-get-activedocument (vlax-get-acad-object))))