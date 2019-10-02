;;;;; Changing all objects in a block to layer 0-----by block;;;;;;;

(defun c:norm (/ *error* adoc lst_layer func_restore-layers)

  (defun *error* (msg)
    (func_restore-layers)
    (vla-endundomark adoc)
    (princ msg)
    (princ)
    ) ;_ end of defun

  (defun func_restore-layers ()
    (foreach item lst_layer
      (vla-put-lock (car item) (cdr (assoc "lock" (cdr item))))
      (vl-catch-all-apply
        '(lambda ()
           (vla-put-freeze
             (car item)
             (cdr (assoc "freeze" (cdr item)))
             ) ;_ end of vla-put-freeze
           ) ;_ end of lambda
        ) ;_ end of vl-catch-all-apply
      ) ;_ end of foreach
    ) ;_ end of defun

  (vl-load-com)
  (vla-startundomark
    (setq adoc (vla-get-activedocument (vlax-get-acad-object)))
    ) ;_ end of vla-startundomark
  (if (and (not (vl-catch-all-error-p
                  (setq selset
                         (vl-catch-all-apply
                           (function
                             (lambda ()
                               (ssget '((0 . "INSERT")))
                               ) ;_ end of lambda
                             ) ;_ end of function
                           ) ;_ end of vl-catch-all-apply
                        ) ;_ end of setq
                  ) ;_ end of vl-catch-all-error-p
                ) ;_ end of not
           selset
           ) ;_ end of and
    (progn
      (vlax-for item (vla-get-layers adoc)
        (setq
          lst_layer (cons (list item
                                (cons "lock" (vla-get-lock item))
                                (cons "freeze" (vla-get-freeze item))
                                ) ;_ end of list
                          lst_layer
                          ) ;_ end of cons
          ) ;_ end of setq
        (vla-put-lock item :vlax-false)
        (vl-catch-all-apply
          '(lambda () (vla-put-freeze item :vlax-false))
          ) ;_ end of vl-catch-all-apply
        ) ;_ end of vlax-for
      (foreach blk_def
               (mapcar
                 (function
                   (lambda (x)
                     (vla-item (vla-get-blocks adoc) x)
                     ) ;_ end of lambda
                   ) ;_ end of function
                 ((lambda (/ res)
                    (foreach item (mapcar
                                    (function
                                      (lambda (x)
                                        (vla-get-name
                                          (vlax-ename->vla-object x)
                                          ) ;_ end of vla-get-name
                                        ) ;_ end of lambda
                                      ) ;_ end of function
                                    ((lambda (/ tab item)
                                       (repeat (setq tab  nil
                                                     item (sslength selset)
                                                     ) ;_ end setq
                                         (setq
                                           tab
                                            (cons
                                              (ssname selset
                                                      (setq item (1- item))
                                                      ) ;_ end of ssname
                                              tab
                                              ) ;_ end of cons
                                           ) ;_ end of setq
                                         ) ;_ end of repeat
                                       tab
                                       ) ;_ end of lambda
                                     )
                                    ) ;_ end of mapcar
                      (if (not (member item res))
                        (setq res (cons item res))
                        ) ;_ end of if
                      ) ;_ end of foreach
                    (reverse res)
                    ) ;_ end of lambda
                  )
                 ) ;_ end of mapcar
        (vlax-for ent blk_def
          (vla-put-layer ent "0")
          (vla-put-color ent 0)
          (vla-put-lineweight ent aclnwtbyblock)
          (vla-put-linetype ent "byblock")
          ) ;_ end of vlax-for
        ) ;_ end of foreach
      (func_restore-layers)
      (vla-regen adoc acallviewports)
      ) ;_ end of progn
    ) ;_ end of if
  (vla-endundomark adoc)
  (princ)
  ) ;_ end of defun

; Written By: Tom Beauford
; Changes all block entities to layer "0" with color, linetype & lineweight ByBlock
(defun C:FIXBLKS (/ SSET intCount ENAM ELST BNAM FLST FIX1)
  (defun FIX1 (BNAM / BENAM BONAM)
    (if (not (member BNAM FLST))
   (progn
     (setq FLST  (cons BNAM FLST)
     BENAM (tblobjname "block" BNAM)
     )
   (while (setq BENAM (entnext BENAM))
      (if (= (cdr (assoc 0 (entget BENAM))) "INSERT")
       (fix1 (cdr (assoc 2 (entget BENAM))))
       (progn
         (setq BONAM(vlax-ename->vla-object BENAM))
                (vl-catch-all-apply 'vla-put-layer (list BONAM "0"))
                (vl-catch-all-apply 'vla-put-color (list BONAM 0))
                (vl-catch-all-apply 'vla-put-linetype (list BONAM "Byblock"))
                (vl-catch-all-apply 'vla-put-Lineweight (list BONAM -2))
;                (vl-catch-all-apply 'vla-put-PlotStyleName (list BONAM "Byblock"))
       )
     )
   )
        )
    )
  )
  (setq SSET (ssget (list (cons 0 "INSERT"))))
  (vl-cmdf "undo" "BEgin")
  (repeat (setq intCount (sslength SSET))
    (setq intCount     (1- intCount)
          ENAM (ssname SSET intCOunt)
    ELST (entget ENAM)
    BNAM (cdr (assoc 2 ELST))
    FLST nil
    )
    (fix1 BNAM)
  )
  (vl-cmdf "undo" "End")
  (vl-cmdf "regen")
  (princ)
)