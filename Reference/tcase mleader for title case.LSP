
;;;;;;;;; From this topic http://www.theswamp.org/index.php?topic=38947.0
(defun c:tcase2 ( / acsel func name ) ;; © Lee Mac 2011
  (if
    (ssget "_:L"
     '(
        (-4 . "<OR")
          (0 . "ARCALIGNEDTEXT,ATTDEF,*TEXT")
          (-4 . "<AND")
            (0 . "INSERT")
            (66 . 1)
          (-4 . "AND>")
          (-4 . "<AND")
            (0 . "*DIMENSION")
            (1 . "*?*")
          (-4 . "AND>")
          (-4 . "<AND")
            (0 . "MULTILEADER")
            (304 . "~LEADER_LINE{")
          (-4 . "AND>")
        (-4 . "OR>")
      )
    )       
    (progn
      (initget "Sentence Lower Upper Title toGgle")
      (setq func
        (eval
          (cdr
            (assoc
              (setq case
                (cond
                  (
                    (getkword
                      (strcat "\nSpecify Case [Sentence/Lower/Upper/Title/toGgle] <"
                        (setq case (cond ( case ) ( "Upper" ))) ">: "
                      )
                    )
                  )
                  ( case )
                )
              )
             '(
                ("Upper" . strcase)
                ("Lower" . (lambda ( x ) (strcase x t)))
                ("Title" . EE:TitleCase)
                ("Sentence" . LM:SentenceCase)
                ("toGgle" . LM:ToggleCase)
              )
            )
          )
        )
      )
      (vlax-for obj
        (setq acsel
          (vla-get-activeselectionset
            (vla-get-activedocument (vlax-get-acad-object))
          )
        )
        (cond
          ( (wcmatch (setq name (vla-get-objectname obj)) "AcDb*Dimension")
            (vla-put-textoverride obj (func (vla-get-textoverride obj)))
          )
          ( (or (eq "RText" name)(eq "AcDbArcAlignedText" name))
            (vlax-put-property obj 'contents (func (vlax-get-property obj 'contents)))
          )
          ( (eq "AcDbMLeader" name)
            (if (eq acmtextcontent (vla-get-contenttype obj))
              (vla-put-textstring obj (func (vla-get-textstring obj)))
            )
          )
          ( (eq "AcDbBlockReference" name)
            (foreach att (vlax-invoke obj 'getattributes)
              (vla-put-textstring att (func (vla-get-textstring att)))
            )
          )
          ( t
            (vla-put-textstring obj (func (vla-get-textstring obj)))
          )
        )
      )
      (vla-delete acsel)
    )
  )
  (princ)
)

(defun EE:TitleCase ( s ) ;; ElpanovEvgeniy
  (vl-list->string
    (mapcar
      (function
        (lambda ( a b c ) (if (= 32 a) b c))
      )
      (cons 32 (vl-string->list s))
      (vl-string->list (strcase s))
      (vl-string->list (strcase s t))
    )
  )
)

(defun LM:SentenceCase ( s / f )
  (vl-list->string
    (mapcar
      (function
        (lambda ( a b c )
          (if (or f (= 46 a)) (progn (setq f (= 32 b)) b) c)
        )
      )
      (cons 46 (vl-string->list s))
      (vl-string->list (strcase s))
      (vl-string->list (strcase s t))
    )
  )
)

(defun LM:ToggleCase ( s )
  (vl-list->string
    (mapcar
      (function
        (lambda ( a b c ) (if (< 96 a 123) b c))
      )
      (vl-string->list s)
      (vl-string->list (strcase s))
      (vl-string->list (strcase s t))
    )
  )
)

(vl-load-com) (princ)