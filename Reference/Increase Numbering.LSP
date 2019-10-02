;;----------------=={ Attribute Numbering }==-----------------;;
;;                                                            ;;
;;  Prompts for block insertion and proceeds to number the    ;;
;;  specified attribute with an incrementing numerical string ;;
;;  with optional prefix and suffix text.                     ;;
;;------------------------------------------------------------;;
;;  Author: Lee Mac, Copyright © 2010 - www.lee-mac.com       ;;
;;------------------------------------------------------------;;

(defun c:AttNum ( / *error* _StartUndo _EndUndo _Insert _ApplyZeros
                    block zeros tag prefix suffix doc spc pt )
  (vl-load-com)
  ;; © Lee Mac 2010

  (setq

;;------------------------------------------------------------;;
;;                      Adjustments                           ;;
;;------------------------------------------------------------;;
  
  block  "circle_numbering"   ;; Block to Insert  (block name or full filepath)

  zeros  2         ;; Length of Number (2 => 01)

  tag    "TAG"    ;; Attribute Tag to Number

  prefix ""    ;; Prefix ("" for none)

  suffix ""    ;; Suffix ("" for none)

;;------------------------------------------------------------;;

  )

  (defun *error* ( msg )
    (if doc (_EndUndo doc))
    (or (wcmatch (strcase msg) "*BREAK,*CANCEL*,*EXIT*")
        (princ (strcat "\n** Error: " msg " **")))
    (princ)
  )

  (defun _StartUndo ( doc ) (_EndUndo doc)
    (vla-StartUndoMark doc)
  )

  (defun _EndUndo ( doc )
    (if (= 8 (logand 8 (getvar 'UNDOCTL)))
      (vla-EndUndoMark doc)
    )
  )

  (defun _Insert ( space block point )
    (vla-InsertBlock space (vlax-3D-point point) block 1. 1. 1. 0.)
  )

  (defun _ApplyZeros ( str len )
    (if (< (strlen str) len)
      (_ApplyZeros (strcat "0" str) len)
      str
    )
  )

  (LM:ActiveSpace 'doc 'spc)
  
  (cond
    (
      (not (setq block (LM:FindBlock block)))

      (princ "\n** Block not Found **")
    )
    (t

      (setq *num
        (1-
          (cond
            (
              (getint
                (strcat "\nSpecify Starting Number <"
                  (itoa
                    (setq *num
                      (1+
                        (cond ( *num ) ( 0 ))
                      )
                    )
                  )
                  "> : "
                )
              )
            )
            ( *num )
          )
        )
      )

      (_StartUndo doc)

      (while (setq pt (getpoint "\nPoint for Block: "))
        (LM:SetAttributeValue
          (_Insert spc block (trans pt 1 0)) tag
          (strcat prefix (_ApplyZeros (itoa (setq *num (1+ *num))) zeros) suffix)
        )
      )

      (_EndUndo doc)
    )
  )

  (princ)
)

;;--------------------=={ ActiveSpace }==---------------------;;
;;                                                            ;;
;;  Retrieves pointers to the Active Document and Space       ;;
;;------------------------------------------------------------;;
;;  Author: Lee Mac, Copyright © 2010 - www.lee-mac.com       ;;
;;------------------------------------------------------------;;
;;  Arguments:                                                ;;
;;  *doc - quoted symbol (other than *doc)                    ;;
;;  *spc - quoted symbol (other than *spc)                    ;;
;;------------------------------------------------------------;;

(defun LM:ActiveSpace ( *doc *spc )
  ;; © Lee Mac 2010
  (set *spc
    (vlax-get-property
      (set *doc
        (vla-get-ActiveDocument
          (vlax-get-acad-object)
        )
      )
      (if (= 1 (getvar 'CVPORT)) 'PaperSpace 'ModelSpace)
    )
  )
)

;;----------------=={ Set Attribute Value }==-----------------;;
;;                                                            ;;
;;  Populates the first attribute matching the tag specified  ;;
;;  found within the block supplied with the value specified, ;;
;;  if present.                                               ;;
;;------------------------------------------------------------;;
;;  Author: Lee Mac, Copyright © 2010 - www.lee-mac.com       ;;
;;------------------------------------------------------------;;
;;  Arguments:                                                ;;
;;  block - VLA Block Reference Object                        ;;
;;  tag   - Attribute TagString                               ;;
;;  value - Value to which the Attribute will be set          ;;
;;------------------------------------------------------------;;
;;  Returns:  Value the attribute was set to, else nil        ;;
;;------------------------------------------------------------;;

(defun LM:SetAttributeValue ( block tag value )
  ;; © Lee Mac 2010
  (vl-some
    (function
      (lambda ( attrib )
        (if (eq tag (vla-get-TagString attrib))
          (progn (vla-put-TextString attrib value) value)
        )
      )
    )
    (vlax-invoke block 'GetAttributes)
  )
)

;;---------------------=={ Find Block }==---------------------;;
;;                                                            ;;
;;  Attempts to locate a block definition using supplied      ;;
;;  information.                                              ;;
;;------------------------------------------------------------;;
;;  Author: Lee Mac, Copyright © 2010 - www.lee-mac.com       ;;
;;------------------------------------------------------------;;
;;  Arguments:                                                ;;
;;  block - block name or filename                            ;;
;;------------------------------------------------------------;;
;;  Returns:  Block name or filename, else nil                ;;
;;------------------------------------------------------------;;

(defun LM:FindBlock ( block / path base extn )
  ;; © Lee Mac 2010
  (setq base  (vl-filename-base      block)
        path  (vl-filename-directory block)
        extn  (vl-filename-extension block))

  (or (eq "" path) (setq path (strcat path "\\")))
  
  (cond
    ( (tblsearch "BLOCK" base) base )

    ( (findfile (strcat path base (cond ( extn ) ( ".dwg" )))) )
  )
)