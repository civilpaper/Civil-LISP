;;;=======================[ TextCase.lsp ]=============================
;;; Author:  Charles Alan Butler Copyright© 2009  CAB - TheSwamp.org
;;; Version: 1.01  June 6, 2009
;;; Purpose: Change case of characters in Text & mText
;;;====================================================================

;;  CAB work in progress
;;  No support for Fields at this time
;;  No support for decimals in sentence case at this time
;;  Need to add attributes & tables
;;  Uses ssget to select text but will not work for attributes
;;    and tables. Will need to use nentsel

;; ==============  Get Text One by One  =============================
(defun c:caseU() (GetEachText "Upper"))     ;  ALL TEXT TO UPPER CASE
(defun c:caseL() (GetEachText "Lower"))     ;  all text to lower case
(defun c:caseS() (GetEachText "Sentence"))  ;  All text to sentence case.
(defun c:caseSo() (GetEachText "SenOnly"))  ;  First to cap but no CHANGE to any other.
(defun c:caseT() (GetEachText "Title"))     ;  All Text To Title Case
(defun c:caseI() (GetEachText "Invert"))    ;  invert case of each letter

(defun c:case(/ action)  ; User to pick type of Case Change
  (initget "Upper Lower Invert Sentence Title senOnly None")
  (setq action (getkword (strcat "\nChange text case to ..."
                            "\nUpper/Lower/Invert/Sentence/Title/senOnly:")))
  (if action (GetEachText action))
)

(defun GetEachText (act / ss obj)
  (vl-load-com)
  (while
    (progn                    ; ssget will not work with tables & attributes
      (prompt (strcat "\n Select text to change to " act " case: "))
      (setq ss (ssget ":S" '((0 . "TEXT,MTEXT"))))
    )
     (vla-EndUndoMark (vla-get-ActiveDocument (vlax-get-acad-object)))
     (vla-StartUndoMark (vla-get-activedocument (vlax-get-acad-object)))
     (setq obj (vlax-ename->vla-object (ssname ss 0)))
     (textcase act obj)
     (vla-EndUndoMark (vla-get-ActiveDocument (vlax-get-acad-object)))
  )
  (princ)
)

;;  =============  Get Text by Selection Set  =========================
(defun c:casessU() (GetssText "Upper"))     ;  ALL TEXT TO UPPER CASE
(defun c:casessL() (GetssText "Lower"))     ;  all text to lower case
(defun c:casessS() (GetssText "Sentence"))  ;  All text to sentence case.
(defun c:casessSo() (GetssText "SenOnly"))  ;  First to cap but no CHANGE to any other.
(defun c:casessT() (GetssText "Title"))     ;  All Text To Title Case
(defun c:casessI() (GetssText "Invert"))    ;  invert case of each letter

(defun c:casess(/ action)  ; User to pick type of Case Change
  (initget "Upper Lower Invert Sentence Title senOnly None")
  (setq action (getkword (strcat "\nChange text case to ..."
                            "\nUpper/Lower/Invert/Sentence/Title/senOnly:")))
  (if action (GetEachText action))
)

(defun GetssText (act / ss i)
  (vl-load-com)
  (prompt (strcat "\n Select text to change to " act " case: "))
  (if (setq ss (ssget '((0 . "TEXT,MTEXT"))))
    (progn
     (vla-EndUndoMark (vla-get-ActiveDocument (vlax-get-acad-object)))
     (vla-StartUndoMark (vla-get-activedocument (vlax-get-acad-object)))
     (setq i -1)
     (while (setq ename (ssname ss (setq i (1+ i))))
       (textcase act (vlax-ename->vla-object ename))
     )
     (vla-EndUndoMark (vla-get-ActiveDocument (vlax-get-acad-object)))
    )
  )
  (princ)
)
;;====================================================================




;;====================================================================
;; ====================  Main Function   =============================
;;====================================================================

;;  action is a KEY word, obj is the text object
(defun textcase (action obj / err newtxt nw lst mtext txt spc flg nlst skip)
  (vl-load-com)
  (if (and action (member action '("Upper" "Lower" "Invert" "Sentence" "Title" "SenOnly")))
    (progn
       (setq txt (vla-get-textstring obj))
       (setq mtext (= (vla-get-objectname obj) "AcDbMText")) ; flag mtext
       (setq nlst nil
             skip 0)
       
       (cond
         ;;  mtext takes special handeling
         (mtext (setq newtxt (Mtext_Ignore txt "*" action)))
         ;;-------------------------------------------------
         ((= action "Upper") (setq newtxt (strcase txt)))
         ;;-------------------------------------------------
         ((= action "Lower") (setq newtxt (strcase txt t)))
         ;;-------------------------------------------------
         ((= action "Title")
          (setq lst  (vl-string->list txt)
                spc  t   ; space char flag
          )
          (foreach letter lst
            (cond
              ((> skip 0) (setq skip (1- skip)))
              ((and (= letter 37) (zerop skip) (wcmatch txt "*%%*"))
               (setq skip 2))
              ((and spc (/= letter 32))
               (setq spc nil)
               (and (< 96 letter 123) (setq letter (- letter 32))) ; to upper
              )
              ((= letter 32) (setq spc t))
              ((< 64 letter 91) (setq letter (+ letter 32))) ; to lower
            )
            (setq nlst (cons letter nlst))
          )
          (setq newtxt (vl-list->string (reverse nlst)))
         )
         ;;-------------------------------------------------
         ((= action "Invert")
          (setq lst  (vl-string->list txt))
          (foreach letter lst
            (cond
              ((< 96 letter 123) (setq letter (- letter 32)))
              ((< 64 letter 91) (setq letter (+ letter 32)))
            )
            (setq nlst (cons letter nlst))
          )
          (setq newtxt (vl-list->string (reverse nlst)))
         )
         ;;-------------------------------------------------
         ((vl-position action '("Sentence" "SenOnly"))
          (setq lst  (vl-string->list txt)
                flg  t   ; end of sentence flag
          )
          (foreach letter lst
            (cond
              ((> skip 0) (setq skip (1- skip)))
              ((and (= letter 37) (zerop skip) (wcmatch txt "*%%*"))
               (setq skip 2))
              ((and flg (/= letter 32))
               (setq flg nil)
               (and (< 96 letter 123) (setq letter (- letter 32))) ; to upper
              )
              ((= letter 46) (setq flg t))
              ((= action "Sentence")
               (and (< 64 letter 91) (setq letter (+ letter 32))) ; to Lower
              )
            )
            (setq nlst (cons letter nlst))
          )
          (setq newtxt (vl-list->string (reverse nlst)))
         )
       )  ; end cond stmt
       ;;========================================
       
       ;;  update the text
       (setq err (vl-catch-all-apply 'vla-put-textstring (list obj newtxt)))
       (if (vl-catch-all-error-p err)
         (alert (vl-catch-all-error-message err))
       )
       (vlax-release-object obj)
    )                         ; progn
  )                           ; endif
  (princ)
) ; end defun


(prompt "\n CASE - changes case of letters in selected lines of text.")
(princ)




;;;=======================[ mText_Ignore.lsp ]=============================
;;; Author:  Charles Alan Butler Copyright© 2008  CAB - TheSwamp.org
;;; Version: 1.0 Version 1.0
;;; Purpose: Ignore format characters from text or mtext string
;;; Returns: A string  
;;; Sub Routines: -None
;;; Arguments: str - A string variable to ignore formats from
;;;            fmt - Flag string of formats to ignore
;;;            mode - Case mode flag
;;; Format Flag:
;;;   *    Ignore All Formats found
;;; Note that this is a modified version of my Strip_Mtext lisp
;;;======================================================================

(defun Mtext_Ignore (str fmt mode / skipcnt ndx newlst char fmtcode lst_len
                   IS_MTEXT LST  NEXTCHR PT TMP spc flg i)

  (if (or (/= (type fmt) 'Str) (= fmt "*") (= fmt ""))
          (setq fmt (vl-string->list "AaCcFfHhLlOoPpQqSsTtQqWw~%"))
          (setq fmt (vl-string->list fmt))
  )
  
  (setq ndx  0
        fmtcode  ; "fmtcode" is a list of code flags that will end with ; 
         (vl-string->list "CcFfHhTQqWwAap") ;("\C" "\F" "\H" "\T" "\Q" "\W" "\A" "\p")
  )
  (if (/= str "") ; skip if empty text ""
    (progn
      (setq spc  t)   ; space char flag
      (setq flg  t)
      (setq lst      (vl-string->list str)
            lst_len  (length lst)
            newlst   nil ; list of ascii codes for revised text
            is_mtext nil ; true if mtext
      )
      (while (< ndx lst_len)
        ;; step through text and find FORMAT CHARACTERS
        (setq char    (nth ndx lst) ; Get next character
              nextchr (nth (1+ ndx) lst)
              skipcnt 0
        )

        (cond
          ((and (= char 123) (= nextchr 92)) ; "{\" mtext code
           (setq is_mtext t
                 skipcnt 1
           )
          )

          ((and (= char 125) is_mtext) ; "}"
           (setq skipcnt 1)
          )

          ((= char 37) ; code start with "%"
           (if (null nextchr) ; true if % is last char in text
             (setq skipcnt 1)
             ;;  Dtext codes
             (if (= nextchr 37) ; %% code found 
               (if (< 47 (nth (+ ndx 2) lst) 58) ; is a number
                 (if (vl-position 37 fmt)
                   ;;  number found so fmtcode %%nnn
                   ;;  convert the nnn to a character
                   (setq skipcnt 5)
                   ;;  keep the code in the string
                   (setq skipcnt 5)
                 )
                 
                 ;; else letter code, so fmtcode %%p, %%d, %%c
                 ;;  CAB note - this code does not always exist in the string
                 ;;  it is used to create the character but the actual ascii code
                 ;;  is used in the string, not the case for %%c
                 (if (vl-position 37 fmt)
                   (setq skipcnt 3)
                   (setq skipcnt 3)
                 )
               ) ; endif
             ) ; endif
           ) ; endif
          ) ; end cond (= char "%"))


          ((= char 92) ; code start with "\" 
           ;;  This section processes mtext codes

           (cond
             ;; Process Coded information
             ((null nextchr) ; true if \ is last char in text
              (setq skipcnt 1)
             ) ; end cond 1

             ((member nextchr fmtcode) ; this code will end with ";"
              ;; fmtcode -> ("\C" "\F" "\H" "\T" "\Q" "\W" "\A" "\p"))
              (while (/= (setq char (nth (+ skipcnt ndx) lst)) 59)
                (setq skipcnt (1+ skipcnt))
              )
              (setq skipcnt (1+ skipcnt))
             ) ; end cond 

             ;; found \U then get 7 character group
             ((= nextchr 85) (setq skipcnt (+ skipcnt 7)))

             ;; found \M then get 8 character group
             ((= nextchr 77) (setq skipcnt (+ skipcnt 8)))

             ;; found \P  CR LF
             ((= nextchr 80) ; "\P"
              (if (vl-position 80 fmt)
                (setq skipcnt 2
                      spc t)
              )
             ) ; end cond 


             ((= nextchr 123) ; "\{" normal brace
              (setq ndx (+ ndx 1))
             ) ; end cond 

             ((= nextchr 125) ; "\}" normal brace
              (setq ndx (+ ndx 1))
             ) ; end cond 

             ((= nextchr 126) ; "\~" non breaking space
              (if (vl-position 126 fmt)
                (setq skipcnt 2) ; end cond 9
              )
             )

             ;; 2 character group \L \l \O \o
            ((member nextchr '(76 108 79 111)) 
              (setq skipcnt 2)
             ) ; end cond 

             ;;  Stacked text format as "[ top_txt / bot_txt ]"
             ((= nextchr 83) ; "\S"
              (setq pt  (1+ ndx)
                    tmp '()
              )
              (while
                (not
                  (member
                    (setq tmp (nth (setq pt (1+ pt)) lst))
                    '(94 47 35) ; "^" "/" "#" seperator
                  )
                )
                 ; CAB (setq newlst (append newlst (list tmp)))
              )
              ; CAB (setq newlst (append newlst '(47))) ; "/"
              (while (/= (setq tmp (nth (setq pt (1+ pt)) lst)) 59) ; ";"
                ; CAB (setq newlst (append newlst (list tmp)))
              )
              (setq ndx     pt
                    skipcnt (1+ skipcnt)
              )
             ) ; end cond 
           ) ; end cond stmt  Process Coded information
          ) ; end cond  (or (= char "\\")

        ) ; end cond stmt
        ;;  Skip format code characters
        (if (zerop skipcnt) ; Not a format char, modify for Case mode
          (progn
            (cond
              ((= mode "Upper")(setq char (ascii (strcase (chr char)))))
              ((= mode "Lower")(setq char (ascii (strcase (chr char) t))))
              ((= mode "Invert")
                (cond
                  ((< 96 char 123) (setq char (- char 32)))
                  ((< 64 char 91) (setq char (+ char 32)))
                )
               )
              
              ((vl-position mode '("Sentence" "SenOnly"))
                (cond
                  ((and flg (/= char 32))
                   (setq flg nil)
                   (and (< 96 char 123) (setq char (- char 32))) ; to upper
                  )
                  ((= char 46) (setq flg t))
                  ((= mode "Sentence")
                   (and (< 64 char 91) (setq char (+ char 32))) ; to Lower
                  )
                )
               )

              ((= mode "Title")
                (cond
                  ((and spc (/= char 32))
                   (setq spc nil)
                   (and (< 96 char 123) (setq char (- char 32))) ; to upper
                  )
                  ((= char 32) (setq spc t))
                  ((< 64 char 91) (setq char (+ char 32))) ; to lower
                )

               )
            )
            (setq newlst (append newlst (list char))
                  ndx    (+ ndx 1)
            )
          )
          ;;  else include format characters
          (progn
            (setq i -1)
            (while (< (setq i (1+ i)) skipcnt)
              (setq newlst (append newlst (list (nth (+ ndx i) lst))))
            )
            (setq ndx (+ ndx skipcnt))
          )
        )

      ) ; end while Loop
    ) ; end progn
  ) ; endif
  (vl-list->string newlst) ; return the stripped string
) ; end defun
;;;======================================================================

;|
"\\pxqr;right justified"

When changed to upper case the p becomes a linefeed and the rest of the formatting becomes part of the text.

Here's another example where the first line is left justified and the second line right justified.

"\\pxql;left justified\\P\\pqr;right justified"

Using 2008, this type of embedded formatting occurs when using the icons in the mtext editor toolbar.
|;