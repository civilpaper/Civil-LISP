;;AB.LSP    creates an anonymous block from user
;;          chosen entities

;;Written by Brian Debelius, NAI Technologies Inc., Systems Division
;;Date  1995/11/14
;;      1995/04/04  simplified code

(defun c:AB ()

    ;prompt user to select objects
    (setq sset (ssget))

    ;prompt user for insertion point
    (setq pt (getpoint "\nPick insertion point"))

    ;write block header
    (entmake
        (list   '(0 . "BLOCK")
                '(2 . "*anon")
                '(70 . 1)
                (cons '10 pt)
        )
    )

    ;add entities in selection set to block
    ;repeat for every entity in the selection set
    (setq a 0)
    (repeat (sslength sset)
        (progn

            (entmake (entget (setq ent (ssname sset a))))

            ;if polyline or block reference with attributes,
            ;walk down sub-entities until seqend is found

            (if (assoc 66 (entget ent))
                (progn

                    ;add sub-entities until seqend is found
                    (setq subent (entnext ent))
                    (while (/= (cdr (assoc 0 (entget subent))) "SEQEND")
                        (entmake (entget subent))
                        (setq subent (entnext subent))
                    )

                    ;add seqend sub-entity
                    (entmake (entget subent))
                )
            )

            ;delete original entity
            (entdel ent)
        )

    (setq a (1+ a))
    )

    ;write block end sub-entity
    (setq name (entmake '( (0 . "endblk"))) )

    ;insert block reference at insertion point
    (entmake
        (list   '(0 . "INSERT")
                (cons '2 name)
                (cons '10 pt)
        )
    )

(setq sset nil)
(princ)
)