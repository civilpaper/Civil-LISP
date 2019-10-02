
;(load "mpt_sumto")
;(load "mpt_xre")
;(load "mpt_yre")

;(load "mpt_sumbv")
;(load "mpbv_xre")

(load "mp_config")
(load "mp_plot")
(load "mp_tkt")
;(load "rai9")
;(load "appendix")

(load "setup")


(defun c:reto()
  (c:mp_renum_soto_x)
  (c:mp_renum_tongto)

)

(defun c:rebv()
  (c:mp_renum_sobv_x)
  (c:mp_renum_tongbv)
)

(defun c:fr() (C:MP_FRESH))
(defun c:sbv() (C:sumbv))


(defun c:cs()
 (alert "Vui long lien he nk_long@yahoo.com - 0989 50 52 55")
)

(defun c:mplot()
  (command ".shell" "mplot")
)