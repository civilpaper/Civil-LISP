(PROMpT "\n TD1 by CHIEN HUY " ) 
(PROMpT "\n DL: to drawing the main line " ) 
(PROMpT "\n CG: to drawing the summit curve and valley curve" ) 
(PROMpT "\n hh: to change datum " ) 
(PROMpT "\n ll: to change basic station" ) 
;------------------------------------------------------------------
(defun C:tl2 ()
(initget (+ 1  2  4))
(print)
(setq tlngang (getreal "\nTY LE NGANG : "))
(setq tldung (getreal  "\nTY LE DUNG : "))
(setq xm (/ 1000 tlngang))
(setq ym (/ 1000 tldung))
(setq xym (/ xm ym))
)
(defun C:hhh ()
  (setq oslast (getvar "OSMODE"))
  (setq lalast (getvar "clayer"))
  (setvar "OSMODE" 1)
  (setq pt (getpoint "\nDiem chuan <end of> : "))
  (setq yc (cadr pt))
  (setq en1 (car (entsel "Chon so cao do: ")))
      (setq elst1 (entget en1))
      (setq chso1 (cdr (assoc 1 elst1)))
      (setq hc (atof (substr chso1 1)))


  (setq dy (- (* hc ym) yc))
  (print)
  (setvar "osmode" oslast)
 )

(defun C:ll ()
  (setq oslast (getvar "OSMODE"))
  (setq lalast (getvar "clayer"))
  (setvar "OSMODE" 1)
  (setq pt (getpoint "\nDiem chon LT chuan <end of> : "))
  (setq lll (car pt))
  (setq l0  (* lll -1))
  (setq ltmin (getreal "\n Ly trinh  : "))

  (print)
  (setvar "osmode" oslast)
 )

(defun C:opf ()
(setq fn (getfiled "Input data file" "" "td" 2))
(print)
 ) 
  
(defun C:dl ()
  (if (= xm nil)
   (progn
   (setq tlngang (getreal  "\nTY LE NGANG : "))
   (setq tldung (getreal  "\nTY LE DUNG : "))
   (setq xm (/ 1000 tlngang))
   (setq ym (/ 1000 tldung))
   (setq xym (/ xm ym))
   )
  )
  (setvar "OSMODE" 0)
  (setq oslast (getvar "OSMODE"))
  (setq lalast (getvar "clayer"))
  (print)
 (if (= dy nil) (progn
  (setvar "OSMODE" 1)
  (setq pt (getpoint "\nDiem chuan <end of> : "))
  (setq yc (cadr pt))

   (setq en1 (car (entsel "Chon so cao do: ")))
   (setq elst1 (entget en1))
   (setq chso1 (cdr (assoc 1 elst1)))
   (setq hc (atof (substr chso1 1)))

 
  (setq dy (- (* hc ym) yc))
  (print)  
  (setvar "osmode" oslast)
  )
 )    
 ;;(setq hve (- 125 (* hmin ym) ))
  (setvar "OSMODE" 33)
  (setq pt (getpoint "\nTu diem : "))
  (setq x1 (car pt) y (cadr pt))
  (initget "H h D d")
  (setq s (getkword "/H/Dh/<none> : "))
  (setq y1 y)
  (print)
  (if (= s "H") (progn
    (initget 1)
    (setq h (getreal "Cao do : "))
    (setq y1 (- (* h ym) dy))
    )
  )
  (if (= s "D") (progn
    (initget 1)
    (setq dh (getreal "Chenh cao : "))
    (setq y1 (+ (* dh ym) y))
    )
  )
  (setq pt1 (list x1 y1))
  (setq yii (/ (+ y1 dy) ym))
  (command "layer" "s" "7" "")
  (setvar "osmode" 0)
  (command "text" "r" (list  x1  55) 1.6 90 (rtos yii 2 2))
 ;; (command "line" (list x1 56) (list x1 64) "")
 ;; (command "line" (list x1 66) (list x1 y1) "")
  (print)
  (print)
  (setq i (getreal "\n/Do doc,% /<none> : "))
  (if (not (numberp i)) (setq i 1000))
  (print)
  (print)
  (setvar "osmode" 1)
  (setq pt (getpoint "\nDen diem : "))
  (while (/= pt nil)
   (setq x2 (car pt) y (cadr pt))
    (if (< i 999) (setq y2 (+ y1 (/ (* (- x2 x1) (/ i 100)) xym))))
    (if (> i 999) (progn
      (initget "H h D d")
      (setq s (getkword "/H/Dh/<none> : "))
      (setq y2 y)
      (print)
      (if (= s "H") (progn
        (initget 1)
        (setq h (getreal "Cao do : "))
        (setq y2 (- (* h ym) dy))
        )
      )
      (if (= s "D") (progn
        (initget 1)
        (setq dh (getreal "Chenh cao : "))
        (setq y2 (+ (* dh ym) y))
        )
      )
      )
    )
    (setq pt2 (list x2 y2))
    (if (< x2 lmax) (progn
      (setq i (getreal "\n/Do doc,% /<none> : "))
      (if (not (numberp i)) (setq i 1000))
      )
    )
    (command "layer" "s" 1 "")
    (setvar "osmode" 0)
    (command "line" pt1 pt2 "")
    (setq yii (/ (+ y2 dy) ym))
    (command "layer" "s" "7" "")
   ;; (command "line" (list x2 56) (list x2 64) "")
    (command "text" "r" (list  x2  55) 1.6 90 (rtos yii 2 2))
   ;; (command "line" (list x2 66) (list x2 y2) "")
    (setq pt1 pt2 x1 x2 y1 y2)
    (print)
    (print)
    (setvar "osmode" 1)
    (setq pt (getpoint "\nDen diem : "))
  )
  (setvar "OSMODE" oslast)
  (setvar "layer" lalast)
  )


    (defun C:cg ()
  (if (= xm nil)
   (progn
   (setq tlngang (getreal  "\nTY LE NGANG : "))
   (setq tldung (getreal  "\nTY LE DUNG : "))
   (setq xm (/ 1000 tlngang))
   (setq ym (/ 1000 tldung))
   (setq xym (/ xm ym))
   )
  )
  (setq oslast (getvar "OSMODE"))
  (setq lalast (getvar "CLAYER"))
 (if (= dy nil) (progn
  (setvar "OSMODE" 1)
  (setq pt (getpoint "\nDiem chuan <end of> : "))
  (setq yc (cadr pt))
   (setq en1 (car (entsel "Chon so cao do: ")))
      (setq elst1 (entget en1))
      (setq chso1 (cdr (assoc 1 elst1)))
      (setq hc (atof (substr chso1 1)))

  (setq dy (- (* hc ym) yc))
  (print)  
  (setvar "osmode" oslast)
  )
 ) 
   
  (if (= fn nil)
  (setq fn (getfiled "Input data file" "" "td" 2))
  
  ) 
  
  (setq f2 (open fn "r"))
  (setq s (read-line f2))
  (setq sdem (atoi (substr s 1 8)))

  (if (= ltmin  nil) (progn
  (setvar "OSMODE" 33)
  (setq q (getpoint "\Diem ly trinh chuan : ")) 
  (setq ltmin (getreal "\Ly trinh chuan : ")) 
  (setq lll (car q))
  (setq l0 (* lll -1))     
  ))
  (setq ltmax  0)
  (setq dem 0)
  (repeat sdem
  (setq s (read-line f2))
  (setq lt (atof (substr s  9 8)))
  (setq htn (atof (substr s 20 6)))
  ;;(setq dem (+ dem ))
  (if (< ltmax lt) (setq ltmax  lt))
 ;;;(if (> hmin htn) (setq hmin  htn))
  )
  (close f2)
  
  (setq lmax (- ltmax ltmin))
 ;(setq hve (- 125 (* hmin ym)))
  (setvar "OSMODE" 512)
    (command "layer" "f" 3 "")
    (command "layer" "f" 5 "")
    (command "layer" "f" 0 "")
 
  (setq pt (getpoint "\nDoan 1 : "))
  (while (/= pt nil)
    (command "layer" "s" "1" "")
    (print)
    (print)
    (setq e1 (ssget pt))
    (setq elst (entget (ssname e1 0))
          pt1 (cdr (assoc 10 elst))
          pt2 (cdr (assoc 11 elst))
    )
    (setq x1 (car pt1) x2 (car pt2))
    (if (< x2 x1) (progn
      (setq pt pt1)
      (setq pt1 pt2)
      (setq pt2 pt)
      )
    )
    (setq x1 (car pt1))
    (setq e1 (ssget (getpoint "\nDoan 2 : ")))
    (setq elst (entget (ssname e1 0))
          pt3 (cdr (assoc 10 elst))
          pt4 (cdr (assoc 11 elst))
    )
    (setvar "OSMODE" 0)
    (setq x3 (car pt3) x4 (car pt4))
    (if (< x4 x3) (progn
      (setq pt pt3)
      (setq pt3 pt4)
      (setq pt4 pt)
      )
    )
    (setq x4 (car pt4))
    (if (< x4 x1) (progn
      (setq pt pt1)
      (setq pt1 pt3)
      (setq pt3 pt)
      (setq pt pt2)
      (setq pt2 pt4)
      (setq pt4 pt)
      )
    )
    (setq x1 (car pt1) y1 (cadr pt1))
    (setq x4 (car pt4) y4 (cadr pt4))
    (setq pt0 (inters pt1 pt2 pt3 pt4 nil))
    (setq x0 (car pt0) y0 (cadr pt0))
    (setq it (/ (- y0 y1) (- x0 x1)))
    (setq is (/ (- y4 y0) (- x4 x0)))
    (setq i1 (* it xym))
    (setq i2 (* is xym))
    (setq di (- i2 i1))
    (setq t1 (- x0 x1) t2 (- x4 x0))
    (setq Tmax (/ (min t1 t2) xm))
    (setq Rmax (* (/ Tmax (abs di)) 2))
    (print)
    (print)
    (initget 1 "T t P p")
    (setq R (getreal "\nNhap /T/P/<R> : "))
    (if (or (eq R "T") (eq R "t")) (progn
      (setvar "osmode" 512)
      (setq pt (getpoint "\nDiem dau <hoac cuoi> duong cong : "))
      (setq x2 (car pt))
      (setq T (abs (/ (- x2 x0) xm)))
      (setq R (* (/ T (abs di)) 2))
      (setvar "osmode" 0)
 
      )
    )
    (if (or (eq R "P") (eq R "p")) (progn
      (setq pt (getpoint "\nDiem giua duong cong : "))
      (setq y2 (cadr pt))
      (setq P (abs (/ (- y2 y0) ym)))
      (setq R (* (/ P di) (/ 8.0 di)))
      )
    )
    (if (> R Rmax) (setq R Rmax))
    (setq T (abs (/ (* R di) 2.0)))
    (setq p1 (* (* R di) (/ di 8.0)))
  ;;  (command "layer" "f" 1 "")
  ;;  (command "layer" "f" 2 "")
  ;;  (command "layer" "f" 0 "")
    (setvar  "osmode" 0)
    (command "erase" "c" (list (+ x1 1) 48) (list (- x4 1) 290) "")
   ;; (command "erase" "c" (list (+ x1 1) 60) (list (- x4 1) 56) "")
   ;; (command "erase" "c" (list (+ x1 1) 56) (list (- x4 1) 64) "")
    ;;(command "layer" "t" 1 "")
    ;;(command "layer" "t" 2 "")
    ;;(command "layer" "t" 0 "")
    (setq t0 (* T xm))
    (setq x2 (- x0 t0))
    (setq x21 (- x2 1))
    (setq x3 (+ x0 t0))
    (setq x31 (+ x3 1))
    (setq y2 (- y0 (* t0 it)))
    (setq y3 (+ y0 (* t0 is)))
    (setq pt2 (list x2 y2) pt3 (list x3 y3))
    (command "line" pt1 pt2 "")
    (command "line" pt3 pt4 "")
    (command "color" "5")
    (command "line" pt2 pt0 "")
    (command "line" pt0 pt3 "")
    (command "color" "bylayer")
    (setq xx x2 yy y2)
    (setq yin (/ (+ y2 dy) ym))
    (repeat 2
      (command "layer" "s" "7" "")
      (command "line" (list xx 56) (list xx 64) "")
      (command "line" (list xx 66) (list xx yy) "")
      (command "text" "r" (list  xx  55) 1.6 90 (rtos yin 2 2))
      (command "layer" "s" 1 "")
      (setq xx x3 yy y3)
      (setq yin (/ (+ y3 dy) ym))
    )
    (setq y0in (/ (+ y0  dy) ym))
        (if (< di 0)
    (setq p1 (* -1.0  p1))
        )

    (setq y0in1 (+ y0in p1))
    

    (setq ll (+ ltmin (/ (+ x0 l0) xm))) 
    
    (setq ll1 (strcat "L.T : " (rtos ll 2  2)))
    (setq yp (- (* y0in1 ym) dy))
    (if (> R 0)
    (command "arc" pt2  (list x0 yp) pt3)    
    ) 
    (command "layer" "s" "7" "")
  ;; (command "text" (list (- x0 1.5) (+ y0 3)) 1.6 90  (rtos (- y0in1 y0in))) 
  ;; (command "text" "r" (list (- x0 0.5) 72) 1.6 90 (rtos y0in 2  2)) 
  ;; (command "text" "r" (list x0  55) 1.6 90 (rtos y0in1 2 2))
  ;; (command "text" (list (- x0 0.5) 75) 1.6 90 (rtos p1 2 2))
  ;; (command "text" (list (- x0 0.5) 84) 1.6 90 (rtos y0in1 2 2))

    (command "polygon" "3" pt0 "c" "1") 
    (command "circle" pt0 "0.6") 
    (setq xd x1 xc x2 yd y1 yc y2 i it i0 i1)
    (repeat 2
      (setq ii (* i0 100.0))
      (setq ld (+ ltmin (/ (+ xd l0) xm)) lc (+ ltmin (/ (+ xc l0) xm)))
      (print ld)
      (print lc)
      (setq ld1 (- ld 0.01) ld2 (+ ld 2.5))
      (setq lc1 (- lc 2.5) lc2 (+ lc 0.01))
      (setq b 0)
      (setq f (open fn "r"))
      (setq s (read-line f))
        (while (= b 0)
        (setq s (read-line f))
        (setq lt (atof (substr s 9 8)))
        (if (> lt ld1) (progn
          (if (< lt lc2) (progn
            (setq xv (- (* (- lt ltmin) xm) l0))
            (setq ytkv (+ yd (* (- xv xd) i)))
            (setq ytk (/ (+ ytkv dy) ym))
            (setq sy (substr s 20 8))
            (setq ydap (- ytk (atof sy)))
            (setq ydapin (rtos (abs ydap) 2 2))
            (setq xv1 (+ xv 0.5))
            (setq xv2 (- xv 0.5))
            (command "text" "r" (list xv 55) 1.6 90 (rtos  ytk  2 2))
            (if (>= ydap 0) (command "text" (list xv2 (+ ytkv 3)) 1.6 90  ydapin))
            (if (< ydap 0) (command "text" "r" (list xv2 (- ytkv 3)) 1.6 90 ydapin))
            )
          )
          (if (>= lt lc) (setq b 1))
          )
        )
      )
      (close f)
      (setq iii (strcat (rtos (abs ii) 2 2) "%"))
      (setq lx (/ (- xc xd) xm))
      (setq lxi (rtos lx 2 2))
      (if (> lx 9) (progn
        (setq xtg (/ (+ xd xc) 2.0))
        (if (>= ii 0.01) (progn
          (command "line" (list xd 56) (list xc 64) "")
          (command "text" (list (+ xd 3) 60) 1.6 0 lxi)
          (command "text" "r" (list (- xc 3) 58) 1.6 0 iii)
          )
        )
        (if (<= ii -0.01) (progn
          (command "line" (list xd 64) (list xc 56) "")
          (command "text" (list (+ xd 3) 58) 1.6 0 lxi)
          (command "text" "r" (list (- xc 3) 60) 1.6 0 iii)
          )
        )
        (if (and (< ii 0.01) (> ii -0.01)) (progn
          (command "line" (list xd 59) (list xc 59) "")
          (command "text" "m" (list xtg 60) 1.6 0 "0")
          (command "text" "m" (list xtg 60) 1.6 0 lxi)
          )
        )
        )
      )
      (setq xd x3 xc x4 yd y3 yc y4 i is i0 i2)
    )
     
    
     (if (> R 0) (progn
      (setq ytk2 (/ (+ y2 dy) ym))
      (setq ytk3 (/ (+ y3 dy) ym))
      (setq ld (+ ltmin (/ (+ x2 l0) xm)) lc (+ ltmin (/ (+ x3 l0) xm)))
      (setq ld1 (- ld 0.01) ld2 (+ ld 2.5))
      (setq lc1 (- lc 2.5) lc2 (+ lc 0.01))
      (setq b 0)
      (setq f (open fn "r"))
      (setq ptt pt2)
      (while (= b 0)
        (setq s (read-line f))
        (setq lt (atof (substr s 9 8)))
          (if (> lt ld1) (progn
          (if (<= lt ll) (progn
            (setq xv (- (* (- lt ltmin) xm) l0))
            (setq xt (- lt ld))
            (setq ytc (* (/ xt R) (/ xt 2.0)))
            (if (< di 0) (setq ytc (* -1 ytc)))             
             (setq ytl (+ ytk2 (* xt i1)))
            (setq ytk (+ ytl ytc))
            (setq ytkv (- (* ytk ym) dy))
           ; (command "layer" "s" 3 "")
           ; (setq pts (list xv ytkv))
           ; (command "line" ptt pts "")
            (command "layer" "s" "7" "")
            (setq sy (substr s 20 8))
            (setq ydap (- ytk (atof sy)))
            (setq ydapin (rtos (abs ydap) 2 2))
            (setq xv1 (+ xv 0.5))
            (setq xv2 (- xv 0.5))
            (command "text" "r" (list xv 55) 1.6 90 (rtos ytk  2 2))
            (command "text" "r" (list xv2 72) 1.6 90 (rtos ytl 2 2))
            (command "text" "r" (list xv2 80) 1.6 90 (rtos ytc 2 2))
            (command "text"  (list xv2 84) 1.6 90 (rtos ytk  2 2))
            

            (if (>= ydap 0) (command "text" (list xv2 (+ ytkv 3)) 1.6 90 ydapin))
            (if (< ydap 0) (command "text" "r" (list xv2 (- ytkv 3)) 1.6 90 ydapin))
            ))
            (if (and (> lt ll) (< lt lc)) (progn
            (setq xv (- (* (- lt ltmin) xm) l0))
            (setq xt (- lc lt))
            (setq ytc (* (/ xt R) (/ xt 2.0)))
            (if (< di 0) (setq ytc (* -1 ytc)))             
            (setq ytl (- ytk3 (* xt i2)))
            (setq ytk (+ ytl ytc))
            (setq ytkv (- (* ytk ym) dy))
           ; (command "layer" "s" 3 "")
           ; (setq pts (list xv ytkv))
           ; (command "line" ptt pts "")
            (command "layer" "s" "7" "")
            (setq sy (substr s 20 8))
            (setq ydap (- ytk (atof sy)))
            (setq ydapin (rtos (abs ydap) 2 2))
            (setq xv1 (+ xv 0.5))
            (setq xv2 (- xv 0.5))
            (command "text" "r" (list xv 55) 1.6 90 (rtos ytk  2 2))
            (command "text" "r" (list xv2 72) 1.6 90 (rtos ytl 2 2))
            (command "text" "r" (list xv2 80) 1.6 90 (rtos  ytc  2 2))
            (command "text"  (list xv2 84) 1.6 90 (rtos ytk 2 2))

            (if (>= ydap 0) (command "text" (list xv2 (+ ytkv 3)) 1.6 90 ydapin))
            (if (< ydap 0) (command "text" "r" (list xv2 (- ytkv 3)) 1.6 90 ydapin))
            )
            )
            (if (>= lt lc) (setq b 1))
          )
        )
      )
    (close f)
    (command "layer" "t" 3 "")
    (command "layer" "t" 5 "")
    (command "layer" "t" 0 "")
   
    (initget "C c K k")
    (setq ch (getkword "/ CO GHI KHONG? C/K : "))
    (if (= ch "C") 
      (progn
    (setq f (open fn "a"))
    (setq stghi (strcat (rtos ll 2 2) "    " (rtos y0in  2  2) "  " (rtos R 2 2) ))
    (write-line stghi f)
    (close f)
      )
    )

   ;; (command "layer" "s"  3 "")
   ;; (command "line" ptt pt3 "")
      (command "layer" "s"  "7" "")
      (setq lxi (rtos (/ (- x3 x2) xm) 2 2))
      (setq xtg (/ (+ x2 x3) 2.0))
      (setq lin (strcat "R= " (rtos R 2 0) ", L= " lxi))
      (if (> di 0.0) (progn
        (command "arc" (list x2  64) (list (* 0.5 (+ x2 x3)) 57) (list x3 64) "")
        (command "text" "m" (list xtg 61) 1.6 0 lin)
        )
      )
      (if (< di 0.0) (progn
        (command "arc" (list x2  56) (list (* 0.5 (+ x2 x3)) 63) (list x3 56) "")
        (command "text" "m" (list xtg 59) 1.6 0 lin)
        )
      )
      )
    )
    (if (= R 0) (progn
    (initget "C c K k")
    (setq ch (getkword "/ CO GHI KHONG? C/K : "))
     (if (= ch "C") 
     (progn
    (setq f (open fn "a"))
    (setq stghi (strcat  (rtos ll 2 2) "    " (rtos y0in  2  2) "  " (rtos R 2 2) ))
    (write-line stghi f)
    (close f)
      ))
    ))

    (print)
    (print)
    (setvar "OSMODE" 512)
    (command "layer" "f" 3 "")
    (command "layer" "f" 5 "")
    (command "layer" "f" 0 "")
    (setq pt (getpoint "\nDoan 1 : "))
   )

   (setvar "OSMODE" oslast)
   (setvar "CLAYER" lalast)
    (command "layer" "t" 3 "")
    (command "layer" "t" 5 "")
    (command "layer" "t" 0 "")

  )
