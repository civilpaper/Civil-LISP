(prompt "\n Taluy symbol loading... Written by NTTLe - NQCuong , Tedi South")
;---------------------------------------------------------------
(defun UDIST (bit kwd msg def bpt / inp)
  (if def
    (setq msg (strcat "\n" msg "<" (rtos def) ">: ")
          bit (* 2 (fix (/ bit 2)))
    )
    (if (= " " (substr msg (strlen msg) 1)) 
      (setq msg (strcat "\n" (substr msg 1 (1- (strlen msg))) ": "))
      (setq msg (strcat "\n" msg ": "))
    )
  )
  (initget bit kwd)
  (setq inp
    (if bpt
      (getdist msg bpt)
      (getdist msg)
    )
  )
  (if inp inp def)
)
;---------------------------------------------------------------
(defun DXF (code elist)
  (cdr (assoc code elist))
)
;---------------------------------------------------------------
(defun u_ssget ( / ssl  nsset temp ed ) ; Get LINE & POLYLINE only
  (setq sset (ssget))
  (setq ssl (sslength sset) 
        nsset (ssadd)
  )
  (print ssl) (princ "entities found. ")  
  (princ "\nVerifying the selected entities -- please wait. ")
  (while (> ssl 0)
    (progn
      (setq temp (ssname sset (setq ssl (1- ssl))))
      (setq ed (entget temp))
      (if (OR (= (DXF 0 ed) "LINE") (= (DXF 0 ed) "POLYLINE") (= (DXF 0 ed) "LWPOLYLINE") ) 
        (ssadd temp nsset)
      )
    )
  )
  (setq ssl (sslength nsset)
        sset nsset
  )
  (print ssl) (princ "LINE, POLYLINE, LWPOLYLINE entities found. ")
  (princ)
);defun u_ssget
;---------------------------------------------------------------
(defun draw ( pt1 pt2 ) // For LINE
  (setq #OSMODE (getvar "OSMODE"))
  (setvar "OSMODE" 0)
  (setq x1 (car pt1)
        y1 (cadr pt1)
        a (angle pt1 pt2)
        dist (distance pt1 pt2)
  )
  (if (< dist #DL) (setq dist #DL))
  (setq times (fix (/ dist #DL))
        dl (/ dist times)
        dx (* dl (cos a))
        dy (* dl (sin a))
  );setq

  (while (> times 0)
    (setq times (1- times)
          x (+ x1 (* times dx))
          y (+ y1 (* times dy))
          p1 (list x y)
          p2 (polar p1 (+ a (/ PI 2)) #L1)
          p3 (list (+ x (/ dx 2)) (+ y (/ dy 2)))
          p4 (polar p3 (+ a (/ PI 2)) #L2)
    );setq      
    (command "LINE" p1 p2 "")
    (command "LINE" p3 p4 "")
  );while
  (setvar "OSMODE" #OSMODE)
);defun draw
;---------------------------------------------------------------

(prompt "\nTL1: draw taluy symbol for straigth line")
(defun c:TL1( )
 (command "UCS" "W" "")
 (prompt "\nTaluy symbol...")
 ( u_ssget )
 (setq ssl (sslength sset)
       side (getstring "\nSide <L/R>")
       #L1 (abs (UDIST 0 "" "L1= " #L1 nil))
       #L2 (abs (UDIST 0 "" "L2= " #L2 nil))
       #DL (abs (UDIST 0 "" "Interval :" #DL nil))
 );setq

 (while (> ssl 0)
  (progn
    (setq temp (ssname sset (setq ssl (1- ssl))))
    (setq ed (entget temp))
;---------------
    (if (= (DXF 0 ed) "LINE")
      (progn
         (if (< (nth 0 (dxf 10 ed)) (nth 0 (dxf 11 ed)))
            (setq pt1 (list (nth 0 (dxf 10 ed)) (nth 1 (dxf 10 ed)))      
                  pt2 (list (nth 0 (dxf 11 ed)) (nth 1 (dxf 11 ed))))         
            (setq pt2 (list (nth 0 (dxf 10 ed)) (nth 1 (dxf 10 ed)))      
                  pt1 (list (nth 0 (dxf 11 ed)) (nth 1 (dxf 11 ed))))         
         )    
         (if (OR (= side "L") (= side "l"))
            (draw pt1 pt2) 
            (draw pt2 pt1)
         )
      );progn if
    );if LINE
;---------------
    (if (= (DXF 0 ed) "POLYLINE")
      (progn       
        (setq en (entnext en)
              en1 (entnext en))

        (while (AND (/= (DXF 0 (entget en1)) "SEQEND") (/= (DXF (entget en1)) 16))
          (progn
            (setq ed (entget en)
                  ed1 (entget en1))
            (if (<  (nth 1 (nth 8 ed)) (nth 1 (nth 8 ed1)))
              (setq pt (list (nth 1 (nth 8 ed)) (nth 2 (nth 8 ed)))
                    pt1 (list (nth 1 (nth 8 ed1)) (nth 2 (nth 8 ed1))))
              (setq pt1 (list (nth 1 (nth 8 ed)) (nth 2 (nth 8 ed)))
                    pt (list (nth 1 (nth 8 ed1)) (nth 2 (nth 8 ed1))))
            )
            (if (OR (= side "L") (= side "l"))
               (draw pt pt1) 
               (draw pt1 pt)
            )
            (setq en en1
                  en1 (entnext en))
          );progn while
        );while
      );progn if

    );if POLYLINE  
;---------------
    (if (= (DXF 0 ed) "LWPOLYLINE")
      (progn
         (setq no_vertex (DXF 90 ed))

         (while (> no_vertex 1)
            (progn
               (setq no_vertex (- no_vertex 1)
                     d1 (nth (+ 12 (* 4 no_vertex)) ed)   
                     d2 (nth (+ 12 (* 4 (- no_vertex 1))) ed)   
               )
               (if (< (nth 1 d1) (nth 1 d2))
                  (setq pt1 (list (nth 1 d1) (nth 2 d1))
                        pt2 (list (nth 1 d2) (nth 2 d2)))
                  (setq pt2 (list (nth 1 d1) (nth 2 d1))
                        pt1 (list (nth 1 d2) (nth 2 d2)))
               )
               (if (OR (= side "L") (= side "l"))
                  (draw pt1 pt2) 
                  (draw pt2 pt1)
               )
            );progn while
         );while
      );progn if
    );if LWPOLYLINE
;---------------
  );progn while
 );while
) ; Defun taluy()

;---------------------------------------------------------------

(prompt "\nTL2: draw taluy symbol for curved line")

(defun c:TL2 ( / pt1 pt2 cen side ) ; For CIRCLE
 (command "UCS" "W")
  (setq #OSMODE (getvar "OSMODE"))
  (setq pt1 (getpoint "\nNhap PT1 ")
        pt2 (getpoint "\nNhap PT2 ")
  )
  (setvar "OSMODE" 4)
  (SETQ cen (getpoint "\nNhap CEN "))

  (SETQ side (getint "\nNhap side (1=Huong tam) ")
        #L1 (abs (UDIST 0 "" "L1= " #L1 nil))
        #L2 (abs (UDIST 0 "" "L2= " #L2 nil))
        #DL (abs (UDIST 0 "" "Interval :" #DL nil))
  )


  (setvar "OSMODE" 0)

  (setq R (SQRT (+ (EXPT (- (car pt1) (car cen)) 2) (EXPT (- (cadr pt1) (cadr cen)) 2)))
        A (abs (- (angle cen pt1) (angle cen pt2))) 
        dist (* A R)
        times (fix (/ dist #DL))
        dA (/ A times) 
  );setq
  
  (command "UCS" "3p" cen pt1 pt2)

  (while (> times 0)
    (setq times (1- times)
          p1 (polar '(0 0) (* dA times) R)
          p3 (polar '(0 0) (+ (* dA times) (/ dA 2)) R)
          p2 (polar '(0 0) (* dA times) (if (= side 1) (- R #L1) (+ R #L1)))
          p4 (polar '(0 0) (+ (* dA times) (/ dA 2)) (if (= side 1) (- R #L2) (+ R #L2)))
    );setq      
    (command "LINE" p1 p2 "")
    (command "LINE" p3 p4 "")
  );while
  (setvar "OSMODE" #OSMODE)
  (command "UCS" "W")  

);defun TL2
