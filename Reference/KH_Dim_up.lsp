(defun c:kh()  
  (if (= scale nil) (setq scale (getreal "\nInput current scale : ")))
  (setq SS (ssget '((0 . "dimension"))))   
  (prompt "\nChon kich thuoc chuan:")
  (setq y0 (nth 2 (assoc 10 (entget (ssname (ssget) 0)))))
  (setq osm (getvar "osmode"))
  (setq     
      n (sslength ss) 
      i 0
      ssn '()
      y '()
  );setq
  (While (< i n)
    (setq 
       ssn (append (list (ssname SS i)) ssn)
       y (append (list (nth 2 (assoc 10 (entget (ssname SS i))))) y)
       i (+ i 1)
    );setq
  );While  
  ;------------------------------------
  ;------------------------------------
  (setq ys y ssns ssn n (length y))   
  (setq i 0) 
  (while (< i n)    
    (setq j (+ i 1))
    (while (< j n)
      (if (< (nth i ys) (nth j ys))    
        (progn          
          (setq 
             yi (list (nth i ys))
             ssni (list (nth i ssns))
             yj (list (nth j ys))
             ssnj (list (nth j ssns))  
          )
          ;---------------------------------- 
          (setq k 0 y1 '() ssn1 '())         
          (while (< k i)
            (setq y1 (append y1 (list (nth k ys)))
                  ssn1 (append ssn1 (list (nth k ssns)))
                  k (+ k 1)
            )
          );while
          ;----------------------------------       
          (setq k (+ i 1) y2 '() ssn2 '())
          (while (< k j)
            (setq y2 (append y2 (list (nth k ys)))
                  ssn2 (append ssn2 (list (nth k ssns)))
                  k (+ k 1)
            )
          );while
          ;--------------------------------------
          (setq k (+ j 1) y3 '() ssn3 '())
          (while (< k n)
            (setq 
               y3 (append y3 (list (nth k ys)))
               ssn3 (append ssn3 (list (nth k ssns)))
               k (+ k 1)
            )
          );while
          ;----------------------------------------
          (setq ys (append y1 yj y2 yi y3))  
          (setq ssns (append ssn1 ssnj ssn2 ssni ssn3))         
         );progn  
       );if
       (setq j (+ j 1))     
     );while
     (setq i (+ i 1)) 
   );while   
  ;---------------------------------------
  ;---------------------------------------    
  (if (= y0 (car ys)) 
    (progn    
     (setq do (* (- 4.0) scale))
     (setq dc (* (- 3.5) scale))
    )
    (progn
     (Setq do (* 4.0 scale))
     (setq dc (* 3.5 scale))
     (setq ys (reverse ys))
     (setq ssns (reverse ssns))
    )
  );if
  (setq yd (- y0 dc))    
 ;----------------------------------------  
 ;----------------------------------------  
  (setq k 0)
  (setq i 0) 
  (While (< i n)     
    (setq    
      dy (* k do) 
      ym (+ y0 dy)      
      name (nth i ssns)
      ent (entget name)
      x1 (nth 1 (assoc 10 ent)) 
      x3 (nth 1 (assoc 13 ent)) 
      x4 (nth 1 (assoc 14 ent))  
      y1 (nth 2 (assoc 10 ent))
      y3 (nth 2 (assoc 13 ent))
      y4 (nth 2 (assoc 14 ent))   
      z1 (nth 3 (assoc 10 ent))   
      z3 (nth 3 (assoc 13 ent))   
      z4 (nth 3 (assoc 14 ent))   
      E (subst (list 10 x1 ym z1) (assoc 10 ent) ent)
      E (subst (list 13 x3 yd z3) (assoc 13 E) E)
      E (subst (list 14 x4 yd z4) (assoc 14 E) E)
    )
    (entmod E)  
    (if (and (< (+ i 1) n) (> (abs (- (nth i ys) (nth (+ i 1) ys))) (abs (/ do 3))))
       (setq k (+ k 1))
    )         
    (setq i (+ i 1))  
  );While
  (setvar "osmode" osm)
(princ)
);defun
;----------------------------------------------------------------------------
;----------------------------------------------------------------------
(defun c:kv()  
  (if (= scale nil) (setq scale (getreal "\nInput current scale : ")))
  (setq SS (ssget '((0 . "dimension"))))   
  (prompt "\nChon kich thuoc chuan:")
  (setq x0 (nth 1 (assoc 10 (entget (ssname (ssget) 0)))))
  (setq osm (getvar "osmode"))
  (setq     
      n (sslength ss) 
      i 0
      ssn '()
      x '()
  );setq
  (While (< i n)
    (setq 
       ssn (append (list (ssname SS i)) ssn)
       x (append (list (nth 1 (assoc 10 (entget (ssname SS i))))) x)
       i (+ i 1)
    );setq
  );While  
  ;------------------------------------
  ;------------------------------------
  (setq xs x ssns ssn n (length x))   
  (setq i 0) 
  (while (< i n)    
    (setq j (+ i 1))
    (while (< j n)
      (if (< (nth i xs) (nth j xs))    
        (progn          
          (setq 
             xi (list (nth i xs))
             ssni (list (nth i ssns))
             xj (list (nth j xs))
             ssnj (list (nth j ssns))  
          )
          ;---------------------------------- 
          (setq k 0 x1 '() ssn1 '())         
          (while (< k i)
            (setq x1 (append x1 (list (nth k xs)))
                  ssn1 (append ssn1 (list (nth k ssns)))
                  k (+ k 1)
            )
          );while
          ;----------------------------------       
          (setq k (+ i 1) x2 '() ssn2 '())
          (while (< k j)
            (setq x2 (append x2 (list (nth k xs)))
                  ssn2 (append ssn2 (list (nth k ssns)))
                  k (+ k 1)
            )
          );while
          ;--------------------------------------
          (setq k (+ j 1) x3 '() ssn3 '())
          (while (< k n)
            (setq 
               x3 (append x3 (list (nth k xs)))
               ssn3 (append ssn3 (list (nth k ssns)))
               k (+ k 1)
            )
          );while
          ;----------------------------------------
          (setq xs (append x1 xj x2 xi x3))  
          (setq ssns (append ssn1 ssnj ssn2 ssni ssn3))         
         );progn  
       );if
       (setq j (+ j 1))     
     );while
     (setq i (+ i 1)) 
   );while   
  ;---------------------------------------
  ;---------------------------------------    
  (if (= x0 (car xs)) 
    (progn    
     (setq do (* (- 4.0) scale))
     (setq dc (* (- 3.5) scale))
    )
    (progn
     (Setq do (* 4.0 scale))
     (setq dc (* 3.5 scale))
     (setq xs (reverse xs))
     (setq ssns (reverse ssns))
    )
  );if
  (setq xd (- x0 dc))    
 ;----------------------------------------  
 ;----------------------------------------  
  (setq k 0)
  (setq i 0) 
  (While (< i n)     
    (setq    
      dx (* k do) 
      xm (+ x0 dx)      
      name (nth i ssns)
      ent (entget name)
      x1 (nth 1 (assoc 10 ent)) 
      x3 (nth 1 (assoc 13 ent)) 
      x4 (nth 1 (assoc 14 ent))  
      y1 (nth 2 (assoc 10 ent))
      y3 (nth 2 (assoc 13 ent))
      y4 (nth 2 (assoc 14 ent))   
      z1 (nth 3 (assoc 10 ent))   
      z3 (nth 3 (assoc 13 ent))   
      z4 (nth 3 (assoc 14 ent))        
      E (subst (list 10 xm y1 z1) (assoc 10 ent) ent)
      E (subst (list 13 xd y3 z3) (assoc 13 E) E)
      E (subst (list 14 xd y4 z4) (assoc 14 E) E)
    )
    (entmod E) 
    (if (and (< (+ i 1) n) (> (abs (- (nth i xs) (nth (+ i 1) xs))) (abs (/ do 3))))
       (setq k (+ k 1))
    )          
    (setq i (+ i 1))  
  );While
  (setvar "osmode" osm)
(princ)
);defun
;----------------------------------------------------------------------------
;----------------------------------------------------------------------
