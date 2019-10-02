(defun C:kl ()   
  (setq osm (getvar "osmode"))
  (setq fn (getfiled "Input data file" "" "txt" 1))
  (setq f (open fn "w"))
  (setvar "cmdecho" 0)
  (setvar "osmode" 33)
  (setq p0  (getpoint "\n first point :"))
  (setq p01 (getcorner p0 "\n second point :")) 
  (setvar "osmode" 0)
  (setq cot  (getreal "\n So cot:"))
  (setq hang (getreal "\nSo hang:"))
  (princ "chon coc:")
  (setq lcoc (cdr (assoc 8 (entget (ssname (ssget) 0)))))
  (princ "\nchon dien tich:")
  (setq ldt  (cdr (assoc 8 (entget (ssname (ssget) 0)))))  
  (command "zoom" "e")
  (setq dx (- (car p01) (car p0)))
  (setq dy (- (cadr p0) (cadr p01))) 
  (setq p 0)
  (while (< p cot)
   (setq q 0)
   (while (< q hang)   
     (setq p1 (list (+ (car p0) (* p dx)) (- (cadr p0) (* q dy))))
     (setq p2 (list (+ (car p1) dx) (- (cadr p1) dy)))
     (if (setq DS (ssget "w" p1 p2 '((0 . "text"))))  
       (progn
         (setq L1 '() Lt '())
         (setq n (sslength DS))
         (setq i 0)
         (While (< i n)
           (if (= lcoc (cdr (assoc 8 (entget (ssname DS i)))))
             (setq L1 (append (list (ssname DS i)) L1))
           )
           (if (= ldt (cdr (assoc 8 (entget (ssname DS i)))))  
             (if (= T (numberp (read(cdr (assoc 1 (entget (ssname DS i)))))))        
               (setq Lt (append (list (ssname DS i)) Lt))
             )
           )
           (setq i (+ i 1))
         );while
         (setq j 0)   
         (setq L2 '() L3 '())      
         (setq m (length Lt))
           (setq x1 (nth 1 (assoc 10 (entget (car L1)))))
           (while (< j m)
             (setq x (nth 1 (assoc 10 (entget (nth j Lt)))))
             (if (< x x1)
               (setq L2 (append (list (nth j Lt)) L2))
               (setq L3 (append (list (nth j Lt)) L3))
             )
             (setq j (+ j 1))
           );while   
           (setq Lt1 (sx L1)  
                 L2 (sx L2)
                 L3 (sx L3)
           )
           (setq L1 (append (list (car Lt1)) (tach (cadr Lt1))))
           (setq L (append L2 L3)
                 L (append L1 L)
           )     
          (setq i 1)
          (setq n (length L))
          (setq text (car L))
          (while (< i n)
            (setq text (strcat text "\t" (nth i L)))      
            (setq i (+ i 1))
          ) 
          (write-line text f)   
          (write-line "" f)   
      );progn
    );if
    (setq q (+ q 1))
  );while
  (setq p (+ p 1))
 );while
(close f)  
(setvar "osmode" osm)
(princ)
);defun
;----------------------------------------------
;---------------------------------------------
(defun sx (ss)    
  (setq osm (getvar "osmode"))
  (setq     
      n (length ss) 
      i 0
      ssn '()
      y '()
  );setq
  (While (< i n)
    (setq 
       ssn (append (list (cdr (assoc 1 (entget (nth i SS))))) ssn)
       y (append (list (nth 2 (assoc 10 (entget (nth i SS))))) y)
       i (+ i 1)
    );setq
  );While  
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
   (princ ssns)       
);defun
;--------------------------------------------
;---------------------------------------------
(defun tach (ch)        
  (setq n (strlen ch) i 1)
  (while (< i n)
    (if (= T (numberp (read (substr ch i 1))))    
     (progn
      (setq so (substr ch i)
            i n
      )      
     )
    ) 
    (setq i (+ i 1))
  )
  (princ so) 
  (setq m (strlen so))
  (setq j 1)
  (while (< j m)    
    (if (= "+" (substr so j 1))      
     (progn      
      (setq s1 (strcat "Km" (substr so 1 (- j 1)) "+")
            s2 (substr so (+ j 1))
            s (list s1 s2)
      )
     )
    ) 
    (setq j (+ j 1))
  )
  (princ s)  
)
;-----------------------------------------------------------
;------------------------------------------------------------
