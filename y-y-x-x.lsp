(defun c:yyy ()
  (if (setq objList (ssget "Select object:"))
      (if (> (sslength objList) 2)
        (progn
          (setq ymin 1e6 ymax -1e6 objCoords '() n (sslength objList)) 
          (while (> (sslength objList) 0)
            (setq obj (ssname objList 0)) 
            (setq entData (entget obj)) 
            (if (= (cdr (assoc 0 entData)) "MULTILEADER") 
              (setq key 12)
              (setq key 10)
            )
            (setq y (cadr (cdr (assoc key entData)))) 
            (setq ymin (min y ymin)) 
            (setq ymax (max y ymax)) 
            (setq objCoords (cons obj objCoords))     
            (setq objList (ssdel (ssname objList 0) objList))
          )
          (setq intervalY (/ (- ymax ymin) (1- n))
                newY ymin)
          (foreach obj (reverse objCoords)
            (setq entData (entget obj))
            (if (= (cdr (assoc 0 entData)) "MULTILEADER") 
              (setq key 12)
              (setq key 10)
            )
            (entmod (subst (cons key (list (car (cdr (assoc key entData))) newY)) (assoc key entData) entData))
            (setq newY (+ newY intervalY)) 
          )
          (princ "\nОбъекты успешно распределены."))
        (prompt "\nВыбрано недостаточно объектов."))
    (prompt "\nНе выбраны объекты."))
  (princ)
)

(defun c:xxx ()
  (if (setq objList (ssget "Select object:"))
      (if (> (sslength objList) 2)
        (progn
          (setq xmin 1e6 xmax -1e6 objCoords '() n (sslength objList)) 
          (while (> (sslength objList) 0)
            (setq obj (ssname objList 0)) 
            (setq entData (entget obj))
            (if (= (cdr (assoc 0 entData)) "MULTILEADER") 
              (setq key 12)
              (setq key 10)
            ) 
            (setq x (car (cdr (assoc key entData)))) 
            (setq xmin (min x xmin)) 
            (setq xmax (max x xmax)) 
            (setq objCoords (cons obj objCoords))     
            (setq objList (ssdel (ssname objList 0) objList))
          )
          (setq intervalX (/ (- xmax xmin) (1- n))
                newX xmin)
          (foreach obj objCoords
            (setq entData (entget obj))
            (if (= (cdr (assoc 0 entData)) "MULTILEADER") 
              (setq key 12)
              (setq key 10)
            )
            (entmod (subst (cons key (list newX (caddr (assoc key entData)))) (assoc key entData) entData))
            (setq newX (+ newX intervalX)) 
          )
          (princ "\nОбъекты успешно распределены."))
        (prompt "\nВыбрано недостаточно объектов."))
    (prompt "\nНе выбраны объекты."))
  (princ)
)

(defun c:YY ( / ss1 ent1 yCoord)
  (if (setq ss1 (ssget "Select objects to align Y:"))
    (progn
      (setq yCoord (cadr (getpoint "\nSet align Y point: ")))
      (while (> (sslength ss1) 0)
        (setq ent1 (entget (ssname ss1 0)))
        (cond
          ((= (cdr (assoc 0 ent1)) "MULTILEADER")
           (entmod (subst (cons 12 (list (car (cdr (assoc 12 ent1))) yCoord))
                          (assoc 12 ent1) ent1)))
          ((= (cdr (assoc 0 ent1)) "DIMENSION")
           (if (> (cdr (assoc 70 ent1)) 50)
            (entmod (subst (cons 11 (list (car (cdr (assoc 11 ent1))) yCoord))
                           (assoc 11 ent1) ent1))
            (entmod (subst (cons 10 (list (car (cdr (assoc 10 ent1))) yCoord))
                           (assoc 10 ent1) ent1))
           ))
          (t (entmod (subst (cons 10 (list (car (cdr (assoc 10 ent1))) yCoord))
                          (assoc 10 ent1) ent1)))
        )                   
        (setq ss1 (ssdel (ssname ss1 0) ss1)))
     ) (prompt "\nNo objects selected.")
  ) (princ) )

(defun c:XX ( / ss2 ent2 xCoord)
  (if (setq ss2 (ssget "Select objects to align X:"))
    (progn
      (setq xCoord (car (getpoint "\nSet align X point: ")))
      (while (> (sslength ss2) 0)
        (setq ent2 (entget (ssname ss2 0)))
        (cond
          ((= (cdr (assoc 0 ent2)) "MULTILEADER")
           (entmod (subst (cons 12 (list xCoord (caddr (assoc 12 ent2)))) 
                         (assoc 12 ent2) ent2)))
          ((= (cdr (assoc 0 ent2)) "DIMENSION")
           (if (> (cdr (assoc 70 ent2)) 50)
            (entmod (subst (cons 11 (list xCoord (caddr (assoc 11 ent2)))) 
                           (assoc 11 ent2) ent2))
            (entmod (subst (cons 10 (list xCoord (caddr (assoc 10 ent2)))) 
                           (assoc 10 ent2) ent2))
           ))
          (t (entmod (subst (cons 10 (list xCoord (caddr (assoc 10 ent2)))) 
                         (assoc 10 ent2) ent2))) 
        ) 
        (setq ss2 (ssdel (ssname ss2 0) ss2)))
     ) (prompt "\nNo objects selected.")
  ) (princ) )




