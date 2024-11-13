(defun c:autodim_bo (/ point points)
	(command "_UNDO" "be")  ; - начало группы для отмены (Begin).
	(setq points nil)
	(while
		(setq point (getpoint "\nSelect a point: "))
		(setq points (cons point points))
	)
	(foreach pt (reverse points)
		(CreateBoundary pt)
	)
	; вибираємо всі контури на шарі "Defpoints", які є полілініями з жовтим кольором
	(if (setq ss (ssget "X" (list (cons 8 "Defpoints")(cons 0 "LWPOLYLINE") (cons 62 2)))) 
		(progn
			(while (setq ent (ssname ss 0)) ; беремо перший об'єкт з вибірки
				(DP ent 0)
				(entdel ent)
				(setq ss (ssdel ent ss))
			)
		)
		(print "\nnema ni huya")
	)
	(command "_UNDO" "e")
	(princ)
)
;; Функція  створення контуру по точці в центрі кімнати
(defun CreateBoundary (pt)
	;; Збереження поточного шару та кольору
	(setq current-layer (getvar "CLAYER"))
	(setq current-color (getvar "CECOLOR"))
	;; Перевірка та створення шару "defpoints"
	(if (not (tblsearch "LAYER" "defpoints"))
		(progn
			(command "._LAYER" "New" "defpoints" "")
			(command "._LAYER" "Set" "defpoints" "")
		)
	)
	;; Встановлення шару "defpoints" та кольору синій
	(setvar "CLAYER" "defpoints")
	(setvar "CECOLOR" "2")  ; Колір 5 - синій
	;; Створення контуру за допомогою команди -BO (Boundary)
	(command "-BO" pt "")
	;; Повернення попередніх налаштувань шару та кольору
	(setvar "CLAYER" current-layer)
	(setvar "CECOLOR" current-color)
)


(defun DP (ent reverse_polyline / pt1 pt2 layerName)
	(vl-load-com)
	(setq *error* (lambda (msg) 
		(if (not (wcmatch (strcase msg) "*BREAK,*CANCEL*,*EXIT*"))
			(princ (strcat "\nError: " msg))
            (princ)
		)
        (princ))
	) 
	(setq layerName "\u+0420\u+0430\u+0437\u+043c\u+0435\u+0440\u+044b") 
	(if (not (tblsearch "LAYER" layerName))
		(progn
			(setq doc (vla-get-ActiveDocument (vlax-get-acad-object)))
			(setq layers (vla-get-Layers doc))
			(setq newLayer (vla-Add layers layerName))
			(vla-put-Color newLayer 5)
			(princ (strcat "\nLayer " layerName " created."))
		)
		(princ (strcat "\nLayer " layerName " already exists."))
	)
	(if (= ent 0)
		(progn 
			(setq sel (ssget '((0 . "LWPOLYLINE"))))
			(if (not sel)
				(progn 
					(princ "\nNo polyline selected.") 
					(exit)
				)
			)
		(setq ent (ssname sel 0))
		)
	)
	;; Функція обчислення кута між двома точками
	(defun GetAngle (p1 p2)
		(atan (- (cadr p2) (cadr p1)) (- (car p2) (car p1)))
	)
	;; Функція додавання розміру
	(defun AddDim (pt1 pt2 loc) 
		(if (and (/= (car pt1) (car pt2)) (/= (cadr pt1) (cadr pt2)))
			(progn 
				(setq dim 
					(vla-AddDimAligned 
						(vla-get-ModelSpace (vla-get-ActiveDocument (vlax-get-acad-object)))
						(vlax-3d-point (car pt1) (cadr pt1) 0.0)
						(vlax-3d-point (car pt2) (cadr pt2) 0.0)
						(vlax-3d-point loc)
					)
				)
				(vla-put-Layer dim layerName)
			)
			(progn
				(if (/= (car pt1) (car pt2)) (setq orient 0) (setq orient 1.57079632679))
				(setq dim 
					(vla-AddDimRotated 
						(vla-get-ModelSpace (vla-get-ActiveDocument (vlax-get-acad-object)))
						(vlax-3d-point (car pt1) (cadr pt1) 0.0)
						(vlax-3d-point (car pt2) (cadr pt2) 0.0)
						(vlax-3d-point loc)
						orient
					)
				)
				(vla-put-Layer dim layerName)
			)
		)
	)
	;; Отримання координат полілінії
	(setq obj (vlax-ename->vla-object ent))
	(setq vertices (vlax-safearray->list (vlax-variant-value (vla-get-coordinates obj))))
	;; Перетворення списку координат у список точок
	(setq points '())
	(while vertices
		(setq points (cons (list (car vertices) (cadr vertices) 0.0) points))
		(setq vertices (cddr vertices))
	)
	(if (/= reverse_polyline 0) (setq points (reverse points))) ; реверс полілінії для дзеркальних розмірів
	;; Додавання координат першої точки в кінець для замкнутості
	(setq points (append points (list (car points))))
	;; Перебір всіх сегментів полілінії
	(setq num-verts (length points))
	(repeat (- num-verts 1)
		(setq 
			pt1 (nth (1- num-verts) points)
			pt2 (nth (- num-verts 2) points)
		)
		(if (and pt1 pt2)
			(progn
				(if (and 
						(> (GetAngle pt1 pt2) -1.57) 
						(<= (GetAngle pt1 pt2) 1.5707963267949)
					) 
					(setq offset 100) 
					(setq offset 200)
				)
				(setq 
					midpt (list (/ (+ (car pt1) (car pt2)) 2) (/ (+ (cadr pt1) (cadr pt2)) 2))
					loc_midpoint (polar midpt (+ (GetAngle pt1 pt2) (/ pi 2)) offset)
				) ;; 200 - смещение размеров
				(AddDim pt1 pt2 loc_midpoint)
			)
		)
		(setq num-verts (1- num-verts))
	)
	(princ)
)
(princ "\n\tCommand autodim_bo loaded. (DP 0 0) (DP 0 1) ")
