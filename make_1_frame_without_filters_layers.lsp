(defun c:make_1_frame_without_filters_layers (/ frame_point_1 frame_point_2 layoutName layout)
	(setq 
		frame_point_1 (getpoint "\nВкажіть першу крайню точку : ")                                             			  
		frame_point_2 (getpoint "\nВкажіть другу крайню точку : ")
		count_layout 1
		layoutName (strcat "page_" (itoa count_layout))
		layouts (vla-get-layouts (vla-get-activedocument (vlax-get-acad-object)))
	)
	(while (not (vl-catch-all-error-p (vl-catch-all-apply 'vla-item (list layouts layoutName))))
		(setq 
			count_layout (+ count_layout 1)
			layoutName (strcat "page_" (itoa count_layout))
		)
	) 
	(vla_create_new_layout layoutName frame_point_1 frame_point_2 '("empty"))

	(foreach layout (layoutlist) ; отримуємо всі листи
		(if (wcmatch layout "*page_*") ; перевіряємо, чи назва листа містить "page_"
			(princ (strcat "\nLayout: " layout " contains 'page_'"))
			(vla_delete_layout (list layout))
		)
	)
	(princ)
)	



(defun vla_create_new_layout (layoutName frame_point_1 frame_point_2 layers / layouts layout viewport ss Custom_Scale result)
	(setq layouts (vla-get-layouts (vla-get-activedocument (vlax-get-acad-object))))                                      ; Отримуємо список листів в активному документі
	(if (not (vl-catch-all-error-p (vl-catch-all-apply 'vla-item (list layouts layoutName))))                             ; Перевіряємо, чи лист з заданим ім'ям вже існує
		(princ (strcat "\nLayout \"" layoutName "\" already exists."))                                                    ; Якщо лист вже існує, виводимо повідомлення
		(progn
			(setq layout (vla-add layouts layoutName))                                                                    ; Якщо лист не існує, створюємо новий лист з заданим ім'ям
			(vla-put-ActiveLayout (vla-get-activedocument (vlax-get-acad-object)) layout)                                 ; Робимо новий лист активним
			(vla-put-ConfigName layout "Print As PDF.pc3")                                                                ; Встановлюємо конфігурацію принтера
			(vla-RefreshPlotDeviceInfo layout)
			(vla-put-CanonicalMediaName layout "A3")                                                                      ; Задаємо розмір паперу
			;(vla-put-PlotRotation layout ac90degrees)                                                                     ; Встановлюємо орієнтацію на альбомну
			(setq ss (ssget "L" '((0 . "VIEWPORT"))))                                                                     ; Вибираємо видове вікно
			(if ss
				(progn
					(setq viewport (vlax-ename->vla-object (ssname ss 0)))                                                ; Отримуємо об'єкт видового вікна
					(setq Custom_Scale (abs (compare_distances (list 0.0 0.0 0.0) (list 420.0 297.0 0.0) frame_point_1 frame_point_2)))   ; Змінюємо розмір видового вікна
					(vlax-put-property viewport 'Height 297)                                                              ; Встановлюємо висоту  видового вікна
					(vlax-put-property viewport 'Width 420)                                                               ; Встановлюємо ширину видового вікна
					(vlax-put-property viewport 'Layer "0")                                                               ; Встановлюємо шар видового вікна
					(vla-put-Center viewport (vlax-3d-point '(210.0 148.5 0.0)))                                          ; Змінюємо центр видового вікна
					(vla-put-mspace (vla-get-activedocument (vlax-get-acad-object)) :vlax-true)                           ; Переходимо в простір моделі
					(vla-ZoomWindow (vlax-get-acad-object) (vlax-3d-point frame_point_1) (vlax-3d-point frame_point_2))   ; Змінюємо масштаб видового вікна 
					(vlax-put-property viewport 'CustomScale Custom_Scale)                                                ; Встановлюємо користувацький масштаб видового вікна
					(if (/= layers 0)
						(progn 
						(setq result (list "_.vplayer"))
						(foreach layer layers
							(if (tblsearch "LAYER" layer) ; перевіряємо, чи існує шар
								(setq result (append result (list "f" layer "C")))
								(princ (strcat "\n **** Слой " layer " не існує ****"))
							)
						)
						(setq result (append result (list "")))
						(command result)
						)
					)
					(vla-put-mspace (vla-get-activedocument (vlax-get-acad-object)) :vlax-false)                          ; Переходимо в простір листа
					(vla-ZoomAll  (vlax-get-acad-object))                                                                 ; Змінюємо масштаб виду на "Все"
					(vla-put-ActiveLayout (vla-get-activedocument (vlax-get-acad-object)) (vla-item layouts "Model"))
				)
			)
			
		)
	)
	(princ)                                                                                                               ; Завершуємо вивід
)
                                                                                                                          ;-------ПОЧАТОК----- ФУНКЦІЯ СТВОРЕННЯ ЛИСТА ПО ІМЕНІ І ДВУМ ТОЧКАМ 

                                                                                                                          ;-------КІНЕЦЬ----- ФУНКЦІЯ СТВОРЕННЯ ЛИСТА ПО ІМЕНІ І ДВУМ ТОЧКАМ 
(defun compare_distances (p1 p2 p3 p4 / dx1 dy1 dx2 dy2 ratio_x ratio_y)
	(setq 
		dx1 (- (car p2) (car p1))
		dy1 (- (cadr p2) (cadr p1))
		dx2 (- (car p4) (car p3))
		dy2 (- (cadr p4) (cadr p3))
		ratio_x (/ dx1 (if (= dx2 0) 1 dx2))
		ratio_y (/ dy1 (if (= dy2 0) 1 dy2))
	)
	(min (abs ratio_x) (abs ratio_y))
)



(defun vla_delete_layout (layoutNames  / layoutObject)                                                                    ;  layoutNames список імен листів для видалення
	(foreach layoutName layoutNames                                                                                       ; Цикл для кожного імені листа в списку
		(if                                                                                                               ; Перевірка, чи існує лист з заданим ім'ям
			  (setq layoutObject (vla-item (vla-get-layouts (vla-get-activedocument (vlax-get-acad-object))) layoutName)) ; Якщо лист існує, зберігаємо його в змінну 'layoutObject'
			  (progn
				(vla-delete layoutObject)                                                                                 ; Якщо лист існує, видаляємо його
				(princ (strcat "\nLayout \"" layoutName "\" has been deleted."))                                          ; Виводимо повідомлення про успішне видалення листа
			  )
			  (princ (strcat "\nLayout \"" layoutName "\" does not exist."))                                              ; Якщо лист не існує, виводимо повідомлення про це
		)
	)
  (princ)                                                                                                                 ; Завершуємо функцію
)
                                                                                                                          ;-------КІНЕЦЬ----- ФУНКЦІЯ ВИДАЛЕННЯ ЛИСТІВ ЯКІ ЗНАХОДЯТЬСЯ У СПИСКУ



                                                                                                                          ;-------ПОЧАТОК----- ФУНКЦІЯ ВИДАЛЕННЯ ВСІХ ЛИСТІВ
(defun vla_delete_all_layout (/ delete_layouts layoutObject)
	(if (not (vl-catch-all-error-p (vl-catch-all-apply 'vla-item (list (vla-get-layouts (vla-get-activedocument (vlax-get-acad-object))) "delete_layout")))) (vla_delete_layout '("delete_layout")))
	                                                                                                                      ;(if (setq layoutObject (vla-item (vla-get-layouts (vla-get-activedocument (vlax-get-acad-object))) "delete_layout")) (vla_delete_layout '("delete_layout")))
	(setq delete_layouts (layoutlist))                                                                                    ; Отримуємо список існуючих листів в документі
	(vla_create_new_layout  "delete_layout" '(0.0 0.0 0.0) '(420.0 297.0 0.0) 0)                                            ; Створюємо новий лист з ім'ям "delete_layout", так як повинен бути хочаб 1 лист
	(vla_delete_layout delete_layouts)                                                                                    ; Видаляємо всі листи, які входять до списку 'delete_layouts'
)
