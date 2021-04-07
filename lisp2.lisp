;1. Определите FUNCALL через функционал APPLY
(defun -funcall (function &rest args) 
    (apply function args)
    )

(print (funcall '+ 1 2 3 4 5))

;3. Определите функционал (APL-APPLY f x), который применяет каждую функцию fi списка
;(f1 f2 ... fn)
;к соответствующему элементу списка
;x = (x1 x2 ... xn)
;и возвращает список, сформированный из результатов.
(defun f1 (x)
    (+ x 25)
    )

(defun f2 (x)
    (apply '+ x)
    )

(defun APL-APPLY (f x)
    (IF (NULL f)
        nil
        (CONS (funcall (car f) (car x)) (APL-APPLY (CDR f) (CDR x)) )
        )
    )

(print (APL-APPLY '(f1 f1 f1 f2 f2) '(1 2 3 (1 2 3) (1 2 3 4))))

;5. Определите функциональный предикат (НЕКОТОРЫй пред список), который истинен, когда, 
;являющейся функциональным аргументом предикат пред истинен хотя бы для одного элемента списка список.
(defun некоторый (p lst) 
    (cond ((null lst) nil) ;если дошли до конца списка то ложь
        ((funcall p (car lst)) T) ; если верно на одном элементе то истина
        (t (некоторый p (cdr lst))) ;рекурсия
    )
)
 
(print(некоторый #'numberp '(1 2 3 5 7))); numberp предикат проверяет является ли числом
(print(некоторый #'numberp '(A B)))

;7. Определите фильтр (УДАЛИТь-ЕСЛИ-НЕ пред список), удаляющий из списка список все элементы, 
;которые не обладают свойством, наличие которого проверяет предикат пред

(defun удалить-если-не (p lst)
    ((lambda (f r)
    (cond ((null lst) nil)
        ((funcall p f) (cons f (удалить-если-не p r))) ;если предикат истинен на элементе, то составляем список из этого элемента и дальше проверяем на хвосте списка
        ((удалить-если-не p r))
        ))
        (car lst)
        (cdr lst)
    )
)
 
(print(удалить-если-не #'numberp '(1 A 3 B 5 6 7)))

;9. Напишите генератор порождения чисел Фибоначчи: 0, 1, 1, 2, 3, 5, ...

(let ( (x 1) (y 0) )
     (defun GenerateFibonachy () (let ((temp x)) (setq x (+ x y) y temp)))
     )


(print (funcall 'GenerateFibonachy))
(print (funcall 'GenerateFibonachy))
(print (funcall 'GenerateFibonachy))

;11.Определите фукнционал МНОГОФУН, который использует функции, являющиеся аргументами, по следующей схеме:
;(МНОГОФУН ’(f g ... h) x) ⇔ (LIST (f x) (g x) ... (h x)).

(defun многофун (f x)
    (mapcar #'(lambda (f) (apply f x)) f) ; mapcar вычисляет функцию на элементах списка
)

(print(многофун '(+ -) '(1 2)))

;13. Определите функцию, которая воввращает в качестве значения свое определение (лямбда-выражение).

(defun Request () (
                   (lambda (x)  (list 'defun 'Request () (list x (list x))))
                   '(lambda (x)  (list 'defun 'Request () (list x (list x))))
                   )
    )

(print (Request))

