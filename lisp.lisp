;5) Определите функцию, которая увеличивает элементы исходного списка на единицу.

(defun plus-one (lst)
    (cond
        ((null lst) nil)
        (t (cons (1+ (car lst)) (plus-one (cdr lst))))
        )
    )

(print ( plus-one '( )))
(print ( plus-one '(1)))
(print ( plus-one '(1 2 3 4)))

;6) Определите функцию, переводящую список чисел в список соответствующих им названий.
(defun num-to-string(L)
    ( cond
        ( (null L) L )
        ( t ( cons  ( format nil "~r" ( car L) ) ( num-to-string ( cdr L ) ) ) )

    )
)



( print ( num-to-string '(12 10 3 193) ) )
( print ( num-to-string '(1000 0 -1213) ) )
( print ( num-to-string '( 1 ) ) )

;10)Определите функцию, осуществляющую удаление указанного количества последних элементов исходного списка.

(defun delete-last-el (list n) 
    (cond
         ((= (list-length  list) n) nil) 
         (t (cons (car list) (delete-last-el (cdr list) n)))
    )
) 
 
(print (delete-last-el '(1 2) 2)) ;NIL
(print (delete-last-el '(a b 3 4 5) 3)) ;(A B)
(print (delete-last-el '(1 2) 0)) ;(1 2)


;20) Определите функцию ПЕРВЫЙ-АТОМ, результатом которой будет первый атом списка

(defun ПЕРВЫЙ-АТОМ(x)
    ((lambda (f1)
        (cond 
            ((ATOM f1) f1)
            (t (ПЕРВЫЙ-АТОМ f1))
        ))
    (car x)))

(print (ПЕРВЫЙ-АТОМ '(((5) 6 7)4 (1 2) 2 3)))
;5
(print (ПЕРВЫЙ-АТОМ '(1 2 3 4 5)))
;1
(print (ПЕРВЫЙ-АТОМ '(() (1 2 3) 1 1)))
;NIL

;36) Проверить пересекаются  ли списки
(defun Пересечение (lst1 lst2)
    (cond ((null lst1) t)
        ((mem (car lst1) lst2) nil)
        (t (Пересечение (cdr lst1) lst2))))


(defun mem (x y)
    (cond
        ((null y) nil)
        ((equal x (car y)) t)
        (t (mem x (cdr y)))))


(print (Пересечение '(1 4 6 10) '(0 2 3 5 10)))
;NIL
(print (Пересечение '(1 4 6) '(2 3 5 10)))
;T
(print (Пересечение '() '()))
;T

;42)Определите функцию, находящую максимальное из значений, находящихся в вершинах дерева

(defun max-c (a b) 
    (if (> a b) a b))

(defun max-elem (tree) (
    cond 
        ((null (first tree)) -1)
        ((not (or (second tree) (third tree))) (first tree))
        (t (max-c (max-elem (second tree)) (max-elem (third tree))))))


(print (max-elem '(1 (2 (6) ()) (3 () (10)))));10


;47) Определите функцию УДАЛИТЬ-ВСЕ-СВОЙСТВА, которая удаляет все свойства символа.

(defun удалить-все-свойства(x)
    ( (lambda (свойства)
         ( cond 
             ((null свойства ) t )
             (t (remprop x (car свойства)) (удалить-все-свойства x))
         )
    )(SYMBOL-PLIST x))
)

(setf (get 'x 'property1)'value)
(setf (get 'x 'property2)2)

(удалить-все-свойства 'x)
