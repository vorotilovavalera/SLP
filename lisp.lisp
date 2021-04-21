;5) Определите функцию, которая увеличивает элементы исходного списка на единицу.

(defun increase(list)
	(cond 
		( (null list) nil )
		( t (cons (+(car list) 1) (increase(cdr list))) )
	) 
)

(print (increase '(-4 5 0)))         
;>(-3 6 1) 

(print (increase '()))               
;>NIL 

;6) Определите функцию, переводящую список чисел в список соответствующих им названий.
(defun num-to-string(L)
    ( cond
        ( (null L) L )
        ( t ( cons  ( format nil "~r" ( car L) ) ( num-to-string ( cdr L ) ) ) )

    )
)

( print ( num-to-string '(119 0 -1213) ) ) ;("one hundred and nineteen" "zero" "minus one thousand, two hundred and thirteen") 

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

;19) Определите функцию (ЛУКОВИЦА, n), строящую N-уровневый вложенный список,
;элементом которого на самом глубоком уровне является N
;Пример: (ЛУКОВИЦА, 5) -> (((((5)))))

(defun onion (n)
    (do ((res (list n)))
        ((= n 1) res)
            (setq res (list res))
        (setq n (- n 1))))

;TEST 1
(print (onion 7))
;TEST 2
(print (onion 2))
;TEST 3
(print (onion 13))
;20) Определите функцию ПЕРВЫЙ-АТОМ, результатом которой будет первый атом списка

(defun ПЕРВЫЙ-АТОМ(x)
    ((lambda (f1)
        (cond 
            ((ATOM f1) f1)
            (t (ПЕРВЫЙ-АТОМ f1))
        ))
    (car x)))

(print (ПЕРВЫЙ-АТОМ '(((5) 6 7)4 (1 2) 2 3))) ;5
(print (ПЕРВЫЙ-АТОМ '(() (1 2 3) 1 1))) ;NIL

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
(defun del (a)
    (cond((null (symbol-plist a))nil)
        (t(remprop a (car (symbol-plist a)))(del a))
        )
    )

(setf (get 'a 'x) 2)
(setf (get 'a 'y) 3)

(setf (get 'b 'x) 5)
(setf (get 'b 'y) 1)

(setf (get 'atlanta 'x) -3)
(setf (get 'atlanta 'y) 2)

(print "Task 47")

(print (del 'a))

(print (del 'b))

(print (del 'atlanta))



