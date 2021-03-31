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

;14)
 (defun cut-list (lst n l) 
  (cond ((zerop l) nil)
        ((and (= n 1) (> l 0)) (cons (car lst) (cut-list (cdr lst) 1 (- l 1))))
        (t (cut-list (cdr lst) (- n 1) l))))
(defun task (lst p q)
  (cond ((= p q) lst)
        ((> p q) (task lst q p))
        (t  (let* ((ls (length lst))
                  (l (cut-list lst 1 (- p 1)))
                  (m (cut-list lst p (- q p)))
                  (r (cut-list lst q (- ls q -1))))
            (append l (list (car r)) (cdr m) (list (car m)) (cdr r))))))
(print(task '(1 2 3 4 5 6 7) 5 3))

;19)
(defun onion (n &optional (m n))
  (cond ((zerop n) m)
        (t (list (onion (1- n) m)))))
        (print(onion 5))

;20) Определите функцию ПЕРВЫЙ-АТОМ, результатом которой будет первый атом списка

(defun первый-атом (list) 
  ( ( lambda (first)
             (cond 
                 ((atom first) first)
                 (t (первый-атом first))
              )
    )(car list)
  )    
) 
    
(print (первый-атом '(((a b)) c d)) ) ;A
(print (первый-атом '((((1 ) b)) c d)) ) ;1
(print (первый-атом '(((atom) a) b) ) ) ;ATOM

;36) Проверить пересекаются  ли списки
(defun f (x y)
(cond ((null x) t)
((mem (car x) y) nil)
(t (f (cdr x) y))))


(defun mem (x y)
(cond
((null y) nil)
((equal x (car y)) x)
(t (mem x (cdr y)))))


(print (f '(1 4 6 10) '(0 2 3 5 10)))

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
