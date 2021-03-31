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
