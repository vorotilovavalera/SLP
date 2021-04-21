-- 3. Определите функцию, которая разделит исходный список из целых чисел на два списка: список положительных чисел и список отрицательных чисел. 

separator [] =([],[])
separator bt = ([x|x<-bt,x<0],[y|y<-bt,y>0])

main = print $ separator [-1, 2, 0, -7, 4, 5,-6,6,8]  


--([-1,-7,-6],[2,4,5,6,8])

-- 18. Определите предикат РАВЕНСТВО-МНОЖЕСТВ, проверяющий совпадение двух множеств (независимо от порядка следования элементов).
-- Подсказка: напишите функцию УДАЛИТЬ, удаляющую данный элемент из множества.
delete x member
        | x == [] = []
        | member == (head x) = delete (tail x) member
        | otherwise = (head x) : delete (tail x) member
eq_set x y
        | x == [] && y == [] = True
        | (length x == length y) == False = False
        | otherwise = eq_set (tail x) (delete y (head x))
        
            
main :: IO ()
main = do
    print (eq_set [1, 3, 5] [2, 4, 6])
    print (eq_set [1, 3, 5] [5, 3, 1])
    print (eq_set [1, 3, 5] [3, 1, 5, 6])
-- False
--True
--False

-- 25.Реализовать алгоритм быстрой сортировки.
quicksort [] = []
quicksort (p:xs) = (quicksort lesser) ++ [p] ++ (quicksort greater)
    where
        lesser = filter (< p) xs
        greater = filter (>= p) xs
main = print (quicksort [1, 7, 4, 2])
-- [1,2,4,7]
