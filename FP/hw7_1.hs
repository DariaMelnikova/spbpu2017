data Deque a = Deque [a] [a]

pushFront :: Deque a -> a -> Deque a
pushFront (Deque inp out) x = Deque (x:inp) out

pushBack :: Deque a -> a -> Deque a
pushBack (Deque inp out) x = Deque inp (x:out)

takeHalf :: [a] -> [a]
takeHalf list  = take (fromInteger $ floor $ (toRational $ length list)/2) list

popBack :: Deque a -> (Deque a, a)
popBack (Deque [] []) = error "Empty deque"
popBack (Deque inp (oe:ot)) = (Deque inp ot, oe)
popBack (Deque inp []) = popBack $ Deque (takeHalf inp) (takeHalf $ reverse inp)
                      

popFront :: Deque a -> (Deque a, a)
popFront (Deque [] []) = error "Empty queue"
popFront (Deque (ih:it) out) = (Deque it out, ih)
popFront (Deque [] out) = popFront $ Deque (takeHalf $ reverse out) (takeHalf out)

{-
Применим в данному случае метод банкира по аналогии. 
Чтобы получить деку размера N нужно полуить не менее N$. Так как при данной реализации только половина списка "перекидываеется" в случае необходимости, можно быть уверенными, что линейные затраты на перемещение списка будут происходить не чаще раза в N/2, то есть линенйное число операций. Таким образом амортизированная слоность - константа.

Метод физика в данном случае ничем не отличается от очереди. За потенциал возьмем размер очереди - сумму элементов двух стеков. добавление хотя бы в один из стеков увеличивает потенциал на 1, удаление без перемещения не изменяет потенциал, а перекидывание стека уменьшает потенциал на N и занимает O(N). Потенциал не может оказаться отрицательным, так как перекидываемое количество элементов не может быть больше размера очереди, а между перекидываниями не может быть менее N/2 операций.
-}
