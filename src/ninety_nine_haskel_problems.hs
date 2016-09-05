
-- 1
last' :: [a] -> a
last' [] = error "last of empty list"
last' [x] = x
last' (_:xs) = last' xs

-- 2
lastButOne :: [a] -> a
lastButOne [] = error "lastbutOne of empty list"
lastButOne [x,_] = x;
lastButOne (_:xs) = lastButOne xs

-- 3
elementAt :: [a] -> Int -> a
elementAt [] _ = error "index out of bounds"
elementAt (x:_) 1 = x
elementAt (x:xs) i
    | i < 1 = error "index out of bounds"
    | otherwise = elementAt xs (i - 1)

