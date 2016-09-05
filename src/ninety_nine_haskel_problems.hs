
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

-- 4
length' :: [a] -> Int
length' [] = 0
length' (_:xs) = 1 + length' xs

-- 5
reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = reverse' xs ++ [x]

-- 6
isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome [] = True
isPalindrome [_] = True
isPalindrome xs = head xs == last xs && isPalindrome (init $ tail xs)