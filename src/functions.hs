doubleMe :: Num a => a -> a
doubleMe x = x + x

doubleSmallNumber :: (Ord a, Num a) => a -> a
doubleSmallNumber x = if x > 100
    then x
    else x*2

removeNonUppercase :: String -> String  
removeNonUppercase st = [ c | c <- st, c `elem` ['A'..'Z']]

addThreeNumbers :: Int -> Int -> Int -> Int
addThreeNumbers x y z = x + y + z

factorial :: Integer -> Integer  
factorial n = product [1 .. n]

circumference :: Float -> Float  
circumference r = 2 * pi * r

circumference' :: Double -> Double  
circumference' r = 2 * pi * r

nando :: (Ord a, Num a) => [a] -> String
nando x = if minimum x + maximum x > 5 then "ERROU" else "ACERTOU"

-- Pattern matching --

lucky :: (Integral a) => a -> String  
lucky 7 = "LUCKY NUMBER SEVEN!"  
lucky x = "Sorry, you're out of luck, pal!"

addVectors :: (Num a) => (a, a) -> (a, a) -> (a, a)  
addVectors (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

first :: (a, b, c) -> a  
first (x, _, _) = x

second :: (a, b, c) -> b  
second (_, y, _) = y

third :: (a, b, c) -> c  
third (_, _, z) = z

head' :: [a] -> a
head' [] = error "Can't call head on an empty list, faggot!"
head' (x:_) = x

tell :: (Show a) => [a] -> String
tell [] = "The list is empty"
tell (x:[]) = "The list has one element " ++ show x
tell (x:y:[]) = "The list has two elements " ++ show x ++ " and " ++ show y
tell (x:y:_) = "This list is long. The first two elements are: " ++ show x ++ " and " ++ show y

length' :: (Num b) => [a] -> b
length' [] = 0
length' (_:xs) = 1 + length' xs

sum' :: (Num a) => [a] -> a
sum' [] = 0
sum' (x:xs) = x + sum' xs

-- Patterns --

capital :: String -> String  
capital "" = "Empty string, whoops!"  
capital all@(x:xs) = "The first letter of " ++ all ++ " is " ++ [x]

-- Guards --
bmiTell :: (RealFloat a) => a -> a -> String  
bmiTell weight height  
    | bmi ^ 2 <= skinny = "You're underweight, you emo, you!"  
    | bmi ^ 2 <= normal = "You're supposedly normal. Pffft, I bet you're ugly!"  
    | bmi ^ 2 <= fat = "You're fat! Lose some weight, fatty!"  
    | otherwise = "You're a whale, congratulations!"
    where bmi = weight / height ^ 2
          skinny = 18.5
          normal = 25.0
          fat = 30.0 

nandoUsingGuards :: (Ord a, Num a) => [a] -> String
nandoUsingGuards l
    | minimum l + maximum l > 5 = "ERROU"
    | otherwise = "ACERTOU"

max' :: (Ord a) => a -> a -> a
max' a b
    | a > b =     b
    | otherwise = b

myCompare :: (Ord a) => a -> a -> Ordering
a `myCompare` b
    | a > b     = GT
    | a == b    = EQ
    | otherwise = LT

initials :: String -> String -> String  
initials firstname lastname = [f] ++ ". " ++ [l] ++ "."  
    where (f:_) = firstname  
          (l:_) = lastname

calcBmis :: (RealFloat a) => [(a, a)] -> [a]  
calcBmis xs = [bmi w h | (w, h) <- xs]  
    where bmi weight height = weight / height ^ 2 

-- Lets -- 
--calcBmis :: (RealFloat a) => [(a, a)] -> [a]  
--calcBmis xs = [bmi | (w, h) <- xs, let bmi = w / h ^ 2]  

cylinder :: (RealFloat a) => a -> a -> a  
cylinder r h = 
    let sideArea = 2 * pi * r * h  
        topArea = pi * r ^2  
    in  sideArea + 2 * topArea

-- Case --
--head' :: [a] -> a  
--head' [] = error "No head for empty lists!"  
--head' (x:_) = x

--head' :: [a] -> a  
--head' xs = case xs of [] -> error "No head for empty lists!"  
--                      (x:_) -> x 

describeList :: [a] -> String  
describeList xs = "The list is " ++ case xs of [] -> "empty."  
                                               [x] -> "a singleton list."   
                                               xs -> "a longer list."

--describeList :: [a] -> String  
--describeList xs = "The list is " ++ what xs  
--    where what [] = "empty."  
--          what [x] = "a singleton list."  
--          what xs = "a longer list."

-- Recursion

maximum' :: (Ord a) => [a] -> a  
maximum' [] = error "maximum of empty list"  
maximum' [x] = x  
maximum' (x:xs)   
    | x > maxTail = x  
    | otherwise = maxTail  
    where maxTail = maximum' xs 

-- cleaner way
-- maximum' (x:xs) = max x (maximum' xs) 

replicate' :: (Num a, Ord a) => a -> b -> [b]
replicate' n x
    | n <= 0 = []
    | otherwise = x:replicate' (n - 1) x

take' :: (Num i, Ord i) => i -> [a] -> [a]  
take' n _  
    | n <= 0   = []  
take' _ []     = []  
take' n (x:xs) = x : take' (n-1) xs

reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = reverse' xs ++ [x]

repeat' :: a -> [a]
repeat' x = x:repeat' x

zip' :: [a] -> [b] -> [(a, b)]
zip' [] _ = [];
zip' _ [] = [];
zip' (x:xs) (y:ys) = (x, y):zip' xs ys

elem' :: (Eq a) => a -> [a] -> Bool
elem' _ [] = False
elem' e (x:xs)
    | e == x = True
    | otherwise = elem' e xs

quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) = 
    let smallerSorted = quicksort [a | a <- xs, a <= x]
        biggerSorted = quicksort [a | a <- xs, a > x]
    in smallerSorted ++ [x] ++ biggerSorted

quicksort' :: (Ord a) => [a] -> [a]
quicksort' [] = []
quicksort' (x:xs) = smaller ++ [x] ++ bigger
    where smaller = quicksort' [a | a <- xs, a <= x]
          bigger = quicksort' [a | a <- xs, a > x]

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys

flip' :: (a -> b -> c) -> (b -> a -> c)
-- flip' f = g
--     where g x y = f y x
flip' f x y = f y x

map' :: (a -> b) -> [a] -> [b]
map' _ [] = []
map' f (x:xs) = f x : map' f xs

filter' :: (a -> Bool) -> [a] -> [a]
filter' _ [] = []
filter' p (x:xs)
    | p x = x : filter p xs
    | otherwise = filter p xs

-- quicksort' :: (Ord a) => [a] -> [a]
-- quicksort' [] = []
-- quicksort' (x:xs) = smaller ++ [x] ++ bigger
--     where smaller = quicksort' (filter (<=x) xs)
--           bigger = quicksort' (filter (>x) xs)

largestDivisible :: (Integral a) => a
largestDivisible = head (filter p [100000, 99999 ..])
    where p x = x `mod` 3829 == 0

takeWhile' :: (Eq a) => (a -> Bool) -> [a] -> [a]
takeWhile' _ [] = []
takeWhile' p (x:xs)
    | p x = x : takeWhile' p xs
    | otherwise = []

collatzSequence :: (Integral a) => a -> [a]
collatzSequence 1 = [1]
collatzSequence n
    | even n =  n:collatzSequence (n `div` 2)
    | odd n = n:collatzSequence (n * 3 + 1)

numLongCollatzSequences :: Int  
numLongCollatzSequences = length (filter isLong (map chain [1..100]))  
    where isLong xs = length xs > 15