{-
  not solved: 2, 6
-}

-- ex.1

aa :: Int -> Int -> Int
a `aa` b | b == 1 = a
         | otherwise = a * (a `aa` (b - 1))
                       
-- ex.3

myand :: [Bool] -> Bool
myand [] = True
myand (False:xs) = False
myand (True:xs) = myand xs

myconcat :: [[a]] -> [a]
myconcat [] = []
myconcat (x:xs) = x ++ myconcat xs

myreplicate :: Int -> a -> [a]
myreplicate 0 _ = []
myreplicate n x = x : replicate (n - 1) x

(!!?) :: [a] -> Int -> a
(x:xs) !!? 0 = x
(x:xs) !!? n = xs !!? (n - 1)

myelem :: Eq a => a -> [a] -> Bool
myelem _ [] = False
myelem e (x:xs) | (e == x) = True
                | otherwise = myelem e xs


-- ex.4

merge :: Ord a => [a] -> [a] -> [a]
merge [] a = a
merge a [] = a
merge (x:xs) (y:ys) | (x < y) = (x : merge xs (y:ys))
                    | otherwise = (y : merge (x:xs) ys)
                                  

-- ex.5

halve :: [a] -> ([a], [a])
halve [] = ([], [])
halve [x] = ([x], [])
halve (x:y:s) = ((x:s1), (y:s2))
  where
    (s1, s2) = halve s
  
msort :: Ord a => [a] -> [a]
msort [] = []
msort [x] = [x]
msort l = merge (msort l1) (msort l2)
  where
    (l1, l2) = halve l
