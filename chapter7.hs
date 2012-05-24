{-
  passed: 5
  not solved: 8, 9
-}

-- ex.1

ex1_1 f p xs = [f x | x <-xs, p x]
ex1_2 f p xs = map f (filter p xs)
ex1_3 f p = (map f) . (filter p)


-- ex.2

myall :: (a -> Bool) -> [a] -> Bool
myall _ [] = True
myall f (x:xs) | f x == True = myall f xs
               | otherwise = False

myany :: (a -> Bool) -> [a] -> Bool
myany _ [] = False
myany f (x:xs) | f x == False = myany f xs
               | otherwise = True

-- NOT WORKING!!
mytakeWhile :: (a -> Bool) -> [a] -> [a]
mytakeWhile _ [] = []
mytakeWhile f (x:xs) | f x == True = (x : mytakeWhile f xs)
                     | otherwise = []
                                   
mydropWhile :: (a -> Bool) -> [a] -> [a]
mydropWhile _ [] = []
mydropWhile f (x:xs) | f x == True = mydropWhile f xs
                     | otherwise = x:xs


-- ex.3

mymap f = foldr (\x y-> (f x) : y) []

myfilter f = foldr (\x y-> if (f x) then x:y else y) []


-- ex.4

dec2int :: [Int] -> Int
dec2int = foldl (\a n -> a * 10 + n) 0


-- ex.6

mycurry :: ((a,b) -> c) -> a -> b -> c
mycurry f = \x -> \y -> f (x, y)

myuncurry :: (a -> b -> c) -> (a,b) -> c
myuncurry f = \(x,y) -> f x y


-- ex.7

type Bit = Int

unfold p h t x | p x = []
               | otherwise = h x : unfold p h t (t x)

chop8 :: [Bit] -> [[Bit]]
chop8 = unfold null (take 8) (drop 8)

mymap2 :: (a -> b) -> [a] -> [b]
mymap2 f = unfold null (f . head) tail

myiterate :: (a -> a) -> a -> [a]
myiterate f = unfold (\_ -> False) (\x -> x) f
