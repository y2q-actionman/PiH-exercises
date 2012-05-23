{-
  not solved: 8
-}

square_sums = [x*x | x <- [1..100]]


myreplicate :: Int -> a -> [a]
myreplicate n a = [a | _ <- [1..n]]


pyths :: Int -> [(Int, Int, Int)]
pyths n = [(x, y, z) | x <- [1..n], y <- [1..n], z <- [1..n],
                       x*x + y*y == z*z]

-- from p.47
factors :: Int -> [Int]
factors n = [x | x <- [1..n], n `mod` x == 0]

perfects :: Int -> [Int]
perfects n = [x | x <- [2..n], (x * 2) == (sum $ factors x)]


-- [(x,y) | x <- [1,2,3], y <- [4,5,6]]
single_gen = concat [[(x,y) | y <- [4,5,6]] | x <-[1,2,3]]


-- from p.48
find :: Eq a => a -> [(a,b)] -> [b]
find k t = [v | (kk, v) <- t, k == kk]

positions :: Eq a => a -> [a] -> [Int]
positions x xs = find x (zip xs [0..])

               
scalarproduct :: [Int] -> [Int] -> Int
scalarproduct xs ys = (sum [x * ys!!ind | (x, ind) <- zip xs [0..]])