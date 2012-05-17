halve :: [a] -> ([a], [a])
halve xs  = (take n xs, drop n xs)
            where n = (length xs) `div` 2


safetail1 :: [a] -> [a]
safetail1 xs = if null xs then [] else tail xs

safetail2 :: [a] -> [a]
safetail2 xs | null xs = []
             | otherwise = tail xs

safetail3 :: [a] -> [a]
safetail3 [] = []
safetail3 (x:xs) = xs


v :: Bool -> Bool -> Bool
True `v` True = True
True `v` False = True
False `v` True = True
False `v` False = False


a :: Bool -> Bool -> Bool
b1 `a` b2 = if (b1 == True && b2 == True) then True
            else False


aa :: Bool -> Bool -> Bool
b1 `aa` b2 = if b1 == True then b2
             else False


mult = \x -> (\y -> (\z -> x * y * z))
