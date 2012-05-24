{-
  not solved: 5,6,7
  passed: 8
-}

-- ex.1

data Nat = Zero | Succ Nat deriving Show

add :: Nat -> Nat -> Nat
add Zero n = n
add (Succ m) n = add m (Succ n)

mult :: Nat -> Nat -> Nat
mult Zero _ = Zero
mult (Succ Zero) n = n
mult (Succ n) m = mult n (add m m)


-- ex.2

data Tree = Leaf Int | Node Tree Int Tree deriving Show

t :: Tree
t = Node (Node (Leaf 1) 3 (Leaf 4)) 5 (Node (Leaf 6) 7 (Leaf 9))

occurs :: Int -> Tree -> Bool
occurs m (Leaf n) = m == n
occurs m (Node l n r) = case compare m n of
  LT -> occurs m l
  EQ -> True
  GT -> occurs m r


-- ex.3

data Tree = Leaf Int | Node Tree Tree deriving Show

countLeaves :: Tree -> Int
countLeaves (Leaf _) = 1
countLeaves (Node l r) = (countLeaves l) + (countLeaves r)

balanced :: Tree -> Bool
balanced (Leaf _) = True
balanced (Node l r) = -1 <= diff && diff <= 1
  where diff = (countLeaves l) - (countLeaves r)
        
        
-- ex.4        

halve :: [a] -> ([a], [a])
halve xs  = (take n xs, drop n xs)
            where n = (length xs) `div` 2

balance :: [Int] -> Tree
balance [x] = Leaf x
balance [x,y] = Node (Leaf x) (Leaf y)
balance xs = Node (balance l) (balance r)
  where (l,r) = halve xs
