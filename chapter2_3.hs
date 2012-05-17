-- chapter 2

double x = x + x
quadruple x = double (double x)

factorial n = product [1..n]
average ns = sum ns `div` length ns

a = b + c
  where
    c = 1
    b = 1


n = a `div` length xs
  where
     a = 10
     xs = [1,2,3,4,5]

mylast x = drop (length x - 1) x
mylast2 x = head $ reverse x

myinit x = take (length x - 1) x
myinit2 x = reverse (tail (reverse x))


-- chapter 3

second :: [a] -> a
second xs = head (tail xs)

swap :: (a, b) -> (b, a)
swap (x,y) = (y,x)

pair :: a -> b -> (a, b)
pair x y = (x, y)

mydouble :: Num a => a -> a
mydouble x = x * 2

palindrome :: Eq a => [a] -> Bool
palindrome xs = reverse xs == xs

twice :: (a -> a) -> a -> a
twice f x = f (f x)
