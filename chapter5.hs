-- List comprehensions

squareList :: (Num a,Enum a) => a -> [a]
squareList n = [x^(2::Integer) | x <- [1..n]]

zipList :: Int -> Int -> [(Int,Int)]
zipList m n = [(x,y) | x <- [0..m],y<-[0..n]] 

concatList :: [[a]] -> [a]
concatList xss = [x |  xs <- xss, x <- xs]

firstdxs :: [(a,b)] -> [a]
firstdxs xs = [x| (x,_) <- xs ]

length' :: [a] -> Int
length' xs = sum [1 | _ <- xs]

-- will list all factors of a number
factors :: Int -> [Int]
factors n = [ x | x <- [1..n], n `mod` x == 0]

prime :: Int -> Bool
prime n = factors n == [1,n]

primes :: Int -> [Int]
primes n = [x | x <- [1..n],prime x]

find' :: Eq a1 => a1 -> [(a1, a2)] -> [a2]
find' k t = [v | (k',v) <- t, k == k']

pairs :: [a] -> [(a,a)]
pairs xs = zip xs (tail xs)

sorted :: [Int] -> Bool
sorted xs = and [x<=y | (x,y) <- pairs xs]

positions :: (Num a1,Enum a1, Eq a2) => a2 -> [a2] -> [a1] 
positions a xs = [i | (x,i)<- zip xs [0..], x == a]

-- exercises 

-- 1 sum of suare of first 100
answer1 :: [Integer]
answer1 = squareList 100

-- 2 grid List
answer2 :: Int -> Int -> [(Int,Int)]
answer2 m n = zipList m n

-- 3 square grid
answer3 :: Int -> [(Int,Int)]
answer3 n = answer2 n n 

-- 4 replicate a value
answer4 :: Int -> a -> [a]
answer4 n x = [x|_<-[1..n]]

-- 5 list all Pythagoran triplets
answer5 :: (Integral c) => c -> [(c,c,c)]
answer5 n = [(x,y,z)|x<-[1..n],y<-[1..n],z<-[1..n],(x^2) + (y^2) == z^2]

-- 6 factors of an number
factors''' :: Int -> [Int]
factors''' n = [ x | x <- [1..n-1], n `mod` x == 0]

perfects :: Int -> [Int]
perfects n = [x | x<-[1..n],sum (factors''' x) == x]

-- 7 concat generators
answer7 = concat [[(x,y) | y <- [3,4]] | x <- [1,2]]

-- 8 
find'' :: Eq a => a -> [(a,b)] -> [b]
find'' k t = [v | (k', v) <- t, k == k']

positions'' :: Eq a => a -> [a] -> [Int]
positions'' x xs = [i | i <- find'' x (zip xs [0..])]


-- 9 scalar product
scalarproduct :: [Int] -> [Int] -> Int
scalarproduct xs ys = sum[x*y | (x,y) <- zip xs ys]

