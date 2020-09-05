-- List comprehensions

squareList :: (Num a,Enum a) => a -> [a]
squareList n = [x^(2::Integer) | x <- [1..n]]

zipList :: Int -> Int -> [(Int,Int)]
zipList m n = [(x,y) | x <- [1..m],y<-[1..n]] 

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

--positions :: a -> [a] -> [Int]
positions a xs = [i | (x,i)<- zip xs [0..], x == a]