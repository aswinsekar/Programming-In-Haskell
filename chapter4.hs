-- check for even
even' :: Integral a => a -> Bool
even' n = n `mod` 2 == 0

-- split list at nth element
splitAt' :: Int -> [t] -> ([t],[t])
splitAt' n xs = (take n xs , drop n xs)

-- reciprocate the number
recip :: Fractional n => n -> n
recip n = 1 / n

-- simple if else clause

-- absolute number of a number
abs' :: Int -> Int
abs' n = if n >= 0 then n else -n

-- nested if else clause

--signature number of the number (-1 or 0 or 1)
signum' :: Int -> Int
signum' n = if n > 0 then 1 else if n == 0 then 0 else -1

-- guarded equations

-- absolute number of a number
abs'' :: Int -> Int
abs'' n | n>=0 = n
        | otherwise = -n

--signature number of the number (-1 or 0 or 1)
signum'' :: Int -> Int
signum'' n | n==0 = 0
           | n > 0 = 1
           | otherwise = -1


-- Pattern Matching

-- Normal patterns
-- Tuple patterns
-- List patterns

-- Lambda equations
odds :: Int -> [Int]
odds n = map ( \x -> x * 2 + 1) [1..n-1]


-- Exercies
halve :: [a] -> ([a],[a])
halve xs = (take n xs , drop n xs)
            where n = length xs `div` 2

-- reusing splitAt' we defined earlier
halve' :: [a] -> ([a],[a])
halve' xs = splitAt' (length xs `div` 2) xs

-- using index of
third :: [a] -> a
third xs = xs !! 2

-- using prelude functions
third' :: [a] -> a
third' xs = head (tail (tail xs))

-- not suggestible as there are exhaustive patterns
third'' :: [a] -> a
third'' (_:_:x:_) = x

-- safetail to handle empty list
safetail :: [a] -> [a]
safetail xs = if null xs then [] else tail xs

safetail' :: [a] -> [a]
safetail' xs | null xs = []
             | otherwise = tail xs

safetail'' :: [a] -> [a]
safetail'' [] = []
safetail'' (_:xs) = xs


-- luhn algo for specified digit of numbers (here 4)
double :: Num a => a -> a
double x = x + x

luhnDouble :: Integral a => a -> a
luhnDouble x = double x `mod` 9

luhn :: Int -> Int -> Int -> Int -> Bool
luhn a b c d = (luhnDouble a+  b+luhnDouble c+ d) `mod` 10 == 0