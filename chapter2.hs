import Prelude
{-- factorial of a number using product with list --}
factorial :: Int -> Int
factorial c = product [1..c]

{-- average of a Int List --}
{-- Caution : this is not ideal average function as this will give the floor of the avaerage value --}
average :: Foldable t => t Int -> Int
average ns = sum ns `div` length ns

-- Exercises
-- 3
n :: Int
n = a `div` length xs
    where
        a = 10
        xs = [1..5] :: [Integer]

{-- colons added to differentiate from prelude functions - START --}

-- 4
last' :: [t] -> t
last' xs = head (reverse xs)

last'' :: [t] -> t
last'' xs = xs !! (length xs - 1)

-- 5
init' :: [t] -> [t]
init' xs = take (length xs - 1) xs

init'' ::[t] -> [t]
init'' xs = reverse(tail(reverse xs))

{-- colons added to differentiate from prelude functions - END --}