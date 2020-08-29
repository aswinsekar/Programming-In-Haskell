{-- double the given number --}
double :: Int -> Int
double a = a + a

{-- quadruple the given number --}
quadruple :: Int -> Int
quadruple a = double(double a)

{-- quick sort using pattern matching and recursion --}
qsort :: [Int] -> [Int]
qsort [ ] = [ ]
qsort (x:xs) = qsort smaller ++ [x] ++ qsort larger
               where
                   smaller = [a | a <- xs, a<=x]
                   larger = [b | b <- xs,b>x]

{-- TODO: Add sequence actions using IO Monads--}

