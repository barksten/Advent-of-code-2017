module Day5
    ( someFunc
    ) where

someFunc :: String -> String
someFunc s = show (day5 s)

--inc :: Int -> Int
--inc = (+) 1

pt2 :: Int -> Int
pt2 x = if x > 2 then x - 1 else x + 1


updateNth :: Int -> (Int -> Int) -> [Int] -> [Int]
updateNth n f (x:xs)
     | n == 0 = (f x):xs
     | otherwise = x:updateNth (n-1) f xs


jump :: (Int, Int) -> [Int] -> Int
jump (n, i) xs
      | i < 0 || i >= length xs = n
      | otherwise = jump ((n + 1), (i + xs !! i)) (updateNth i pt2 xs)

parseInt :: String -> Int
parseInt s = read s :: Int

prepare :: String -> [Int]
prepare = map parseInt . lines

day5 :: String -> Int
day5 input = jump (0,0) (prepare input)
