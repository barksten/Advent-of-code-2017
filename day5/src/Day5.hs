module Day5
    ( someFunc
    ) where

someFunc :: String -> String
someFunc s = show (day5 s)

inc :: Int -> Int
inc = (+) 1


updateNth :: Int -> (Int -> Int) -> [Int] -> [Int]
updateNth n f (x:xs)
     | n == 0 = (inc x):xs
     | otherwise = x:updateNth (n-1) f xs


jump :: (Int, Int) -> [Int] -> Int
jump (n, i) xs
      | i < 0 || i >= length xs = n
      | otherwise = jump ((n + 1), (i + xs !! i)) (updateNth i inc xs)

parseInt :: String -> Int
parseInt s = read s :: Int

prepare :: String -> [Int]
prepare = map parseInt . lines

day5 :: String -> Int
day5 input = jump (0,0) (prepare input)
