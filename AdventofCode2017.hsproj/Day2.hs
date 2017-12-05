

module Day2 where

import Data.Char

main = interact someFunc

someFunc s = show (day2 s)


diff :: (Num a, Ord a) => [a] -> a
diff xs = maximum xs - minimum xs

rowdiffer :: [[Int]] -> [Int]
rowdiffer = map diff

day2 :: String -> Int
day2 = sum . rowdiffer . makeMatrix


input = "123\t234\t345\t456\n123\t234\t345\t456\n123\t234\t345\t456\n"

makeMatrix :: String -> [[Int]]
makeMatrix =  map (map parseInt . words) . lines

parseInt :: String -> Int
parseInt s = read s :: Int