module Lib
    ( someFunc
    ) where

type Matrix = [[Int]]

someFunc s = show (day2 s)

day2 :: String -> Int
day2 = sum . rowdiffer . makeMatrix

makeMatrix :: String -> Matrix
makeMatrix =  map (map parseInt . words) . lines

parseInt :: String -> Int
parseInt s = read s :: Int

rowdiffer :: Matrix -> [Int]
rowdiffer = map diff

diff :: (Num a, Ord a) => [a] -> a
diff xs = maximum xs - minimum xs
