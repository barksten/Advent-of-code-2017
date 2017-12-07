module Day2
    ( someFunc
    ) where

import           Data.List
import           Data.Maybe


type Row = [Int]
type Matrix = [Row]

someFunc s = show (day2 s)

day2 :: String -> Int
day2 = sum . map ((divide . evenly) . sortRow) . makeMatrix

makeMatrix :: String -> Matrix
makeMatrix =  map (map parseInt . words) . lines

parseInt :: String -> Int
parseInt s = read s :: Int

sortRow :: Row -> Row
sortRow = sortBy (flip compare)


findEvenly :: Int -> Row -> Row
findEvenly _ []     = [0]
findEvenly n (x:xs) = if rem n x == 0 then [n, x] else findEvenly n xs

evenly :: Row -> Row
evenly [] = []
evenly (x:xs) | findEvenly x xs == [0] = evenly xs
              | otherwise = findEvenly x xs

divide :: Row -> Int
divide []     = 0
divide (x:xs) = if null xs then 0 else quot x (head xs)



rowdiffer :: Matrix -> Row
rowdiffer = map diff

diff :: (Num a, Ord a) => [a] -> a
diff xs = maximum xs - minimum xs
