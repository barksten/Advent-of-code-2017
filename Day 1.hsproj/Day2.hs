
module Day2 where



prep :: String -> [[Int]]
prep s = [[5,1,9,5],
         [7,5,3],
         [2,4,6,8]]

diff :: (Num a, Ord a) => [a] -> a
diff xs = (maximum xs) - (minimum xs)

rowdiffer :: [[Int]] -> [Int]
rowdiffer matrix = map diff matrix

day2 :: String -> Int
day2 = sum . rowdiffer . prep
