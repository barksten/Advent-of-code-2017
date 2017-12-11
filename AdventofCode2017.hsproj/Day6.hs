module Day6
    ( solution
    ) where
      
import Data.Char
import Data.List

main :: IO ()
main = interact solution

solution :: String -> String
solution s = show (day6 s)

parseInt :: String -> Int
parseInt s = read s :: Int

prepare :: String -> [Bank]
prepare = map parseInt . words

day6 :: String -> Int
day6 input = length (findConfigs [banks]) - 1
  where banks = prepare input
  
day6' :: String -> Int
day6' input = length (findConfigs [patt] ) - 1
  where patt = head (findConfigs [(prepare input)])

type Bank = Int
type Index = Int

maxi :: [Bank] -> Index
maxi xs = case elemIndex (maximum xs) xs of --snd (maximum (zip xs [0..]))
              Just n -> n
              Nothing -> 0 -- TODO: exception
              
redistribute :: Int -> Index -> [Bank] -> [Bank]
redistribute n i xs
     | n == 0 = xs
     | otherwise = redistribute (n-1) (wrappedIndex(i+1) xs) (updateNth i (+1) xs)

updateNth :: Int -> (Int -> Int) -> [Bank] -> [Bank]
updateNth n f (x:xs)
     | n == 0 = (f x):xs
     | otherwise = x:updateNth (n-1) f xs
     
wrappedIndex :: Int -> [a] -> Int
wrappedIndex i xs = mod i (length xs)

findConfigs :: [[Bank]] -> [[Bank]]
findConfigs (xs:xss)
    | xs == [] = [[]]
    | xss == [[]] = [xs]
    | elem xs xss = xs:xss
    | otherwise = findConfigs (newConfig:xs:xss)
  where idx = maxi xs; newConfig = redistribute (xs !! idx) (wrappedIndex(idx+1) xs) (updateNth idx (const 0) xs)
         