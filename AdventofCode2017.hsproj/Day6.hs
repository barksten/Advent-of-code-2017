module Day6
    ( solution
    ) where
      
import Data.Char

main :: IO ()
main = interact solution

solution :: String -> String
solution s = show (day6 s)

prepare :: String -> [Bank]
prepare = map digitToInt . filter isDigit

day6 :: String -> [Bank]
day6 input = redistribute (banks !! idx) (idx) (updateNth idx (const 0) banks)
  where banks = prepare input
        idx = maxi banks

type Bank = Int
type Index = Int

maxi :: [Bank] -> Index
maxi xs = snd (maximum (zip xs [0..]))

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