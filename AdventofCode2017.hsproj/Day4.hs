module Day4
    ( solution
    ) where

import Data.List

main :: IO ()
main = interact solution

solution :: String -> String
solution s = show (day4 s)

type Words = [String]

duplicates :: Words -> Bool
duplicates [] = False
duplicates (x:xs) = elem x xs || duplicates xs

-- part 2
sortWords :: Words -> Words
sortWords ws = map sort ws 

isValid :: Words -> Bool
isValid = not . duplicates . sortWords

removeInvalid :: [Words] -> [Words]
removeInvalid = filter isValid

process :: String -> [Words]
process input = map words (lines input)

day4 :: String -> Int
day4 = length . removeInvalid . process
