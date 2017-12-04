{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.Text.IO as T

import Data.Char

main :: IO ()
main = someFunc

someFunc :: IO ()
someFunc = T.putStrLn "someFunc"


prepare :: String -> [Int]
prepare s = map digitToInt s

pair :: [a] -> [(a, a)]
pair xs = zip xs (last xs : init xs)

isNeighbors :: Eq a => (a, a) -> Bool
isNeighbors (x, y) = x == y


day1 :: String -> Int
day1 = sum . map fst . filter isNeighbors . pair . prepare

rotate _ [] = []
rotate n xs = zipWith const (drop n (cycle xs)) xs

halfLength xs = quot (length xs) 2

pair' :: [a] -> [(a, a)]
pair' xs = zip xs (rotate (halfLength xs) xs)


day1' = sum . map fst . filter isNeighbors . pair' . prepare

