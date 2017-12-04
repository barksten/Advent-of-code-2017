{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( someFunc
    , foo
    ) where


import qualified Data.Text.IO as T

someFunc :: IO ()
someFunc = T.putStrLn "someFunc"

foo :: Integer -> Integer -> Integer

foo x y = x + y

-- A thin monadic skin layer
getList :: IO [Char]
getList = fmap take5 getContents

-- The actual worker
take5 :: [Char] -> [Char]
take5 = take 5 . filter (`elem` ['a'..'e'])
