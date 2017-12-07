module Day3
    ( someFunc
    ) where

someFunc :: IO ()
someFunc = putStrLn "someFunc"


endNumbers = map (^2) (filter odd [1..1000]) 


countOnOneSide n =  2 * n + 1

--countOnCircle n = 

-- Returnerer vilket lager som n finns i
indexOf n = length (filter ( < n ) endNumbers)

