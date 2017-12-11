module Day3
    ( solution
    ) where

main :: IO ()
main = interact solution

solution :: String -> String
solution s  = "someFunc"


endNumbers = map (^2) (filter odd [1..1000]) 


countOnOneSide n =  2 * n + 1

--countOnCircle n = 

-- Returnerer vilket lager som n finns i
indexOf n = length (filter ( < n ) endNumbers)

