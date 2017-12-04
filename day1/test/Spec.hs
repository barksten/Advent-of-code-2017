import Data.Char
import Test.QuickCheck
import Lib


main :: IO ()
main = putStrLn "Test suite not yet implemented"


instance Arbitrary Char where
    arbitrary     = choose ('\32', '\128')
    coarbitrary c = variant (ord c `rem` 4)
