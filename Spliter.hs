--import Data.Map as M
import Data.List
import Control.Arrow
import Test.QuickCheck

-- This algorithm was modified so 
split [] = []
split (y:[]) = [[[y]]]
split (y:ys) =  concatMap (merge y) (split ys)
                where merge z [] = [[[z]]]
                      merge z (x:xs) =
                          let s1 = (z : x) : xs
                              s2 = [z] : x : xs         
                          in [s1, s2]

-- All concatenated strings should be the same that the original
prop1 :: [Int] -> Property
prop1 x = 
    -- Just for efficiency
    length x < 20 ==> 
               all (== x) 
               $ map concat
               $ split x

prop2 :: [Int] -> Property
prop2 x = 
    x == nub x
          ==> let p = split x 
              in (length p) == (length $ nub p)