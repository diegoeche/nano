{-# LANGUAGE NoMonomorphismRestriction #-}
import Test.QuickCheck
import Analyser
import Evaluator
import LambdaCalculus

deepCheck = check (defaultConfig { configMaxTest = 1000})

splitConcat :: (Ord a) => [a] -> Maybe Bool
splitConcat x = do 
  (b, v, a) <- split x
  return $ (b ++ [v] ++ a) == x

defaultMaybe _ (Just x) = x 
defaultMaybe x Nothing  = x     

prop_SplitConcat x = (x /= []) ==> defaultMaybe False . splitConcat $ x

main :: IO ()
main = deepCheck (prop_SplitConcat:: [Int] -> Property)

factorial = executeProgram
        ("let rec suffix n ! = ifThenElse (0 == n) 1 (n * (n - 1)!)\n"
         ++ "main =  5!") == Right (Const (Data 120))

pow = executeProgram
        ("let rec infixl x ^ n = ifThenElse (0 == n) 1 (x * (x ^ (n - 1)))\n"
         ++ "main =  5^2") == Right (Const (Data 25))

