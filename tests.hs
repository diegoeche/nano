{-# LANGUAGE NoMonomorphismRestriction #-}
import Test.QuickCheck
import Analyser
import Evaluator
import LambdaCalculus
import Data.List
deepCheck = check (defaultConfig { configMaxTest = 1000})

splitConcat :: (Ord a) => [a] -> Maybe Bool
splitConcat x = do 
  (b, v, a) <- split x
  return $ (b ++ [v] ++ a) == x

defaultMaybe _ (Just x) = x 
defaultMaybe x Nothing  = x     

prop_SplitConcat x = (x /= []) ==> defaultMaybe False . splitConcat $ x

--main :: IO ()
--main = deepCheck (prop_SplitConcat:: [Int] -> Property)

-- Simple example describing suffix operator syntax
factorial = executeProgram
        "let rec suffix n ! = \
        \  ifThenElse (0 == n) \
        \    1 \
        \    (n * (n - 1)!) \
        \main =  5!" == Right (Const $ Data 120)

-- Simple example describing infix operator syntax
pow = executeProgram "let rec infixl x ^ n = \
                      \  ifThenElse (0 == n) \
                      \    1 \
                      \    (x * (x ^ (n - 1))) \
                      \main =  5^2" == Right (Const $ Data 25)

-- Parameters are evaluated as needed in whnf. Passing _|_ 
-- Doesn't cause non termination.
laziness = executeProgram "let rec x = x \
                          \main = ifThenElse (1 == 1) \
                          \   2 \
                          \   x " == Right (Const $ Data 2)

-- Test for resolving ambiguous operators. 
-- "+++++++" can be interpreted in a number of ways. Given the current environment 
-- the only one that has some sense is (1 + (++(++(++ 3)))) nano is able of discovering this.
ambiguousOp = executeProgram "let ++ x = x + 1 \
                            \main = 1 +++++++ 3"  == Right (Const $ Data 7)


-- Other examples.
closedOps = (executeProgram . intercalate "\n")  
            ["let incr = 1",
             "let suffix x ! = incr + x",
             "let closed < x > = x!!!!",
             "main = <1+2*3>!"]

prefixOp = (executeProgram . intercalate "\n")
           ["let add x y = x + y",
            "main = add 5 6"]

tests = [("factorial", factorial),
         ("pow", pow),
         ("laziness",laziness),
         ("ambiguousOp",ambiguousOp)]

failed = map fst $ filter (not . snd) tests



