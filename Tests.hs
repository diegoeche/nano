{-# LANGUAGE NoMonomorphismRestriction #-}
module Tests where
import Test.QuickCheck
import Analyser
import Evaluator
import LambdaCalculus
import Data.List
--import AlgorithmW
--import TypeChecker

deepCheck :: (Testable a) => a -> IO ()
deepCheck = check (defaultConfig { configMaxTest = 1000})

splitConcat :: (Ord a) => [a] -> Maybe Bool
splitConcat x = do 
  (b, v, a) <- split x
  return $ (b ++ [v] ++ a) == x

defaultMaybe :: t -> Maybe t -> t
defaultMaybe _ (Just x) = x 
defaultMaybe x Nothing  = x     

prop_SplitConcat :: (Ord a) => [a] -> Property
prop_SplitConcat x = (x /= []) ==> defaultMaybe False . splitConcat $ x

commonOperators =
    ["let closed ( x ) = x",
     "let infixr 1 a + b = add a b",
     "let infixr 2 a * b = times a b",
     "let infixr 1 a - b = sub a b",
     "let infixr 2 a == b = equals a b"]
--main :: IO ()
--main = deepCheck (prop_SplitConcat:: [Int] -> Property)
factorialN = executeProgram
    "let closed ( x ) = x \
    \let rec suffix n ! = \
    \  if (equals 0 n) \
    \    1 \
    \    (times n ((sub n 1)!)) \
    \main =  5!"

factorial :: Bool
factorial = executeProgram
        "let closed ( x ) = x \
        \let rec suffix n ! = \
        \  if (equals 0 n) \
        \    1 \
        \    (times n ((sub n 1)!)) \
        \main =  5!" == Right (Const $ IData 120)

powProgram = commonOperators ++
             ["let rec infixl x ^ n = ",
              "if (0 == n)",
              "1",
              "(x * (x ^ (n - 1)))",
              "main =  5^2"
             ]

-- Simple example describing infix operator syntax
pow :: Bool
pow = executeList powProgram == Right (Const $ IData 25)

int :: Integer -> Analyser.ExprTree
int = Value . IPrim

-- Parameters are evaluated as needed in whnf. Passing _|_ 
-- Doesn't cause non termination.
lazinessN =
    commonOperators 
    ++ ["let rec x = x ",
        "main = if (1 == 1) 2 x"]

laziness :: Bool
laziness = executeList lazinessN == Right (Const $ IData 2)

ambiguousOpN = commonOperators 
               ++ ["let ++ x = x + 1",
                   "main = 1 +++++++ 3"]
                                  

-- Test for resolving ambiguous operators. 
-- "+++++++" can be interpreted in a number of ways. Given the current environment 
-- the only one that has some sense is (1 + (++(++(++ 3)))) nano is able of discovering this.
ambiguousOp :: Bool
ambiguousOp = executeList ambiguousOpN == Right (Const $ IData 7)

-- Other examples.
closedOps :: Either [Char] LambdaCalculus.Expr
closedOps = executeList
            ["let incr = 1",
             "let suffix x ! = incr + x",
             "let closed < x > = x!!!!",
             "main = <1+2*3>!"]

prefixOp :: Either [Char] LambdaCalculus.Expr
prefixOp = executeList
           ["let add x y = x + y",
            "main = add 5 6"]

precedence :: Either [Char] LambdaCalculus.Expr
precedence = executeList
             ["main = (1+3)+(3*2)+( 7 + 5 )",
              " + ( 8 + 9 ) + ( 6 * 7 )",
              "+ ( 3 + 3 ) + ( 2 + 3 * ",
              "(1+3)+(3*2)+( 7 + 5 ) + ( 8 + 9 )",
              "+ ( 6 * 7 ) + ( 3 + 3 ) + ( 2 + 3 ) )"]

tests :: [([Char], Bool)]
tests = [("factorial", factorial),
         ("pow", pow),
         ("laziness",laziness),
         ("ambiguousOp",ambiguousOp),
         ("pairTest", pairTest   == Right (Const (IData 7))),
         ("listTest", listTest   == Right (Const (IData 1))),
         ("listLength", listLength == Right (Const (IData 4))) 
        ]

failed :: [[Char]]
failed = map fst $ filter (not . snd) tests

executeList :: [[Char]] -> Either [Char] LambdaCalculus.Expr
executeList = executeProgram . intercalate "\n"

typeTest :: Either [Char] LambdaCalculus.Expr
typeTest = executeList
           ["main = 5 + (6 == 3)"]

pairTest :: Either [Char] LambdaCalculus.Expr
pairTest = executeList $
           commonOperators ++ ["let pair = buildPair 5 (6 == 3)",
                               "main = if (snd pair) (fst pair + 3) (fst pair + 2)"
                              ]

-- Yep... infinite list + obfuscation via concatenation of operators
listTest :: Either [Char] LambdaCalculus.Expr
listTest = executeList
           ["let rec l = cons 1 l",
            "main = hdrestrestrest l"
           ]

listLength :: Either [Char] LambdaCalculus.Expr
listLength = executeList $
             commonOperators ++
             ["let rec closed [| l |] = if (isNull l) 0 (1 + [|rest l|] )",
              "main = [|cons 5 cons 6 empty|] + [|cons 3 cons 2 empty|]"
             ]

