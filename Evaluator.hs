import Analyser 
import Control.Monad
import qualified Data.Map as M 
import Data.List
import LambdaCalculus
import Parser
--------------------------------------------------
-- Test environment
--------------------------------------------------
createOpTuple p n a f = (n, OpInfo {precedence = p, name = n,
                                    assoc = a, fix = f})

plusS     = createOpTuple 1 "+"  LeftA Infix
timesS    = createOpTuple 2 "*"  RightA Infix
incr      = createOpTuple 2 "++" RightA Prefix
lParenS   = createOpTuple 5 "("  LeftA  (Open  ")") 
rParenS   = createOpTuple 5 ")"  RightA (Close "(")

environment = M.fromList [plusS,incr,timesS,lParenS,rParenS]

-- Temporary function for eval 
buildLambdaExpr (Value (IPrim n)) = Const $ Data n
buildLambdaExpr (Call "+" [x, y]) = add (buildLambdaExpr x) (buildLambdaExpr y)
buildLambdaExpr (Call "*" [x, y]) = times (buildLambdaExpr x) (buildLambdaExpr y)
buildLambdaExpr (Call "(" [x])    = App identity (buildLambdaExpr x) 
buildLambdaExpr (Call "++" [x])   = add (buildLambdaExpr x) (c 1)

--test :: Either [Char] SElement
buildLambdaWrap s = 
    liftM buildLambdaExpr (buildExpression s environment)

evalExpr s = 
  liftM (whnf . buildLambdaExpr) (buildExpression s environment)
--    liftM eval (buildExpression s environment)

pp s = buildExpression s environment >>= pPrinter environment 

{-
Tests:
buildLambdaExpr "3 + ( ( ( ( 1 + 2 ) + 4 + 3 * 2 + 4 + ( 7 + 2 ) ) + 3 + 17 * 6 + 27 ) + 3 )"
164
evalExpr "( 1 + 3 ) + ( 3 * 2 ) + ( 7 + 5 ) + ( 8 + 9 ) + ( 6 * 7 ) + ( 3 + 3 ) + ( 2 + 3 )"
92
evalExpr "(1+3)+(3*2)+( 7 + 5 ) + ( 8 + 9 ) + ( 6 * 7 ) + ( 3 + 3 ) + ( 2 + 3 )"
92
pp "(1+3)+(3*2)+( 7 + 5 ) + ( 8 + 9 ) + ( 6 * 7 ) + ( 3 + 3 ) + ( 2 + 3 )" 
92
buildLambdaWrap "(1+3)+(3*2)+( 7 + 5 ) + ( 8 + 9 ) + ( 6 * 7 ) + ( 3 + 3 ) + ( 2 + 3 * (1+3)+(3*2)+( 7 + 5 ) + ( 8 + 9 ) + ( 6 * 7 ) + ( 3 + 3 ) + ( 2 + 3 ) )" 
evalExpr "(1+3)+(3*2)+( 7 + 5 ) + ( 8 + 9 ) + ( 6 * 7 ) + ( 3 + 3 ) + ( 2 + 3 * (1+3)+(3*2)+( 7 + 5 ) + ( 8 + 9 ) + ( 6 * 7 ) + ( 3 + 3 ) + ( 2 + 3 ) )" 
[Const (Data 189)]
-}