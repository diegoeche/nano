import Analyser 
import Control.Monad
import qualified Data.Map as M 
import Data.List
import LambdaCalculus
import Parser
import qualified Data.Set as S
import Data.Maybe
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

program =  
    "let closed x + y = add x y\n" 
    ++ "let infixl x * y = times x y\n" 
    ++ "let suffix x ! = factorial x\n" 
    ++ "let closed ( x ) = id x\n" 
    ++ "let ++ x  = add x 1\n" 
    ++ "main  = (1 + 2 * 4)!" 

parsedProgram :: [([Parser.Declaration], [ExprToken])]
parsedProgram = parseProgramWrap program

buildFunction :: Expr -> [String] -> Expr
buildFunction = foldr Lam 


unary  f [x] = f x 
binary f [x, y] = f x y
definitions = 
    M.fromList
    [("+", binary add), ("*",binary times), 
     ("(", unary $ App identity)]

-- Temporary function for eval 
buildLambdaExpr binded env expr = buildLambdaExpr' expr
    where buildLambdaExpr' (Value (IPrim n)) = return $ Const $ Data n
          buildLambdaExpr' (Call x args) | elem x binded = 
              do
                lambdaArgs <- mapM buildLambdaExpr' args
                return $ applyArgs (Var x) lambdaArgs
          buildLambdaExpr' (Call x args) = 
              do
                lambdaArgs <- mapM buildLambdaExpr' args
                definition <- M.lookup x env
                return $ definition lambdaArgs
          applyArgs = foldl App


--       buildLambdaExpr' (Call "+" [x, y]) = add (buildLambdaExpr' x) (buildLambdaExpr' y)
--           buildLambdaExpr' (Call "*" [x, y]) = times (buildLambdaExpr' x) (buildLambdaExpr' y)
--           buildLambdaExpr' (Call "(" [x])    = App identity (buildLambdaExpr' x) 
-- --  
--        buildLambdaExpr' (Call "++" [x])   = add (buildLambdaExpr' x) (c 1)
  
--test :: Either [Char] SElement
buildLambdaWrap s = 
      buildExpression s environment 
      >>= maybeToList . buildLambdaExpr [] definitions

evalExpr s = 
    liftM whnf $ buildLambdaWrap s 

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