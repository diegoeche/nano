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

unary  f [x] = f x 
binary f [x, y] = f x y
definitions = 
    M.fromList
    [("+", binary add), ("*",binary times), 
     ("(", unary $ App identity)]

buildLambdaExpr :: [String]
                   -> M.Map String ([Expr] -> Expr)
                   -> ExprTree
                   -> Maybe Expr
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


createExprFromTokens :: [ExprToken]
                        -> M.Map String OpInfo
                        -> M.Map String ([Expr] -> Expr)
                        -> [String]
                        -> [Expr]
createExprFromTokens tokens env defs vars = do
      tree <- buildTreeFromTokens tokens env
      maybeToList $ buildLambdaExpr vars defs tree
    

createDefinition :: Declaration
                    -> M.Map String OpInfo
                    -> M.Map String ([Expr] -> Expr)
                    -> [(Expr, OpInfo)]
createDefinition decl env defs = 
    let vars = (bindedVars decl)
        binded = map (\x -> (x, defaultPrefix x)) 
                 . bindedVars $ decl
        env' = foldr curryInsert env binded
    in do
      expr <- createExprFromTokens (definition decl) env' defs vars
      return (foldr Lam expr vars, opInfo decl)
    where curryInsert (x,y) = M.insert x y
          defaultPrefix name = createOp 2 name LeftA Prefix

createProgram env defs (declarations,main) = createProgram' env defs declarations
    where createProgram' env' defs' [] = createExprFromTokens main env' defs' [] 
    
program =  
       "let closed < x > = x + 1" 
    ++ "let infixl x * y = times x y\n" 
    ++ "let suffix x ! = factorial x\n" 
    ++ "let closed ( x ) = id x\n" 
    ++ "let ++ x  = add x 1\n" 
    ++ "main  = (1 + 2 * 4)!" 

parsedProgram :: [([Parser.Declaration], [ExprToken])]
parsedProgram = parseProgramWrap program

--test :: Either [Char] SElement
buildLambdaWrap s = 
      buildTree s environment 
      >>= maybeToList . buildLambdaExpr [] definitions

evalExpr = liftM whnf . buildLambdaWrap 

pp s = buildTree s environment >>= pPrinter environment 

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