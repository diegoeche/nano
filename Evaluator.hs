import Analyser 
import Control.Monad
import qualified Data.Map as M 
import Data.List
import LambdaCalculus
import Parser
import qualified Data.Set as S
import Data.Maybe

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
    let opInfo' = opInfo decl
        vars =  if isRec opInfo' 
                then name opInfo': bindedVars decl
                else bindedVars decl
        binded = map (\x -> (x, defaultPrefix x)) 
                 $ vars
        env' = foldr curryInsert env binded
    in do
      expr <- createExprFromTokens (definition decl) env' defs vars
      let expr' = foldr Lam expr vars
          recExpr = if isRec opInfo' 
                    then App yComb expr'
                    else expr'
      return (recExpr, opInfo decl)
    where curryInsert (x,y) = M.insert x y
          defaultPrefix name = createOp 2 name LeftA Prefix False

createProgram env defs (declarations,main) = createProgram' env defs declarations
    where createProgram' env' defs' [] = createExprFromTokens main env' defs' [] 
          createProgram' env' defs' (decl:decls) = 
              do
                (def, opInfo) <- createDefinition decl env' defs' 
                let name' = name opInfo 
                    env'' = (M.insert name' opInfo env') 
                case fix opInfo of -- It's necessary to add a dummy operator in case of closed operator
                  Close x -> createProgram' (M.insert x (opInfo {fix = Open name'}) env'') 
                               (M.insert name' (\x -> applyArgs def x) defs') decls
                  Open x -> createProgram' (M.insert x (opInfo {fix = Close name'}) env'') 
                               (M.insert name' (\x -> applyArgs def x) defs') decls
                  _ -> createProgram' env'' (M.insert name' (\x -> applyArgs def x) defs') decls

executeProgram s = liftM whnf $ parseProgramWrap s
                   >>= createProgram environment definitions

--------------------------------------------------
-- Test environment
--------------------------------------------------
createOpTuple p n a f = (n, OpInfo {precedence = p, name = n,
                                    assoc = a, fix = f, isRec = False})

plusS       = createOpTuple 1 "+"  LeftA Infix
minusS      = createOpTuple 1 "-"  LeftA Infix
timesS      = createOpTuple 2 "*"  RightA Infix
ifThenElse  = createOpTuple 2 "ifThenElse" RightA Prefix
true'       = createOpTuple 2 "true" RightA Prefix
false'      = createOpTuple 2 "false" RightA Prefix
equals'     = createOpTuple 2 "==" RightA Infix
lParenS     = createOpTuple 5 "("  LeftA  (Open  ")") 
rParenS     = createOpTuple 5 ")"  RightA (Close "(")

environment = M.fromList [plusS, timesS, minusS, 
                          ifThenElse, true', false', 
                          equals', lParenS, rParenS]
applyArgs = foldl App
unary  f [x] = f x 
binary f [x, y] = f x y
ternary f [x, y, z] = f x y z
definitions = 
    M.fromList
    [("+", binary add), 
     ("*", binary times), 
     ("-", binary minus),
     ("(", unary $ App identity), 
     ("==", binary equals), 
     ("true", const true),
     ("false", const false),
     ("ifThenElse", ifthenelse)]

program =  
       "let closed < x > = x + 1" 
    ++ "let infixl x * y = times x y\n" 
    ++ "let suffix x ! = factorial x\n" 
    ++ "let closed ( x ) = id x\n" 
    ++ "let ++ x  = add x 1\n" 
    ++ "main  = (1 + 2 * 4)!" 

program2 = intercalate "\n" [ 
            "let incr = 1",
            "let suffix x ! = incr + x",
            "let closed < x > = (x)!!!!",
            "main = <1+2*3>!"]

program3 = intercalate "\n" [ 
            "let add x y = x + y",
            "main = add 5 6"]

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
executeProgram "let rec fact n = ifThenElse (0 == n) 1 ((n) * (fact (n - 1))) main = fact 4"

liftM whnf $ parseProgramWrap "let suffix x ! = x + 1 let suffix x $ = (2 + x)!! main = (1 + 2 * 4)$$" >>= createProgram environment definitions
-}