module Evaluator(executeProgram) where
import System.IO.Unsafe
import Analyser 
import Control.Monad
import qualified Data.Map as M 
import Data.List
import LambdaCalculus
import Parser
import TypeChecker
import qualified Data.Set as S
import Data.Maybe
import AlgorithmW

-- Builds a lambda expression using the tree of operator calling. 
-- The previous definitions of operators. And the binded variables. 
buildLambdaExpr :: [String]
                   -> M.Map String ([Expr] -> Expr)
                   -> ExprTree
                   -> Either [Char] Expr 
buildLambdaExpr binded env expr = buildLambdaExpr' expr
    where buildLambdaExpr' (Value (IPrim n)) = return $ Const $ Data n
          buildLambdaExpr' (Call x args) | elem x binded = 
              do
                lambdaArgs <- mapM buildLambdaExpr' args
                return $ applyArgs (Var x) lambdaArgs
          buildLambdaExpr' (Call x args) = 
              do
                lambdaArgs <- mapM buildLambdaExpr' args
                definition <- lookup x env
                return $ definition lambdaArgs
          lookup x env = case M.lookup x env of
                         Just x -> Right x
                         _      -> Left $ "Could not find " ++ show x

-- Builds a lambda expression using the list of tokens.
createExprFromTokens :: [ExprToken]
                        -> M.Map String OpInfo
                        -> M.Map String ([Expr] -> Expr)
                        -> [String]
                        -> Either [Char] Expr
createExprFromTokens tokens env defs vars = 
     buildTreeFromTokens tokens env >>= buildLambdaExpr vars defs

-- Takes the function declaration type and builds a definition.
createDefinition decl env tEnv defs = 
    let opInfo' = opInfo decl
        name' = name opInfo'
        vars =  if isRec opInfo' 
                then name':bindedVars decl
                else bindedVars decl
        binded = ((name', opInfo'):) 
                 . map (\x -> (x, defaultPrefix x)) 
                 $ bindedVars decl
        env' = foldr curryInsert env binded
    in do
      tree <- buildTreeFromTokens (definition decl) env' 
      expr <- buildLambdaExpr vars defs tree
      type' <- 
          if isRec opInfo' 
          then do 
            TFun x y <-  typeCheckFunction tEnv tree vars 
            if x == y then return x else fail "Types in recursive definition doesn't match."
          else typeCheckFunction tEnv tree vars 
      let expr' = foldr Lam expr vars
          recExpr = if isRec opInfo' 
                    then App yComb expr'
                    else expr'
      return (recExpr, opInfo decl, type')
    where curryInsert (x,y) = M.insert x y
          -- The 1000 means "resolve the introduced vars last (ie. low precedence)"
          defaultPrefix name = createOp 1000 name LeftA Prefix False 0 

getDefinitions env' tEnv defs' [] = return (env', defs', tEnv)
getDefinitions env' tEnv defs' (decl:decls) = 
    do
      (def, opInfo,t) <- createDefinition decl env' tEnv defs' 
      let name' = name opInfo 
          env'' = (M.insert name' opInfo env') 
          tEnv' = M.insert name' (Scheme (S.toList $ ftv t) t) tEnv
      case fix opInfo of -- It's necessary to add a dummy operator in case of closed operator
        Close x -> 
            let openName = opInfo {fix = Open name'}
                env''' = M.insert x openName env''
                defs'' = M.insert name' (\x -> applyArgs def x) defs'
            in getDefinitions env''' tEnv' defs'' decls
        Open x -> 
            let closedName = opInfo {fix = Close name'}
                env''' = M.insert x closedName env''
                defs'' = M.insert name' (\x -> applyArgs def x) defs'
            in getDefinitions env''' tEnv' defs'' decls
        _ -> getDefinitions env'' tEnv' (M.insert name' (\x -> applyArgs def x) defs') decls

createProgram env defs (declarations,main) = do
  (env', defs', types) <- getDefinitions env primitiveTypes defs declarations
  tree <- buildTreeFromTokens main env' 
  expr <- buildLambdaExpr [] defs' tree
  type' <- typeCheckExpr types tree
  program <- createExprFromTokens main env' defs' []
  return (program, type')

showTypes s = do
  (decls,main) <- parseProgramWrap s
  (_,_,types)  <- getDefinitions environment primitiveTypes definitions decls
  return $ M.map (\(Scheme _ x) -> x) types

--executeProgram :: String -> Either [Char] Expr
executeProgram s = do
    parseTree <- parseProgramWrap s
    (program, type') <- createProgram environment definitions parseTree
    unsafePerformIO (putStrLn $ "Type: " ++ show type') -- Temporal thing
                        `seq` return $ whnf program
--    return $ (show type') ++ "\n"  ++ (show $ whnf program) -- Later we should put this in the 

ppExpression = ()

-- liftM whnf 
                   -- $ parseProgramWrap s
                   -- >>= createProgram environment definitions

--------------------------------------------------
-- Test environment
--------------------------------------------------
createOpTuple p n a f = (n, OpInfo {precedence = p, name = n,
                                    assoc = a, fix = f, isRec = False, arity = 2})

plusS       = createOpTuple 1 "+"  LeftA Infix
minusS      = createOpTuple 1 "-"  LeftA Infix
timesS      = createOpTuple 2 "*"  RightA Infix
ifThenElse  = createOpTuple 2 "ifThenElse" RightA Prefix
buildPair'  = createOpTuple 2 "buildPair" RightA Prefix
first'      = createOpTuple 2 "fst" RightA Prefix
second'     = createOpTuple 2 "snd" RightA Prefix
true'       = createOpTuple 2 "true" RightA Prefix
false'      = createOpTuple 2 "false" RightA Prefix
equals'     = createOpTuple 2 "==" RightA Infix
lParenS     = createOpTuple 5 "("  LeftA  (Open  ")") 
rParenS     = createOpTuple 5 ")"  RightA (Close "(")

primitiveTypes = 
    M.fromList [("(",Scheme ["b0"] $ TFun  (TVar "b0") (TVar "b0")),
                ("true", Scheme [] TBool),
                ("false", Scheme [] TBool),
                ("+", Scheme []       $ TFun  TInt  (TFun TInt TInt)),
                ("*", Scheme []       $ TFun  TInt  (TFun TInt TInt)),
                ("-", Scheme []       $ TFun  TInt  (TFun TInt TInt)),
                ("==",Scheme []       $ TFun  TInt  (TFun TInt TBool)),
                ("buildPair",Scheme ["a", "b"] $ TFun  (TVar "a") (TFun (TVar "b") (TProd (TVar "a") (TVar "b")))),
                ("fst",Scheme ["a", "b"]       $ TFun  (TProd (TVar "a") (TVar "b")) (TVar "a")),
                ("snd",Scheme ["a", "b"]       $ TFun  (TProd (TVar "a") (TVar "b")) (TVar "b")),
                ("ifThenElse", Scheme ["b1"] $ TFun TBool (TFun (TVar "b1") (TFun (TVar "b1") (TVar "b1"))))
               ]


environment = M.fromList [plusS, timesS, minusS, 
                          ifThenElse, true', false', 
                          equals', lParenS, rParenS, buildPair', first', second']
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
     ("buildPair", binary buildPair),
     ("fst", unary first),
     ("snd", unary second),
     ("true", const true),
     ("false", const false),
     ("ifThenElse", ifthenelse)
    
    ]

int = Value . IPrim