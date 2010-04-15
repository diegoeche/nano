module Evaluator(createDefinition,
                 evalExpression,
                 addDeclToEnv) where
import System.IO.Unsafe
import qualified Data.Set as S
import qualified Data.Map as M
import Data.List
import Data.Maybe
import AlgorithmW
import Analyser
import Control.Monad
import LambdaCalculus
import Parser
import TypeChecker
import Environment
import Data.Either (partitionEithers)
applyArgs :: Expr -> [Expr] -> Expr
applyArgs = foldl App

-- Builds a lambda expression using the tree of operator calling.
-- The previous definitions of operators. And the binded variables.
buildLambdaExpr :: [String]
                   -> M.Map String ([Expr] -> Expr)
                   -> ExprTree
                   -> Either String Expr
buildLambdaExpr binded env expr = buildLambdaExpr' expr
    where buildLambdaExpr' (Value (IPrim n)) = return $ Const $ IData n
          buildLambdaExpr' (Value (SPrim s)) = return $ Const $ SData s
          buildLambdaExpr' (Call x args) | elem x binded =
              do
                lambdaArgs <- mapM buildLambdaExpr' args
                return $ applyArgs (Var x) lambdaArgs
          buildLambdaExpr' (Call x args) =
              do
                lambdaArgs <- mapM buildLambdaExpr' args
                def <- lookupE x env
                return $ def lambdaArgs
          lookupE x e = case M.lookup x e of
                         Just v -> Right v
                         _      -> Left $ " Could not find " ++ show x

-- Builds a lambda expression using the list of tokens.
-- createExprFromTokens tokens env defs vars =
--      buildTreeFromTokens tokens env >>= map (buildLambdaExpr vars defs)

-- Takes the function declaration type and builds a definition.
createDefinition decl env tEnv defs =
    let opInfo' = opInfo decl
        name' = name opInfo'
        vars =  if isRec opInfo'
                then name':bindedVars decl
                else bindedVars decl
        operator =
            case fix opInfo' of
              Close x -> [(x,opInfo'{fix = Open name', name = x}),
                          (name',opInfo')]
              Open x  -> [(x,opInfo'{fix = Close name', name = x}),
                          (name',opInfo')]
              _ -> [(name',opInfo')]
        binded = (operator ++)
                 . map (\x -> (x, defaultPrefix x))
                 $ bindedVars decl
        env' = foldr curryInsert env binded
    in
--      let (bleErrs, exprs) = partitionEithers $ map (buildLambdaExpr vars defs) trees
      case buildTreeFromTokens (definition decl) env' of
          Left error -> Left error
          Right trees -> do
                let slns = do
                          tree <- trees
                          let expr = buildLambdaExpr vars defs tree
                          case expr of
                            Left error -> fail error
                            Right expr -> do
                                  let type' = if isRec opInfo'
                                              then do
                                                   TFun x y <-  typeCheckFunction tEnv tree vars
                                                   if x == y
                                                      then return x
                                                      else fail $ "Types in recursive definition doesn't match.\n"
                                                                ++ (show x) ++ " and " ++ (show y)
                                              else typeCheckFunction tEnv tree vars
                                  return (tree, type', expr)

                let correctSlns = do
                    (tr, Right ty, exp) <- slns
                    return (tr, ty, exp)
                case correctSlns of
                 [(tree, type', expr)] -> do
                                 let expr' = foldr Lam expr vars
                                     recExpr = if isRec opInfo'
                                           then App yComb expr'
                                           else expr'
                                 return (recExpr, opInfo decl, type')
                 (i1: i2:_) -> fail $ unlines ("Multiple possible interpretations like:": map show [i1, i2])

    where curryInsert (x,y) = M.insert x y
          -- The 1000 means "resolve the introduced vars last (ie. low precedence)"
          defaultPrefix name' = createOp 1000 name' LeftA Prefix False 0

getDefinitions :: M.Map String OpInfo
                  -> M.Map String Scheme
                  -> M.Map String ([Expr] -> Expr)
                  -> [Declaration]
                  -> Either
                       String
                       (M.Map String OpInfo,
                        M.Map String ([Expr] -> Expr),
                        M.Map String Scheme)
getDefinitions env tEnv defs [] = return (env, defs, tEnv)
getDefinitions env tEnv defs (decl:decls) = do
    (env', tEnv', defs', _) <- addDeclToEnv env tEnv defs decl
    getDefinitions env' tEnv' defs' decls

-- Adds a definition to the environment and returns
-- The environment as a tuple.
addDeclToEnv :: M.Map String OpInfo
                -> M.Map String Scheme
                -> M.Map String ([Expr] -> Expr)
                -> Declaration
                -> Either
                     String
                     (M.Map String OpInfo,
                      M.Map String Scheme,
                      M.Map String ([Expr] -> Expr),
                      Type)
addDeclToEnv ops tEnv defs decl =
    do
      (def, opInfo',t) <- createDefinition decl ops tEnv defs
      let name' = name opInfo'
          ops' = (M.insert name' opInfo' ops)
          tEnv' = M.insert name' (Scheme (S.toList $ ftv t) t) tEnv

          defs'' =   M.insert name' (\x -> applyArgs def x) defs
      case fix opInfo' of -- It's necessary to add a dummy operator in case of closed operator
        Close x ->
            let openName = opInfo' {fix = Open name'}
                ops'' = M.insert x openName ops'
            in return (ops'', tEnv', defs'', t)
        Open x ->
            let closedName = opInfo' {fix = Close name'}
                ops'' = M.insert x closedName ops'
            in return (ops'', tEnv', defs'', t)
        _ -> return (ops', tEnv', defs'', t)

evalExpression ops tEnv defs main = do
  trees <- buildTreeFromTokens main ops
  let (mexpr,mtype')  = head $ do
                        tree <- trees
                        return  (buildLambdaExpr [] defs tree, typeCheckExpr tEnv tree)
  type' <- mtype'
  expr  <- mexpr
  return (type', whnf expr)

-- createProgram env defs (declarations,main) = do
--   (env', defs', types') <- getDefinitions env types defs declarations
--   trees <- buildTreeFromTokens main env'
--   case map (typeCheckExpr types') trees of
--     type':[] -> do
--       program <- createExprFromTokens main env' defs' []
--       return (program, type')

showTypes :: String -> Either String (M.Map String Type)
showTypes s = do
  (decls,_) <- parseProgramWrap s
  (_,_,types')  <- getDefinitions operators types definitions decls
  return $ M.map (\(Scheme _ x) -> x) types'

-- --executeProgram :: String -> Either String Expr
-- executeProgram :: String -> Either String Expr
-- executeProgram s = do
--     parseTree <- parseProgramWrap s
--     (program, _) <- createProgram operators definitions parseTree
-- --    unsafePerformIO (putStrLn $ "Type: " ++ show type') `seq`-- Temporal thing
--     return $ whnf program

