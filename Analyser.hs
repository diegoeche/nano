-- Nano programming language.
-- Bug reports to Diego Echeverri at diegoeche@gmail.com
{- Notes:

February 5:
The idea of using an infix and postfix operator for emulating distfix operators like:
[_] has some troubles. This the "hard" example "2 * ( 3 ) + 2 " the argument of `(` 
would be 3 ) + 2

We will just offer an operator of the form [_] 
-}

{-# LANGUAGE NoMonomorphismRestriction #-}

module Analyser (split)  where
import Control.Applicative ((<$>))
import Control.Monad.Error hiding (fix) 
import qualified Data.Map as M hiding (split) 
import qualified Data.Set as Set
import Data.List  
import Data.Maybe
import Data.Char
import Parser

--------------------------------------------------
-- Language Elements
--------------------------------------------------
-- Operator Definition
data Assoc = LeftA | RightA -- Lets take out this one for now | NeutralA
             deriving (Show,Eq)

--            f  a      f  a     a f b   
data Fixing = Suffix | Prefix | Infix  
            -- This ones will allow us to have closed operators.
            | Open String  -- I know this is redundant but is "handy"
            | Close String  
              deriving (Show,Eq)


-- Semantic Elements. Used for resolving fixity. 
data SElement = SInteger Integer 
              | SFunction OpInfo -- Let's exclude Strings for the moment.
              | SPartial ExprTree -- Partially builded trees.
               deriving (Show,Eq)

-- Information about operator
data OpInfo = OpInfo {
      name :: String,
      precedence :: Int,
      assoc :: Assoc,
      fix :: Fixing
    }deriving (Show,Eq)


-- We need this to make the split work.
instance Ord SElement where
    compare (SInteger _) _ = LT
    compare _ (SInteger _) = GT
    compare (SPartial _) _ = LT
    compare _ (SPartial _) = GT
    compare (SFunction info1) (SFunction info2) = 
        case compare (precedence info2) (precedence info1) of
          EQ -> 
              case ((assoc info1),(assoc info2)) of
                (RightA, RightA) -> LT
                (LeftA, LeftA) -> GT
                _ -> EQ
          x -> x

-- With this is possible to use a more general lookup
maybeToM :: (Monad m) => Maybe t -> m t
maybeToM (Just x)  = return x
maybeToM  Nothing  = fail "This Maybe failed"

tokenToSElement :: (Monad m, Functor m) =>
                   M.Map String OpInfo -> 
                   ExprToken -> 
                   m SElement
tokenToSElement _ (LiteralToken (NInteger int)) = return $ SInteger int 
tokenToSElement env (FunctionToken x) = 
    SFunction <$> (maybeToM $ M.lookup x env)

-- Lookup of expression Elements.
-- If we cannot resolve any of the symbols we fail.
-- sAnalyse :: (Monad m, Functor m) =>
--             M.Map String OpInfo -> 
--             [ExprToken] -> 
--             m [SElement]
sAnalyse env tokens = 
    map concat
    $ mapM t tokens 
    where t (LiteralToken (NInteger int)) = return $ return $ SInteger int
          t (FunctionToken x) = do
             monad <- resolveOperator x env
             operators <- monad
             mapM (tokenToSElement env) operators

isValidToken i identifiers =
    case parseTokenWrap i of 
      Just (FunctionToken name) | Set.member name identifiers -> True
      Just (LiteralToken _)                                   -> True
      _                                                       -> False

-- Returns the possible operators given the environment and the string

--resolveOperator :: (Monad m) =>
--                   [Char] -> M.Map String a -> m [[ExprToken]]
resolveOperator operator env =  
    mapM (mapM parseTokenWrap)
    . take 10 -- Just for reducing the amount of cases to try.
    -- For solving: "+++" 
    -- We get: [["+++"],["++","+"],["+","+","+"]]
    -- This filter takes care of those cases.
    . filter (isValid . head) 
    $ split operator
    where isValid = flip isValidToken keys
          keys = Set.fromList . M.keys $ env 
          split [] = [] -- Thanks Joel!
          split (y:[]) = [[[y]]]
          split (y:ys) =  concatMap (merge y) (split ys)
              where merge z [] = [[[z]]]
                    merge z (x:xs) =
                        let s1 = (z : x) : xs
                            s2 = [z] : x : xs         
                        in if isValidToken x keys 
                           -- Corner case. "1234" should return [["1234"]]
                           then if isDigit z && all isDigit x 
                                then [s1]
                                else [s1, s2]  
                           else [s1] 


-- This function will be used for building the Expression Tree
split :: (Ord a, Monad m) => [a] -> m ([a], a, [a])
split ([]) = fail "Cannot split empty list"
split (x:xs) = return $ splitAccum [] x [] xs
    where 
      splitAccum before value after [] = (before,value,after)
      splitAccum b v a (x:xs) = 
          if x > v then splitAccum (b ++ [v] ++ a) x [] xs 
          else splitAccum b v (a ++ [x]) xs

-- The structure behind the types and values?
data Function = IPrim Integer 
              | SPrim String 
              | Func Function Function 
                deriving (Show,Eq)

-- The structure for evaluating expressions.
data ExprTree = Value Function
              | Binary String ExprTree ExprTree 
              | App String ExprTree
                deriving (Show,Eq)

-- Generates SPartial constructors from the distfix operators in the
-- expression
-- Maybe wrap this inside of a Monad to avoid loosing the fail messages?
resolveDistfix :: [SElement] -> [SElement]
resolveDistfix xs = 
    resolveDistfix' Nothing [] [] xs
    where resolveDistfix' Nothing before after [] = before
          resolveDistfix' Nothing before after ((SFunction opInfo):xs) 
              | isOpen  $ fix opInfo = resolveDistfix'(Just opInfo) before [] xs
              | isClose $ fix opInfo = fail $ "Found close operator " 
                                       ++ name opInfo ++ "with no open operator"
                where isClose (Close _) = True
                      isClose _  = False
                      isOpen (Open _) = True
                      isOpen _  = False
          resolveDistfix' Nothing before _ (x:xs) = resolveDistfix' Nothing (before ++ [x]) [] xs
          resolveDistfix' current@(Just opInfo) before after x = 
              case x of 
                [] -> fail $ "Expression terminated while expecting closing operator of " 
                      ++ name opInfo 
                (x@(SFunction opInfo2)):xs ->
                    case fix opInfo2 of
                      Open _ -> resolveDistfix' (Just opInfo2) (before ++ [x] ++ after) [] xs  
                      Close open | open == name opInfo -> do
                                       inside <- buildExprTree after
                                       resolveDistfix $ before ++ [SPartial $ App open inside] ++ xs
                      Close x -> fail $ "Closing operator found while expecting closing operator of" 
                                 ++ name opInfo 
                      _ -> resolveDistfix' current before (after ++ [x]) xs
                x:xs -> resolveDistfix' current before (after ++ [x]) xs

-- Builds a tree of integer values 

buildExprTree [] = fail "Cannot build expression tree"
buildExprTree [SInteger n] = return $ Value $ IPrim n
buildExprTree [SPartial t] = return t
buildExprTree xs = do
    (before, SFunction opInfo, after) <- split xs
    let opName = (name opInfo) 
    case ((fix opInfo), before, after) of
      (Infix, [], _)  -> fail $ "Expecting left argument of " ++ opName
      (Infix, _, [])  -> fail $ "Expecting right argument of " ++ opName
      (Prefix, _, []) -> fail $ "Expecting argument of " ++ opName
      (Suffix, [], _) -> fail $ "Expecting argument of " ++ opName
      (Infix, _, _) -> do
                        par1 <- buildExprTree before
                        par2 <- buildExprTree after
                        return $ Binary opName par1 par2
      (Prefix, [], _) -> do
                        right <- buildExprTree after
                        return $ App opName right 
      (Prefix, _, _) -> do
                        right <- buildExprTree after
                        buildExprTree $ before ++ [(SPartial $ App opName right)]
      (Suffix, _, []) -> do
                        left <- buildExprTree before
                        return $ App opName left
      (Suffix, _, _)-> do
                        left <- buildExprTree before
                        buildExprTree $ (SPartial $ App opName left) : after


--data Value = 

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
eval (Value (IPrim n)) = n
eval (Binary "+" x y) = eval x + eval y
eval (Binary "*" x y) = eval x * eval y
eval (App "(" x) = eval x
eval (App "++" x) =  1 + eval x 


pPrinter _   (Value (IPrim n))  = return . show $ n
pPrinter _   (Value (SPrim s))  = return . show $ s
pPrinter env (Binary op x y)    =  
    do
      v1 <- pPrinter env x
      v2 <- pPrinter env y
      return $ intercalate " " [v1, op, v2]
pPrinter env (App op x) =      
    do 
      opInfo <- maybeToList . M.lookup op $ env
      value <- pPrinter env x
      format (fix opInfo) value
      where format Prefix = \x -> return $ op ++ " " ++ x
            format Suffix = \x -> return $ x ++ " "  ++ op
            format (Open close) = \x -> return $ intercalate " " [op, x, close]
            format (Close open) = format $ Open op




--test :: Either [Char] SElement
evalExpr s = 
    liftM eval (parseWrap s
                >>= sAnalyse environment 
                >>= (buildExprTree . resolveDistfix)) 

pp s = parseWrap s
       >>= sAnalyse environment 
       >>= (buildExprTree . resolveDistfix)
       >>= pPrinter environment 


test2 s = 
    parseWrap s
    >>= sAnalyse environment 



{-
Tests:
test "3 + ( ( ( ( 1 + 2 ) + 4 + 3 * 2 + 4 + ( 7 + 2 ) ) + 3 + 17 * 6 + 27 ) + 3 )"
164
test "( 1 + 3 ) + ( 3 * 2 ) + ( 7 + 5 ) + ( 8 + 9 ) + ( 6 * 7 ) + ( 3 + 3 ) + ( 2 + 3 )"
92
test "(1+3)+(3*2)+( 7 + 5 ) + ( 8 + 9 ) + ( 6 * 7 ) + ( 3 + 3 ) + ( 2 + 3 )"
92

-}