-- Nano programming language.
-- Bug reports to Diego Echeverri at diegoeche@gmail.com
{-# LANGUAGE NoMonomorphismRestriction #-}

{- Notes:

February 5:
The idea of using an infix and postfix operator for emulating distfix operators like:
[_] has some troubles. This the "hard" example "2 * ( 3 ) + 2 " the argument of `(` 
would be 3 ) + 2

We will just offer an operator of the form [_] 
-}
module Nano (parse, split) where
 
import qualified Text.ParserCombinators.Parsec.Token as P
import Text.ParserCombinators.Parsec.Language 
import Text.Parsec
import Control.Applicative ((<$>))
import Control.Monad.Error hiding (fix) 
import qualified Data.Map as M

--------------------------------------------------
-- Language Elements
--------------------------------------------------
-- Operator Definition
data Assoc = LeftA | RightA -- Lets take out this one for now | NeutralA
             deriving (Show,Eq)

--            f  a      f  a     a f b   
data Fixing = Suffix | Prefix | Infix  
            | Open   | Close String  -- This ones will allow us to 
              deriving (Show,Eq)

-- Information about operator
data OpInfo = OpInfo {
      name :: String,
      precedence :: Int,
      assoc :: Assoc,
      fix :: Fixing
    }deriving (Show,Eq)

-- Literal Definition. In the moment just strings 
-- and integers 
data Literal = NString String | NInteger Integer 
               deriving Show

-- Expression Definition
data ExprToken = LiteralToken Literal | FunctionToken String
                 deriving Show

-- The key for our programming language. 
-- Permissive identifiers
lexer :: P.TokenParser ()
lexer  = P.makeTokenParser
         (emptyDef {
            identStart = oneOf $ filter (`notElem` digits) allChars,
            identLetter = oneOf allChars
          })
         where allChars = filter (`notElem` "\'\"") ['!'..'~']
               digits = ['0'..'9']

-- Aliases for common lexer parsers
pString =  P.stringLiteral lexer
pNatural =  P.natural lexer
pIdentifier = P.identifier lexer

pLiteral = 
        NString <$> pString 
    <|> NInteger <$> pNatural

-- Parse expression using the given environment.
-- pExprElems :: ParsecT String () Control.Monad.Identity.Identity [ExprToken]
pExprElems = many (LiteralToken <$> pLiteral
                   <|> FunctionToken <$> pIdentifier)

-- To get standard monad handling of errors.
parseWrap :: (Monad m) => String -> m [ExprToken]
parseWrap s = wrap $ parse pExprElems "" s
    where 
          wrap (Left _) = fail "Parse Error!"    
          wrap (Right x) = return x

-- Semantic Elements. 
data SElement = SInteger Integer 
              | SFunction OpInfo -- Let's exclude Strings for the moment.
              | SPartial IntegerExprTree -- Partially builded trees.
               deriving (Show,Eq)

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

-- Lookup of expression Elements.
-- If we cannot resolve any of the symbols we fail.
sAnalyse :: (Monad m, Functor m) =>
            M.Map String OpInfo -> [ExprToken] -> m [SElement]
sAnalyse env tokens = mapM t tokens 
    where t (LiteralToken (NInteger int)) = return $ SInteger int 
          t (FunctionToken x) =  SFunction <$> (maybeToM $ M.lookup x env)

-- This function will be used for building the Expression Tree
split :: (Ord a, Monad m) => [a] -> m ([a], a, [a])
split ([]) = fail "Cannot split empty list"
split (x:xs) = return $ splitAccum [] x [] xs
    where 
      splitAccum before value after [] = (before,value,after)
      splitAccum b v a (x:xs) = 
          if x > v then splitAccum (b ++ [v] ++ a) x [] xs 
          else splitAccum b v (a ++ [x]) xs

data IntegerExprTree = Value Integer 
                     | Binary String IntegerExprTree IntegerExprTree 
                     | Unary String IntegerExprTree
                       deriving (Show,Eq)


--------------------------------------------------
-- Test environment
--------------------------------------------------
createOpTuple p n a f = (n, OpInfo {precedence = p, name = n,
                                    assoc = a, fix = f})

plusS     = createOpTuple 1 "+" LeftA Infix
timesS    = createOpTuple 2 "*" RightA Infix
lParenS   = createOpTuple 5 "(" LeftA Open
rParenS   = createOpTuple 5 ")" RightA (Close "(")

environment = M.fromList [plusS,timesS,lParenS,rParenS]

-- Generates SPartial constructors from the distfix operators in the
-- expression
-- Maybe wrap this inside of a Monad to avoid loosing the fail messages?
resolveDistfix :: [SElement] -> [SElement]
resolveDistfix xs = 
    resolveDistfix' Nothing [] [] xs
    where resolveDistfix' Nothing before after [] = before
          resolveDistfix' Nothing before after ((SFunction opInfo):xs) 
              | fix opInfo == Open = resolveDistfix'(Just opInfo) before [] xs
              | isClose $ fix opInfo = fail $ "Found close operator " 
                                       ++ name opInfo ++ "with no open operator"
                where isClose (Close _) = True
                      isClose _  = False
          resolveDistfix' Nothing before _ (x:xs) = resolveDistfix' Nothing (before ++ [x]) [] xs
          resolveDistfix' current@(Just opInfo) before after x = 
              case x of 
                [] -> fail $ "Expression terminated while expecting closing operator of " 
                      ++ name opInfo 
                (x@(SFunction opInfo2)):xs ->
                    case fix opInfo2 of
                      Open -> resolveDistfix' (Just opInfo2) (before ++ [x] ++ after) [] xs  
                      Close open | open == name opInfo -> do
                                       inside <- buildIntExprTree after
                                       resolveDistfix $ before ++ [SPartial $ Unary open inside] ++ xs
                      Close x -> fail $ "Closing operator found while expecting closing operator of" 
                                 ++ name opInfo 
                      _ -> resolveDistfix' current before (after ++ [x]) xs
                x:xs -> resolveDistfix' current before (after ++ [x]) xs

-- Builds a tree of integer values 
buildIntExprTree :: (Monad m) => [SElement] -> m IntegerExprTree
buildIntExprTree [] = fail "Cannot build expression tree"
buildIntExprTree [SInteger n] = return $ Value n
buildIntExprTree [SPartial t] = return t
buildIntExprTree xs = do
    (before, SFunction opInfo, after) <- split xs
    let opName = (name opInfo) 
    case ((fix opInfo), before, after) of
      (Infix, [], _)  -> fail $ "Expecting left argument of " ++ opName
      (Infix, _, [])  -> fail $ "Expecting right argument of " ++ opName
      (Prefix, _, []) -> fail $ "Expecting argument of " ++ opName
      (Suffix, [], _) -> fail $ "Expecting argument of " ++ opName
      (Infix, _, _) -> do
                        par1 <- buildIntExprTree before
                        par2 <- buildIntExprTree after
                        return $ Binary opName par1 par2
      (Prefix, [], _) -> do
                        right <- buildIntExprTree after
                        return $ Unary opName right 
      (Prefix, _, _) -> do
                        right <- buildIntExprTree after
                        buildIntExprTree $ before ++ [(SPartial $ Unary opName right)]
      (Suffix, _, []) -> do
                        left <- buildIntExprTree before
                        return $ Unary opName left
      (Suffix, _, _)-> do
                        left <- buildIntExprTree before
                        buildIntExprTree $ (SPartial $ Unary opName left):after
      
-- Temporary function for eval 
eval (Value n) = n
eval (Binary "+" x y) = eval x + eval y
eval (Binary "*" x y) = eval x * eval y
eval (Unary "(" x) = eval x
eval (Unary ")" x) = eval x

--test :: Either [Char] SElement
test s = 
    liftM eval (parseWrap s
                >>= sAnalyse environment 
                >>= (buildIntExprTree . resolveDistfix)) 

{-
Tests:
test "3 + ( ( ( ( 1 + 2 ) + 4 + 3 * 2 + 4 + ( 7 + 2 ) ) + 3 + 17 * 6 + 27 ) + 3 )"
164
test "( 1 + 3 ) + ( 3 * 2 ) + ( 7 + 5 ) + ( 8 + 9 ) + ( 6 * 7 ) + ( 3 + 3 ) + ( 2 + 3 )"
92
-}