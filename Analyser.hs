-- Nano programming language.
-- Bug reports to Diego Echeverri at diegoeche@gmail.com
{- Notes:

This module is the one that builds up the expression tree from a
list of tokens

February 5:
The idea of using an infix and postfix operator for emulating distfix operators like:
[_] has some troubles. This the "hard" example "2 * ( 3 ) + 2 " the argument of `(`
would be 3 ) + 2

We will just offer an operator of the form [_]
-}

{-# LANGUAGE PackageImports, NoMonomorphismRestriction #-}

module Analyser (split, buildTree, buildTreeFromTokens, pPrinter,
                 SElement(SFunction, SInteger, SPartial),
                 ExprTree(Value, Call),
                 sAnalyse,
                 Atom(IPrim, SPrim)) where

import Control.Applicative ((<$>))
import "mtl" Control.Monad.Error hiding (fix)
import qualified Data.Map as M hiding (split)
import qualified Data.Set as Set
import System.IO.Unsafe
import Data.List
import Data.Maybe
import Data.Either
import Data.Char
import Parser

--------------------------------------------------
-- Language Elements
--------------------------------------------------

-- Semantic Elements. Used for resolving fixity.
data SElement = SInteger Integer
              | SString String
              | SFunction OpInfo -- Let's exclude Strings for the moment.
              | SPartial ExprTree -- Partially builded trees.
               deriving (Show,Eq)

-- We need this to make the split work.
instance Ord SElement where
    compare (SInteger _) _ = LT
    compare _ (SInteger _) = GT
    compare (SString _) _ = LT
    compare _ (SString _) = GT
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
tokenToSElement _ (LiteralToken (NString s))    = return $ SString  s
tokenToSElement _ (LiteralToken (NInteger int)) = return $ SInteger int
tokenToSElement env (FunctionToken x) =
    SFunction <$> (maybeToM $ M.lookup x env)

-- Lookup of expression Elements.
-- If we cannot resolve any of the symbols we fail.
sAnalyse :: M.Map String OpInfo -> [ExprToken] -> [[SElement]]
sAnalyse env tokens =
    map concat
    $ mapM t tokens
    where t (LiteralToken (NInteger int)) = return $ return $ SInteger int
          t (LiteralToken (NString s))    = return $ return $ SString s
          t (FunctionToken x) = do
             monad <- resolveOperator x env
             operators <- monad
             mapM (tokenToSElement env) operators

isValidToken :: String -> Set.Set String -> Bool
isValidToken i identifiers =
    case parseTokenWrap i of
      Just (FunctionToken n) | Set.member n identifiers -> True
      Just (LiteralToken _)                             -> True
      _                                                 -> False

-- Returns the possible operators given the environment and the string
resolveOperator :: (Monad m) =>
                   [Char] -> M.Map String a -> m [[ExprToken]]
resolveOperator operator env =
    mapM (mapM parseTokenWrap)
    . take 10 -- Just for reducing the amount of cases to try.
    -- For solving: "+++"
    -- We get: [["+++"],["++","+"],["+","+","+"]]
    -- This filter takes care of those cases.
    . filter (isValid . head)
    $ split' operator
    where isValid = flip isValidToken keys
          keys = Set.fromList . M.keys $ env
          split' [] = [] -- Thanks Joel!
          split' (y:[]) = [[[y]]]
          split' (y:ys) =  concatMap (merge y) (split' ys)
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

-- The structure behind the types and values?
data Atom = IPrim Integer
          | SPrim String
            deriving (Show,Eq)

-- The structure for evaluating expressions.
data ExprTree = Value Atom
              | Call String [ExprTree]
                deriving (Show,Eq)

-- This function is used for building the Expression Tree
split :: (Ord a, Monad m) => [a] -> m ([a], a, [a])
split ([]) = fail "Cannot split empty list"
split (x:xs) = return $ splitAccum [] x [] xs
    where
      splitAccum before value after [] = (before,value,after)
      splitAccum b v a (x':xs') =
          if x' > v then splitAccum (b ++ [v] ++ a) x' [] xs'
          else splitAccum b v (a ++ [x']) xs'

-- Generates SPartial constructors from the distfix operators in the
-- expression
resolveDistfix :: [SElement] -> Either String [SElement]
resolveDistfix xs =
    resolveDistfix' Nothing [] [] xs
    where resolveDistfix' Nothing before _ [] = return before
          resolveDistfix' Nothing before _ ((SFunction opInfo'):xs')
              | isOpen  $ fix opInfo' = resolveDistfix'(Just opInfo') before [] xs'
              | isClose $ fix opInfo' = fail $ "Found close operator "
                                       ++ name opInfo' ++ "with no open operator"
                where isClose (Close _) = True
                      isClose _  = False
                      isOpen (Open _) = True
                      isOpen _  = False
          resolveDistfix' Nothing before _ (x:xs') = resolveDistfix' Nothing (before ++ [x]) [] xs'
          resolveDistfix' current@(Just opInfo') before after x =
              case x of
                [] -> fail $ "Expression terminated while expecting closing operator of "
                      ++ name opInfo'
                (x'@(SFunction opInfo2)):xs' ->
                    case fix opInfo2 of
                      Open _ ->
                          resolveDistfix' (Just opInfo2) (before ++ [SFunction opInfo'] ++ after ) [] xs'
                      Close open | open == name opInfo' ->
                                     do inside <- buildExprTree after
                                        resolveDistfix $ before ++ [SPartial $ Call open inside] ++ xs'
                      Close x'' -> fail $ "Closing operator: " ++ x'' ++ " found while expecting closing operator of "
                                 ++ name opInfo'
                      _ -> resolveDistfix' current before (after ++ [x']) xs'
                x':xs' -> resolveDistfix' current before (after ++ [x']) xs'

-- Builds a tree of integer values
buildExprTree :: [SElement] -> Either String [ExprTree]
buildExprTree [] = fail "Cannot build expression tree"
buildExprTree x | all isTree x = return $ map toTree x
    where isTree (SInteger _) = True
          isTree (SString _)  = True
          isTree (SPartial _) = True
          -- This cover the case of variables
          isTree (SFunction opInfo') = arity opInfo' == 0
          toTree (SInteger n) = Value $ IPrim n
          toTree (SString  s) = Value $ SPrim s
          toTree (SPartial t) = t
          toTree (SFunction opInfo') = Call (name opInfo') []
buildExprTree xs = do
    (before, SFunction opInfo', after) <- split xs
    let opName = (name opInfo')
    case ((fix opInfo'), before, after) of
      (Infix, [], _)  -> fail $ "Expecting left argument of " ++ opName
      (Infix, _, [])  -> fail $ "Expecting right argument of " ++ opName
                   -- fail $ "Expecting argument of " ++ opName
      (Prefix, _, []) ->
          if arity opInfo' == 0 then
              return [Call opName []] -- Test
          else fail $ "Expecting argument of " ++ opName
      (Suffix, [], _) -> fail $ "Expecting argument of " ++ opName
      (Infix, _, _) -> do par1 <- buildExprTree before
                          par2 <- buildExprTree after
                          return [Call opName (par1 ++ par2)]
      (Prefix, [], _) -> do
                     right <- buildExprTree after
                     let (argsN, n) = (arity opInfo', length right)
                     if argsN == n
                       then
                           return [Call opName right]
                       else fail $ "Expecting " ++ (show argsN)
                                ++ " But found " ++ (show n) ++ " args"

      (Prefix, _, _) -> do right <- buildExprTree after
                           buildExprTree $ before ++ [(SPartial $ Call opName right)]
      (Suffix, _, []) -> do
                     left <- buildExprTree before
                     let (argsN, n) = (arity opInfo', length left)
                     if argsN == n
                           then
                              return [Call opName left]
                           else fail $ "Expecting " ++ (show argsN)
                                    ++ " But found " ++ (show n) ++ "args"
      (Suffix, _, _)-> do left <- buildExprTree before
                          buildExprTree $ (SPartial $ Call opName left) : after
      (_, _, _)     -> fail $ "Unexpected operator" ++ show xs


pPrinter :: M.Map String Parser.OpInfo
            -> ExprTree
            -> [String]
pPrinter _   (Value (IPrim n))  = return . show $ n
pPrinter _   (Value (SPrim s))  = return . show $ s
pPrinter env (Call op params) =
    do
      opInfo' <- maybeToList . M.lookup op $ env
      values <- mapM (pPrinter env) params
      format (fix opInfo') values
      where format Prefix x = return $ op ++ " " ++ lformat x
            format Suffix x = return $ lformat x ++ " "  ++ op
            format (Open close) x = return $ intercalate " " [op, (lformat x), close]
            format (Close _) x = format (Open op) x
            format Infix (x1:x2:_) = return $ x1  ++ op ++ x2
            format Infix (_) = fail "Cannot format Infix with more than two args"
            lformat = intercalate ","

buildTree s env = do
     parsed <- parseWrap s
     let noDistfix = map resolveDistfix $ sAnalyse env parsed
     case partitionEithers noDistfix of
         (l,[])       -> Left $ "Could not build tree from the: " ++ s
                         ++ " expression\n" ++ intercalate "\n" l
         (_,x)        ->
             case partitionEithers $ map buildExprTree x of
               (l,[])     -> Left $ "Could not build tree from the: " ++ s
                             ++ " expression\n" ++ intercalate "\n" l
               (_, t)  -> Right $ concat t

               -- (_,[[t]])    -> Right t
               -- (_,i1:i2:_)  -> Left $ "Ambiguous definition of: " ++ s
               --                  ++ intercalate "\n" ["\nPossible interpretations:", show i1, show i2]
               -- (_,_)      -> Left "This case was not covered by Analyser.buildTree"


buildTreeFromTokens
  :: [ExprToken] -> M.Map String OpInfo -> Either [Char] [ExprTree]
buildTreeFromTokens tokens env =
     let noDistfix = map resolveDistfix $ sAnalyse env tokens
     in
       case partitionEithers noDistfix of
           (l,[])       -> Left $ "Could not build tree from tokens: " ++ show tokens
                           ++ " expression\n" ++ intercalate "\n" l
           (_,x)        ->
               case partitionEithers $ map buildExprTree x of
                 (l,[])     -> Left $ "Could not build tree from the: " ++ show tokens
                             ++ "expression\n" ++ intercalate "\n" l
                 (_,s)    -> Right $ concat s
                 -- (_,[[s]])    -> Right s
                 -- (_,i1:i2:_)  -> Left $ "Ambiguous definition of: " ++ show tokens
                 --                 ++ intercalate "\n" ["\nPossible interpretations:", show i1, show i2]

                 -- (_,_)      -> Left $ "This case was not covered by Analyser.buildTreeFromTokens" ++ show tokens
