-- Nano programming language.
-- Bug reports to Diego Echeverri at diegoeche@gmail.com
{-
The first step of our pl is to create a list of tokens with both Literals and
identifiers.
-}

{-# LANGUAGE NoMonomorphismRestriction, PackageImports #-}

module Parser (parseWrap,
               parseTokenWrap,
               parseProgramWrap,
               Assoc(LeftA, RightA),
               fix, precedence, name,isRec,arity,
               assoc, OpInfo(OpInfo),
               Declaration(Decl),opInfo,
               createOp,
               bindedVars,
               definition,
               Fixing(Suffix, Prefix, Infix, Open, Close),
               Literal(NString, NInteger),
               ExprToken(LiteralToken, FunctionToken),
               pCommandWrap
              ) where

import qualified Text.ParserCombinators.Parsec.Token as P
import Text.ParserCombinators.Parsec.Language
import Text.Parsec
import Control.Applicative ((<$>),(<*),pure)
import Control.Monad (liftM)
import qualified "mtl" Control.Monad.Identity as I

-- Literal Definition. In the moment just strings
-- and integers
data Literal = NString String | NInteger Integer
               deriving (Show, Eq)

-- Expression Definition
data ExprToken = LiteralToken Literal | FunctionToken String
                 deriving (Show, Eq)

-- Information about operator
data OpInfo = OpInfo {
      name :: String,
      precedence :: Integer,
      assoc :: Assoc,
      fix :: Fixing,
      isRec :: Bool,
      arity :: Int
    } deriving (Eq)

type ParsecMonad a = ParsecT String () I.Identity a

instance Show OpInfo where
    show x =
        case fix x of
          Open o -> name x ++ o
          Close c -> name x ++ c
          _ -> name x

-- Function Declaration
data Declaration = Decl {opInfo :: OpInfo,
                         bindedVars :: [String],
                         definition :: [ExprToken]
                        } deriving (Show,Eq)

-- Creates an OpInfo
createOp :: Integer
            -> String
            -> Assoc
            -> Fixing
            -> Bool
            -> Int
            -> OpInfo
createOp p n a f r ar = OpInfo {precedence = p, name = n,
                               assoc = a, fix = f, isRec = r,
                               arity = ar
                              }

-- The key for our programming language.
-- Permissive identifiers
lexer :: P.TokenParser ()
lexer  = P.makeTokenParser
         (emptyDef {
            reservedNames = ["infixr", "infixl", "closed", "suffix",
                             "let", "=", "main", "rec"],
            identStart = oneOf $ filter (`notElem` digits) allChars,
            identLetter = oneOf allChars
          })
         where allChars = filter (`notElem` "\'\"") ['!'..'~']
               digits = ['0'..'9']

-- Aliases for common lexer parsers
pString :: ParsecMonad String
pString =  P.stringLiteral lexer

pNatural :: ParsecMonad Integer
pNatural =  P.natural lexer

pIdentifier :: ParsecMonad String
pIdentifier = P.identifier lexer

reserved :: String -> ParsecMonad ()
reserved = P.reserved lexer

-- Reserved words parsers.
pClosedW :: ParsecMonad ()
pSuffixW :: ParsecMonad ()
pLetW :: ParsecMonad ()
pEqualsW :: ParsecMonad ()
pMainW :: ParsecMonad ()
pRecW :: ParsecMonad ()
[pClosedW, pSuffixW, pLetW, pEqualsW, pMainW, pRecW] =
    map (P.reserved lexer)
            ["closed", "suffix", "let", "=", "main", "rec"]

pRec :: ParsecMonad Bool
pRec = option False (pure True <$> pRecW)

pInfix :: ParsecMonad Assoc
pInfix =
        pure RightA <$> reserved "infixr"
    <|> pure LeftA <$> reserved "infixl"


pLiteral :: ParsecMonad Literal
pLiteral =
        NString <$> pString
    <|> NInteger <$> pNatural

parseToken :: ParsecMonad ExprToken
parseToken = LiteralToken <$> pLiteral
             <|> FunctionToken <$> pIdentifier

--------------------------------------------------
-- Concrete Syntax for declarations
--------------------------------------------------
pExprElems :: ParsecMonad [ExprToken]
pExprElems = many parseToken

pDefinition :: ParsecMonad [ExprToken]
pDefinition = pEqualsW >> pExprElems


pSuffixDef :: ParsecMonad Declaration
pSuffixDef = do
  pLetW
  isRec' <- pRec
  pSuffixW
  name':params <- liftM reverse $ many pIdentifier
  def <- pDefinition
  return Decl {opInfo = createOp 3 name' LeftA Suffix isRec' $ length params,
               bindedVars = reverse params,
               definition = def}

pPrefixDef :: ParsecMonad Declaration
pPrefixDef = do
  pLetW
  isRec' <- pRec
  prec <- option 3 pNatural
  name':params <- many pIdentifier
  def <- pDefinition
  let a = length params
      -- Basically with high precedence, vars are going to be
      -- evaluated last. HACK
      p = if a == 0 then 1000 else prec
  return Decl {opInfo = createOp p name' LeftA Prefix isRec' a,
               bindedVars = params,
               definition = def}

-- For the moment we only accept infix operators with two args.
-- Maybe later we should explore if this restriction is necessary.
pInfixDef :: ParsecMonad Declaration
pInfixDef = do
  pLetW
  isRec' <- pRec
  assoc' <- pInfix
  prec <- option 3 pNatural
  [p1,op,p2] <- many pIdentifier
  def <- pDefinition
  return Decl {opInfo = createOp prec op assoc' Infix isRec' 2,
               bindedVars = [p1,p2],
               definition = def}

pClosedDef :: ParsecMonad Declaration
pClosedDef = do
  pLetW
  isRec' <- pRec
  pClosedW
  open:rest <- many pIdentifier
  let close: args = reverse rest
  def <- pDefinition
  return Decl {opInfo = createOp 3 open LeftA (Open close) isRec' $ length args,
               bindedVars = reverse args,
               definition = def}

pDeclaration :: ParsecMonad Declaration
pDeclaration = foldl1 (<|>) declarations
               where declarations =
                         -- The try is necessary since the parsers seem to consume some input.
                         map try [pPrefixDef, pSuffixDef,
                                  pInfixDef, pInfixDef,
                                  pClosedDef]

pDeclarations :: ParsecMonad [Declaration]
pDeclarations = many pDeclaration

type Command = Either Declaration [ExprToken]

-- Interpreter per-line interpretation.
pCommand :: ParsecMonad Command
pCommand = Left <$> pDeclaration <|> Right <$> pExprElems <* eof

pCommandWrap :: String
                -> Either ParseError (Either Declaration [ExprToken])
pCommandWrap = parse pCommand ""

-- Program Syntactic representation.
pProgram :: ParsecMonad ([Declaration], [ExprToken])
pProgram =
    do
      defs <- pDeclarations
      pMainW
      pEqualsW
      main <- pExprElems
      eof
      return (defs, main)


-- Operator Definition
data Assoc = LeftA | RightA -- Lets take out this one for now | NeutralA
             deriving (Show,Eq)

--            f  a      f  a     a f b
data Fixing = Suffix | Prefix | Infix
            -- This ones will allow us to have closed operators.
            | Open String  -- I know this is redundant but is "handy"
            | Close String
              deriving (Show,Eq)

wrap :: (Monad m, Show t) => Either t a -> m a
wrap (Left err) = fail $ "Parse Error!\n" ++ show err
wrap (Right x) = return x

parseTokenWrap :: (Monad m) => String -> m ExprToken
parseTokenWrap = wrap . parse p ""
    where p = do
            result <- parseToken
            eof
            return result

-- To get standard monad handling of errors.
parseWrap :: (Monad m) => String -> m [ExprToken]
parseWrap = wrap . parse pExprElems ""

parseProgramWrap :: (Monad m) =>
                    String -> m ([Declaration], [ExprToken])
parseProgramWrap = wrap . parse pProgram ""
