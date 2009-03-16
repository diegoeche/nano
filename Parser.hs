-- Nano programming language.
-- Bug reports to Diego Echeverri at diegoeche@gmail.com
{-
The first step of our pl is to create a list of tokens with both Literals and
identifiers.
-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Parser (parseWrap, 
               parseTokenWrap,
               parseProgramWrap,
               Assoc(LeftA, RightA),
               fix, precedence, name,isRec,
               assoc, OpInfo(OpInfo),
               Declaration(Decl),opInfo,
               createOp,
               bindedVars,
               definition,
               Fixing(Suffix, Prefix, Infix, Open, Close),
               Literal(NString, NInteger),
               ExprToken(LiteralToken, FunctionToken)  
              ) where

import qualified Text.ParserCombinators.Parsec.Token as P
import Text.ParserCombinators.Parsec.Language 
import Text.Parsec
import Control.Applicative ((<$>))
import Control.Monad

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
      precedence :: Int,
      assoc :: Assoc,
      fix :: Fixing,
      isRec :: Bool
    } deriving (Show,Eq)

-- Function Declaration 
data Declaration = Decl {opInfo :: OpInfo,
                         bindedVars :: [String],
                         definition :: [ExprToken]
                        } deriving (Show,Eq)
-- Creates an OpInfo
createOp p n a f r = OpInfo {precedence = p, name = n,
                           assoc = a, fix = f, isRec = r}

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
pString =  P.stringLiteral lexer
pNatural =  P.natural lexer
pIdentifier = P.identifier lexer

-- Reserved words parsers.
[pClosedW, pSuffixW, pLetW, pEqualsW, pMainW, pRecW] = 
    map (P.reserved lexer) 
            ["closed", "suffix", "let", "=", "main", "rec"]

pRec = option False (const True <$> pRecW)

pInfix = 
        const RightA <$> reserved "infixr"
    <|> const LeftA <$> reserved "infixl"
    where reserved = P.reserved lexer
    
pLiteral = 
        NString <$> pString 
    <|> NInteger <$> pNatural

parseToken = LiteralToken <$> pLiteral 
             <|> FunctionToken <$> pIdentifier

--------------------------------------------------
-- Concrete Syntax for declarations
--------------------------------------------------
pExprElems = many parseToken
pDefinition = pEqualsW >> pExprElems

    
pSuffixDef = do
  pLetW
  isRec <- pRec
  pSuffixW
  name:params <- liftM reverse $ many pIdentifier
  def <- pDefinition
  return Decl {opInfo = createOp 3 name LeftA Suffix isRec,
               bindedVars = reverse params,
               definition = def} 

pPrefixDef = do
  pLetW
  isRec <- pRec
  name:params <- many pIdentifier
  def <- pDefinition
  return Decl {opInfo = createOp 3 name LeftA Prefix isRec,
               bindedVars = params,
               definition = def} 

-- For the moment we only accept infix operators with two args.
-- Maybe later we should explore if this restriction is necessary.
pInfixDef = do
  pLetW
  isRec <- pRec
  assoc <- pInfix
  [p1,op,p2] <- many pIdentifier
  def <- pDefinition
  return Decl {opInfo = createOp 3 op assoc Infix isRec,
               bindedVars = [p1,p2],
               definition = def} 

pClosedDef = do
  pLetW
  isRec <- pRec
  pClosedW
  open:rest <- many pIdentifier
  let close: args = reverse rest
  def <- pDefinition
  return Decl {opInfo = createOp 3 open LeftA (Open close) isRec, 
               bindedVars = reverse args,
               definition = def} 

pDeclarations = many $ foldl1 (<|>) declarations
               where declarations = 
                         -- The try is necessary since the parsers seem to consume some input.
                         map try [pPrefixDef, pSuffixDef, 
                                  pInfixDef, pInfixDef, 
                                  pClosedDef]

-- Program Syntactic representation.
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

parseProgramWrap = wrap . parse pProgram ""
