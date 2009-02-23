-- Nano programming language.
-- Bug reports to Diego Echeverri at diegoeche@gmail.com
{-
The first step of our pl is to create a list of tokens with both Literals and
identifiers.
-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Parser (parseWrap, 
               parseTokenWrap,
               Literal(NString, NInteger),
               ExprToken(LiteralToken, FunctionToken)  
              ) where
import qualified Text.ParserCombinators.Parsec.Token as P
import Text.ParserCombinators.Parsec.Language 
import Text.Parsec
import Control.Applicative ((<$>))

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

parseToken = LiteralToken <$> pLiteral 
             <|> FunctionToken <$> pIdentifier

-- Parse expression using the given environment.
-- pExprElems :: ParsecT String () Control.Monad.Identity.Identity [ExprToken]
pExprElems = many parseToken

wrap (Left _) = fail "Parse Error!"    
wrap (Right x) = return x

parseTokenWrap = wrap . parse p "" 
    where p = do 
            result <- parseToken
            eof
            return result

-- To get standard monad handling of errors.
parseWrap :: (Monad m) => String -> m [ExprToken]
parseWrap = wrap . parse pExprElems ""

