-- Nano programming language.
-- Bug reports to Diego Echeverri at diegoeche@gmail.com
{- Notes:

February 5:
The idea of using an infix and postfix operator for emulating distfix operators like:
[_] has some troubles. This the "hard" example "2 * ( 3 ) + 2 " the argument of `(` 
would be 3 ) + 2

We will just offer an operator of the form [_] 
-}
module Interpreter (loop,Environment(Env)) where

import Control.Monad.State
import qualified Data.Map as M
import LambdaCalculus
import Parser
import AlgorithmW
import Data.Char (isSpace)
import Control.Monad (when)
import Evaluator
import System.IO
-- This is inherited from the small-step construction
-- methodoloy. We could refactor and use only one map for all the 
-- State.
type Operators   = M.Map String OpInfo
type Types       = M.Map [Char] Scheme
type Definitions = M.Map [Char] ([Expr] -> Expr)

-- All the environment.
data Environment = Env {types:: Types,             -- The types we have defined
                        definitions:: Definitions, -- The definitions (aka implementations)
                        operators:: Operators}     -- The 

type Interpreter a = StateT Environment IO a

getMultiline :: IO String
getMultiline = do
  line <- getLine 
  case reverse line of
    '\\':ls -> do
        next <- getMultiline
        return $ reverse ls ++ '\n':next
    _ -> return line

-- According to the wiki haskell doesn't have this as a built-in
-- Surprising thou.
trim :: String -> String
trim = f . f
    where f = reverse . dropWhile isSpace

loop :: Interpreter ()
loop = do
  lift $ putStr "Nano> "
  lift $ hFlush stdout
  command <- lift getMultiline
  when (trim command /= ":quit") $ process $ pCommandWrap command
      where process (Left err) = 
                do
                  lift $ putStrLn "Error parsing command:"
                  lift $ print err
                  loop
            process (Right x) = 
                do
                  Env ty defs ops <- get
                  case x of
                    Right expr ->
                        case evalExpression ops ty defs expr of 
                          Right (t,v) -> do
                            lift $ putStrLn $ "Type: " ++ show t ++ "\nVal: " ++ show v
                            loop
                          Left err -> do
                            lift $ putStrLn "Error with expression:"
                            lift $ putStrLn err
                            loop
                    Left decl -> 
                        case addDeclToEnv ops ty defs decl of 
                          Right (ops', ty', defs', t) -> do
                            lift $ putStrLn $ "Type: " ++ show t
                            put $ Env ty' defs' ops'
                            loop
                          Left err -> do
                            lift $ putStrLn "Error with declaration:"
                            lift $ putStrLn err
                            loop


