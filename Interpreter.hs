module Interpreter where

import Control.Monad.State
import qualified Data.Map as M
import LambdaCalculus
import Parser
import AlgorithmW
import Data.Char (isSpace)
import Control.Monad (when)
import qualified Evaluator as E

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
  line <-  getLine 
  case reverse line of
    '\\':_ -> do
        next <- getMultiline
        return $ line ++ '\n':next
    _ -> return line

-- According to the wiki haskell doesn't have this as a built-in
-- Surprising thou.
trim :: String -> String
trim = f . f
    where f = reverse . dropWhile isSpace

loop :: Interpreter ()
loop = do
  command <- lift getMultiline
  lift $ putStrLn "Nano>"
  when (trim command /= ":quit") $ process $ pCommandWrap command
      where process (Left err) = 
                do
                  lift $ putStrLn "Parsing error command:"
                  lift $ putStrLn $ show err
                  loop
            process (Right x) = do
                  Env types defs ops <- get
                  case x of
                    Right value ->
                        do
                          lift $ putStrLn "Value Found"
                          loop
                    Left decl -> 
                        do
                          lift $ putStrLn "Decl Found"
                          loop

main = 
    runStateT loop (Env E.types E.definitions E.operators)