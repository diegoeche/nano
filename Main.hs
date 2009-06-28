import Interpreter
import Control.Monad.State
import qualified Environment as E

main :: IO ((), Interpreter.Environment)
main = 
    runStateT loop (Env E.types E.definitions E.operators)