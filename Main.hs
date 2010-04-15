{-# LANGUAGE PackageImports #-}
import Interpreter
import "mtl" Control.Monad.State
import qualified Environment as E

main :: IO ((), Interpreter.Environment)
main =
    runStateT loop (Env E.types E.definitions E.operators)