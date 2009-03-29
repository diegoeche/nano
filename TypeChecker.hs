import Analyser
import qualified Data.Map as M hiding (split) 
import Data.List
import Control.Applicative
import Control.Monad

data Type = TInteger 
          | TString 
          | TBoolean 
          | TVar String 
          | Custom String
          | Arrow Type Type
          | Tuple [Type]
            deriving (Show,Eq)



tVariables = map return ['a'..'z'] ++ map (++ "'") tVariables

getEquations _ (Value (IPrim _)) t _ = return [(t,TInteger)]
getEquations tEnv (Call s xs) t fresh = 
    let (v:vs) = fresh
        -- We just make 2 different list for each recursive call. using the odd and even 
        -- Fresh vars
        vT = TVar v
        (vs',vs'') = partition ((==) 0 . snd) $ zip vs $ cycle [0,1]
    in case xs of
          [] -> case M.lookup s tEnv of
                  Just x -> return [(t,x)]
                  Nothing -> fail $ show s ++ " type not found."
          [x] -> do
                 first <- getEquations tEnv x vT (map fst vs')
                 second <- getEquations tEnv (Call s []) (Arrow vT t) (map fst vs'')
                 return $ first ++ second
          xs -> --let () 
              do
                 first <- concat $ mapM (\par -> getEquations tEnv par vT (map fst vs')) xs
                 second <- getEquations tEnv (Call s []) (Arrow vT t) (map fst vs'')
                 return $ first ++ second

resolveEquations [] = fail "No equations"
resolveEquations ((x, y):xs) | x == y = resolveEquations xs
resolveEquations ((Arrow t1 t2, Arrow t3 t4):xs) = resolveEquations ((t1,t3):(t2,t4):xs)
resolveEquations ((TVar x, t):xs) | xs /= [] = resolveEquations $ map (tmap $ replace x t) xs
resolveEquations ((t, TVar x):xs) | xs /= [] = resolveEquations $ map (tmap $ replace x t) xs
resolveEquations [x] = return x
resolveEquations x =  fail "Not possible to unify"

replace x y (Arrow t1 t2) = Arrow (replace x y t1) (replace x y t2)
replace x y (Tuple l) = Tuple $ map (replace x y) l
replace x y (TVar x') | x == x' = y
replace x y z  = z

tmap f (a,b) = (f a, f b)

--getEquations (M.fromList [("id", Arrow (TVar "A") (TVar "A"))]) (Call "id" []) (TVar "T") tVariables >>= resolveEquations 
--getEquations (M.fromList [("id", Arrow (TVar "A") (TVar "A"))]) (Call "id" [int 4]) (TVar "T") tVariables >>= resolveEquations 

typeEnv = (M.fromList [("id", Arrow (TVar "A") (TVar "A")),("+", Arrow (Tuple [TInteger,TInteger]) TInteger)])
int n = Value $ IPrim n
eq = getEquations typeEnv (Call "+" [int 3, int 5]) (TVar "T") tVariables