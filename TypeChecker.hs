{-# LANGUAGE ParallelListComp #-}
module TypeChecker (typeCheckExpr, typeCheckFunction) where

import Analyser
import qualified Data.Map as M 
import Data.List
import Control.Applicative
import Control.Monad
import AlgorithmW
import Control.Monad.Identity
import System.IO.Unsafe


-- The value actually doesn't matter since we are only using it for TC purposes.
translateToW (Value (IPrim x))  = ELit $ LInt x
translateToW (Call f [])        = EVar f
translateToW (Call f args)      = foldl EApp (EVar f) $ map translateToW args

typeCheckExpr :: M.Map String Scheme
                 -> ExprTree
                 -> Either String Type
typeCheckExpr typeEnv = 
    gwiw typeEnv . translateToW 

typeCheckFunction  typeEnv f args =
    -- Add arguments as abstractions over the function.
--    let a = (foldl (.) id $ map EAbs args) . translateToW $ f
--    in unsafePerformIO (putStrLn $ show a) `seq`
       gwiw typeEnv . (foldl (.) id $ map EAbs args) . translateToW $ f


-- f = Call "ifThenElse" [(Call "==" [Call "n" [],int 0]), 
--                        int 1, 
--                        Call "*" [Call "n" [], 
--                                  (Call "fact" 
--                                   [Call "-" 
--                                    [Call "n" [], int 1]])]
--                       ]
    

------------------------------------------------------
-- Old type checker
------------------------------------------------------

-- data Type = TInteger 
--           | TString 
--           | TBoolean 
--           | TVar String 
--           | Custom String
--           | Arrow Type Type
--           | Tuple [Type]
--             deriving (Show,Eq)

-- --tVariables = map return ['a'..'z'] ++ map (++ "'") tVariables

-- tVariables = "a" : map (++ "'") tVariables

-- -- Gets the equations from an abstraction. (The only way to do 
-- -- Abstractions is in function declaration)
-- getEqsAbs vars tEnv expr t fresh = 
--     let (used,notUsed) = splitAt (length vars) fresh
--         a = map TVar used
--         tEnv' = tEnv `M.union` (M.fromList $ zip vars a)
--         b:notUsed' = notUsed
--     in do 
--       equations <- getEquations tEnv' expr (TVar b) notUsed'
--       case a of 
--         [a] -> return $ equations ++ [(t, Arrow a (TVar b))]
--         xs  -> return $ equations ++ [(t, Arrow (Tuple xs) (TVar b))]

-- getEquations :: (Monad m) =>
--                 M.Map String Type
--                 -> [ExprTree]
--                 -> Type
--                 -> [String]
--                 -> m [(Type, Type)]
-- getEquations _ [Value (IPrim _)] t _ = return [(t,TInteger)]
-- getEquations tEnv [Call s xs] t fresh = 
--     let (v:vs) = fresh
--         -- We just make 2 different list for each recursive call. using the odd and even 
--         -- Fresh vars
--         vT = TVar v
--         (vs',vs'') = partition ((==) 0 . snd) $ zip vs $ cycle [0,1]
--     in case xs of
--           [] -> case M.lookup s tEnv of
--                   Just x -> return [(t,x)]
--                   Nothing -> fail $ show s ++ " type not found."
--           [x] -> do
--                  first <- getEquations tEnv [x] vT (map fst vs')
--                  second <- getEquations tEnv [Call s []] (Arrow vT t) (map fst vs'')
--                  return $ first ++ second
--           xs -> do
--                  first <- getEquations tEnv xs vT (map fst vs')
--                  second <- getEquations tEnv [Call s []] (Arrow vT t) (map fst vs'')
--                  return $ first ++ second
-- getEquations tEnv tuple t fresh = 
--     let (used,nonUsed) = splitAt (length tuple) fresh
--         -- ["a","a'"...] -> [["a","a'"...],["b","b'"...],...]
--         -- Please don't use to much tuples or we can run out of fresh vs. :P
--         freshVarsLists = (iterate . map) (\(x:xs) -> succ x:xs) nonUsed
--         --[(vars,ts,tuples)]
--         vars = map TVar used
--         params = zip3 freshVarsLists vars (map return tuple)
--     in do 
--       tupleEquations <- foldM folder [] params
--       return $ tupleEquations ++ [(t, Tuple vars)]
--           where folder acc (x,y,z) = do 
--                   eq <- getEquations tEnv z y x
--                   return $ acc ++ eq

-- resolveEquations [] = fail "No equations"
-- --resolveEquations (t@(x, _):xs) | x == TVar "T" = resolveEquations $ xs ++ [t]
-- resolveEquations ((x, y):_:xs) | x == y = resolveEquations xs
-- resolveEquations ((Arrow t1 t2, Arrow t3 t4):xs) = resolveEquations ((t1,t3):(t2,t4):xs)
-- resolveEquations ((TVar x, t):xs) 
--     | xs /= [] = resolveEquations $ map (tmap $ replace x t) xs
-- resolveEquations ((t, TVar x):xs) 
--     | xs /= [] = resolveEquations $ map (tmap $ replace x t) xs

-- resolveEquations ((Tuple tx, Tuple ty):xs) 
--     | length tx == length ty = resolveEquations $ zip tx ty ++ xs
--     | otherwise = fail $ "The arity of " ++ show tx ++ " and " ++ show ty ++ "Doesn't match"

-- resolveEquations [x] = return x
-- resolveEquations x =  fail $ "Not possible to unify: " ++ show x

-- isType (TVar _) = False
-- isType (Tuple x) = all isType x
-- isType _ = True

-- replace x y (Arrow t1 t2) = Arrow (replace x y t1) (replace x y t2)
-- replace x y (Tuple l) = Tuple $ map (replace x y) l
-- replace x y (TVar x') | x == x' = y
-- replace x y z  = z

-- tmap f (a,b) = (f a, f b)


-- --------------------------------------------------
-- -- Tests
-- --------------------------------------------------

-- typeEnv = M.fromList [("id", Arrow (TVar "A") (TVar "A")),
--                       ("+", Arrow (Tuple [TInteger,TInteger]) TInteger),
--                       ("*", Arrow (Tuple [TInteger,TInteger]) TInteger),
--                       ("-", Arrow (Tuple [TInteger,TInteger]) TInteger),
--                       ("==", Arrow (Tuple [TInteger,TInteger]) TBoolean),
--                       ("id", Arrow (TVar "A") (TVar "A")),
--                       ("ifThenElse", Arrow (Tuple [TBoolean, TVar "IF", TVar "IF"]) $ TVar "IF")]




-- a () = getEqsAbs ["x"] typeEnv [Call "ifThenElse" 
--                              [(Call "==" [Call "+" [int 3,int 2],
--                                           int 3]), int 5, int 6
--                              ]
--                             ] (TVar "T") tVariables >>= resolveEquations

-- fact () = getEqsAbs ["fact","n"] (typeEnv ) 
--           ] (TVar "T") tVariables -- >>= resolveEquations


-- b () = getEquations typeEnv [Call "==" [int 3, Call "id" [int 3]] ] (TVar "T") tVariables  >>= resolveEquations


