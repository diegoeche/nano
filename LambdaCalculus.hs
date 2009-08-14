module LambdaCalculus (whnf, add, minus, equals, 
                       true, false, andLC, orLC, neg, times, ifthenelse, yComb, c, identity,
                       buildPair, first, second, cons, empty, isNull, hd, rest,
                       Expr(Const, Var, Lam, App),
                       Constant(IData, SData, Prim)) where

import Data.List
--------------------------------------------------
-- Lambda Calculus
-- Mostly based on:
-- http://augustss.blogspot.com/2007/10/simpler-easier-in-recent-paper-simply.html
-- Just added constants to the pure implementation   
-- Bibliography: 2.5.1, 11 The implementation of functional programming languages.
--------------------------------------------------


data Constant = IData  Integer 
              | SData String
              | Prim String 
                deriving (Show, Eq)

data Expr = Const Constant 
          | Var String
          | App Expr Expr
          | Lam String Expr
            deriving (Show, Eq)



-- Weak head normal form.
whnf :: Expr -> Expr
whnf ee = spine ee []
    where spine (App f a) as = spine f (a:as)
          -- This was added to Lennart's implementation.
          spine f@(Const (Prim p)) (a1:a2:as) = 
                case (r1,r2) of
                  (Const x1@(IData _), Const x2@(IData _)) -> 
                       let 
                           replaceRoot = getOperation p x1 x2
                       in spine replaceRoot  as
                  _ -> foldl App f as
              where (r1,r2) = (whnf a1, whnf a2) 
          spine (Lam s e) (a:as) = spine (subst s a e) as
          spine f as = foldl App f as

freeVars :: Expr -> [String]
freeVars (Const _) = []
freeVars (Var s) = [s]
freeVars (App f a) = freeVars f `union` freeVars a
freeVars (Lam i e) = freeVars e \\ [i] 

subst :: String -> Expr -> Expr -> Expr
subst v x' b = sub b
  where sub e@(Const _) = e
        sub e@(Var i) = if i == v then x' else e
        sub (App f a) = App (sub f) (sub a)
        sub (Lam i e) =
            if v == i then
                Lam i e
            else if i `elem` fvx then
                let i' = cloneString e i
                    e' = substVar i i' e
                in  Lam i' (sub e')
            else
                Lam i (sub e)
        fvx = freeVars x'
        cloneString e i = loop i
           where loop i' = if i' `elem` vars then loop (i ++ "'") else i'
                 vars = fvx ++ freeVars e

substVar :: String -> String -> Expr -> Expr
substVar s = subst s . Var 

-- Arithmetic
[x,y,z] = map (:[]) "xyz"

prim :: String -> Expr -> Expr -> Expr
prim p    = App . App (Const $ Prim p)  

add :: Expr -> Expr -> Expr
add       =  prim "+"

times :: Expr -> Expr -> Expr
times     =  prim "*"

minus :: Expr -> Expr -> Expr
minus     =  prim "-"

equals :: Expr -> Expr -> Expr
equals    =  prim "=="

true :: Expr
true = Lam x (Lam y (Var x))

false :: Expr
false = Lam x (Lam y (Var y))


andLC :: [Expr] -> Expr
andLC = foldl App $ Lam x (Lam y (App (App (Var x) (Var y)) (Var x)))

orLC :: [Expr] -> Expr
orLC =  foldl App $ Lam x (Lam y (App (App (Var x) (Var x)) (Var y)))

neg :: [Expr] -> Expr
neg = foldl App $ Lam x (Lam y (Lam z ( App (App (Var x) (Var z)) (Var y))))

ifthenelse :: [Expr] -> Expr
ifthenelse = foldl App (Lam x (Lam y (Lam z (App (App (Var x) (Var y)) (Var z)))))
-- Pairs
buildPair :: Expr -> Expr -> Expr
buildPair = App . App (Lam "a" $ Lam "b" $ Lam x (App (App (Var x) (Var "a")) (Var "b"))) 
first :: Expr -> Expr
first = App (Lam "p" (App (Var "p") true))
second :: Expr -> Expr
second = App (Lam "p" (App (Var "p") false)) 
identity :: Expr
identity = Lam "x" $ Var "x"
-- List
cons :: Expr -> Expr -> Expr
cons = buildPair
empty :: Expr
empty = Lam x true
isNull :: Expr -> Expr
isNull = App (Lam "p" (App (Var "p") (Lam x (Lam y false))))
hd :: Expr -> Expr
hd = first
rest :: Expr -> Expr
rest = second 

c :: Integer -> Expr
c = Const . IData 

-- Y Combinator to implement recursion.
-- Y = λ g. (λ x. g (x x)) (λ x. g (x x))
tmp :: Expr
tmp = Lam x $ App (Var "g") (App (Var x)(Var x))
yComb :: Expr
yComb = Lam "g" $ App tmp tmp

--Primitive operation definition
getOperation :: [Char] -> Constant -> Constant -> Expr
getOperation "+" i1 i2 = Const $ liftInt (+) i1 i2
getOperation "*" i1 i2 = Const $ liftInt (*) i1 i2
getOperation "-" i1 i2 = Const $ liftInt (-) i1 i2
getOperation "==" i1 i2 | liftIData (==) i1 i2 = true
                        | otherwise            = false
getOperation _ _ _     = undefined

liftIData :: (Integer -> Integer -> t) -> Constant -> Constant -> t
liftIData f (IData x1) (IData x2) = f x1 x2
liftIData _ _ _                   = undefined

liftInt :: (Integer -> Integer -> Integer)
           -> Constant
           -> Constant
           -> Constant
liftInt i1 i2 = IData . liftIData i1 i2
