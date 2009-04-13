module LambdaCalculus (whnf, add, minus, equals, 
                       true, false, times, ifthenelse, yComb, c, identity,
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


-- Arithmetic
prim x    = App . App (Const $ Prim x)  
add       =  prim "+"
times     =  prim "*"
minus     =  prim "-"
equals    =  prim "=="
true = Lam x (Lam y (Var x))
false = Lam x (Lam y (Var y))
neg = Lam x (Lam y (Lam z ( App (App (Var x) (Var z)) (Var y))))
ifthenelse params = foldl App (Lam x (Lam y (Lam z (App (App (Var x) (Var y)) (Var z))))) params
-- Pairs
buildPair a b = Lam x (App (App (Var x) a) (b))
first pair = App (Lam "p" (App (Var "p") true)) pair
second pair = App (Lam "p" (App (Var "p") false)) pair
identity = Lam "x" $ Var "x"
-- List
cons = buildPair
empty = Lam x $ true
isNull l = App (Lam "p" (App (Var "p") (Lam x (Lam y false)))) (l)
hd = first
rest = second 

c = Const . IData 

-- http://en.wikipedia.org/wiki/Lambda_calculus
-- Probably these should change when we add polymorphic types.
-- (The Y combinator)
[x,y,z] = map (:[]) "xyz"

-- Y Combinator to implement recursion.
-- Y = λ g. (λ x. g (x x)) (λ x. g (x x))
tmp = Lam x $ App (Var "g") (App (Var x)(Var x))
yComb = Lam "g" $ App tmp tmp

--Primitive operation definition
getOperation "+" x y = Const $ liftInt (+) x y
getOperation "*" x y = Const $ liftInt (*) x y
getOperation "-" x y = Const $ liftInt (-) x y
getOperation "==" x y = 
    case liftIData (==) x y of
      True -> true
      _    -> false

liftIData f (IData x1) (IData x2) = f x1 x2
liftInt x y = IData . liftIData x y 

-- Weak head normal form.
whnf :: Expr -> Expr
whnf ee = spine ee []
    where spine (App f a) as = spine f (a:as)
          -- This was added to Lennart's implementation.
          spine f@(Const (Prim x)) (a1:a2:as) = 
                case (r1,r2) of
                  (Const x1@(IData _), Const x2@(IData _)) -> 
                       let 
                           replaceRoot = getOperation x x1 x2
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
subst v x b = sub b
  where sub e@(Const _) = e
        sub e@(Var i) = if i == v then x else e
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
        fvx = freeVars x
        cloneString e i = loop i
           where loop i' = if i' `elem` vars then loop (i ++ "'") else i'
                 vars = fvx ++ freeVars e

substVar :: String -> String -> Expr -> Expr
substVar s = subst s . Var 

-- Recursion Example:
-- fact = Lam "fact" 
--        $ Lam "n" 
--        $ App (App (App ifthenelse (equals (c 0) (Var "n"))) 
--                 (Const $ IData 1)) 
--        (times (Var "n") (App (Var "fact") (minus (Var "n") (c 1))))

--recFact = App (App yComb fact) . c

{-
-- whnf $ times (c 2) $ add (c 2) (c 5) 
whnf $ minus (c 2) (c 3) 
-- Const (IData 14)
-- whnf $ App (App (App ifthenelse (App neg true)) (Const $ IData 3)) (Const $ IData 4)
whnf $ App yComb (Lam "fact" $ Lam "n" $ App (App (App ifthenelse (equals (c 0) (Var "n"))) (Const $ IData 1)) (times (Var "n") (App (Var "fact") (minus (Var "n") (c 1)))))
whnf $ recFact 3
-}