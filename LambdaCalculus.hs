module LambdaCalculus (whnf, add, times, ifthenelse, yComb, c, identity,
                       Expr(Const, Var, Lam, App),
                       Constant(Data, Prim)) where

import Data.List
--------------------------------------------------
-- Lambda Calculus
-- Mostly based on:
-- http://augustss.blogspot.com/2007/10/simpler-easier-in-recent-paper-simply.html
-- Just added constants to the pure implementation   
-- Bibliography: 2.5.1, 11 The implementation of functional programming languages.
--------------------------------------------------

-- Todo: Add maybe booleans and strings
data Constant = Data Integer 
              | Prim String 
                deriving (Show, Eq)

data Expr = Const Constant 
          | Var String
          | App Expr Expr
          | Lam String Expr
            deriving (Show, Eq)
-- Helpers
prim x = App . App (Const $ Prim x)  
add    =  prim "+"
times  =  prim "*"
minus  =  prim "-"
equals =  prim "=="
identity = Lam "x" $ Var "x"
c = Const . Data 

-- http://en.wikipedia.org/wiki/Lambda_calculus
-- Probably these should change when we add polymorphic types.
-- (The Y combinator)
[x,y,z] = map (:[]) "xyz"
true = Lam x (Lam y (Var x))
false = Lam x (Lam y (Var y))
neg = Lam x (Lam y (Lam z ( App (App (Var x) (Var z)) (Var y))))
ifthenelse = Lam x (Lam y (Lam z (App (App (Var x) (Var y)) (Var z))))

-- Y Combinator to implement recursion.
-- Y = λ g. (λ x. g (x x)) (λ x. g (x x))
tmp = Lam x $ App (Var "g") (App (Var x)(Var x))
yComb = Lam "g" $ App tmp tmp

--Primitive operation definition
getOperation "+" x y = Const $ liftInt (+) x y
getOperation "*" x y = Const $ liftInt (*) x y
getOperation "-" x y = Const $ liftInt (-) x y
getOperation "==" x y = 
    case liftData (==) x y of
      True -> true
      _    -> false

liftData f (Data x1) (Data x2) = f x1 x2
liftInt x y = Data . liftData x y 

-- Weak head normal form.
whnf :: Expr -> Expr
whnf ee = spine ee []
    where spine (App f a) as = spine f (a:as)
          -- This was added to Lennart's implementation.
          spine f@(Const (Prim x)) (a1:a2:as) = 
                case (r1,r2) of
                  (Const x1@(Data _), Const x2@(Data _)) -> 
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
fact = Lam "fact" 
       $ Lam "n" 
       $ App (App (App ifthenelse (equals (c 0) (Var "n"))) 
                (Const $ Data 1)) 
       (times (Var "n") (App (Var "fact") (minus (Var "n") (c 1))))

recFact = App (App yComb fact) . c

{-
-- whnf $ times (c 2) $ add (c 2) (c 5) 
whnf $ minus (c 2) (c 3) 
-- Const (Data 14)
-- whnf $ App (App (App ifthenelse (App neg true)) (Const $ Data 3)) (Const $ Data 4)
whnf $ App yComb (Lam "fact" $ Lam "n" $ App (App (App ifthenelse (equals (c 0) (Var "n"))) (Const $ Data 1)) (times (Var "n") (App (Var "fact") (minus (Var "n") (c 1)))))
whnf $ recFact 3
-}