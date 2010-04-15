module Environment (types, operators, definitions) where

import LambdaCalculus
import Parser
import qualified Data.Map as M
import AlgorithmW

--------------------------------------------------
-- Environment Definition.
--------------------------------------------------

createOpTuple :: Integer
                 -> String
                 -> Assoc
                 -> Fixing
                 -> Int
                 -> (String, OpInfo)
createOpTuple p n a f ar = (n, OpInfo {precedence = p, name = n,
                                      assoc = a, fix = f, isRec = False, arity = ar})

createConstTuple :: Integer
                    -> String
                    -> Assoc
                    -> Fixing
                    -> (String, OpInfo)
createConstTuple p n a f = (n, OpInfo {precedence = p, name = n,
                                       assoc = a, fix = f, isRec = False, arity = 0})


plusS :: (String, OpInfo)
minusS :: (String, OpInfo)
timesS :: (String, OpInfo)
ifThenElse :: (String, OpInfo)
plusS       = createOpTuple 1 "add"        LeftA  Prefix 2
minusS      = createOpTuple 1 "sub"        LeftA  Prefix 2
timesS      = createOpTuple 2 "times"      RightA Prefix 2
ifThenElse  = createOpTuple 2 "if"         RightA Prefix 3


buildPair' :: (String, OpInfo)
first' :: (String, OpInfo)
second' :: (String, OpInfo)
buildPair'  = createOpTuple 2 "buildPair" RightA Prefix 2
first'      = createOpTuple 2 "fst" RightA Prefix 1
second'     = createOpTuple 2 "snd" RightA Prefix 1

hd' :: (String, OpInfo)
rest' :: (String, OpInfo)
isNull' :: (String, OpInfo)
empty' :: (String, OpInfo)
cons' :: (String, OpInfo)
hd'         = createOpTuple 2 "hd" RightA Prefix 1
rest'       = createOpTuple 2 "rest" RightA Prefix 1
isNull'     = createOpTuple 2 "isNull" RightA Prefix 1
empty'      = createConstTuple 1000 "empty" RightA Prefix 
cons'       = createOpTuple 2 "cons" RightA Prefix 2

true' :: (String, OpInfo)
false' :: (String, OpInfo)
equals' :: (String, OpInfo)
true'       = createConstTuple 1000 "true"   RightA Prefix
false'      = createConstTuple 1000 "false"  RightA Prefix
equals'     = createOpTuple 2    "equals" RightA Prefix 2
and'        = createOpTuple 2    "and"    RightA Prefix 2
or'         = createOpTuple 2    "or"     RightA Prefix 2
neg'        = createOpTuple 2    "neg"    RightA Prefix 1

types :: M.Map [Char] Scheme
types = 
    M.fromList [("true", Scheme [] TBool),
                ("false", Scheme [] TBool),
                ("add", Scheme [] $ TFun  TInt  (TFun TInt TInt)),
                ("times", Scheme [] $ TFun  TInt  (TFun TInt TInt)),
                ("sub", Scheme [] $ TFun  TInt  (TFun TInt TInt)),
                ("equals",Scheme [] $ TFun  TInt  (TFun TInt TBool)),
                ("and", Scheme [] $ TFun  TBool  (TFun TBool TBool)),
                ("or",  Scheme [] $ TFun  TBool  (TFun TBool TBool)),
                ("neg", Scheme [] $ TFun TBool TBool),


                ("buildPair",Scheme ["a", "b"] $ TFun  (TVar "a") (TFun (TVar "b") (TProd (TVar "a") (TVar "b")))),
                ("fst",Scheme ["a", "b"]       $ TFun  (TProd (TVar "a") (TVar "b")) (TVar "a")),
                ("snd",Scheme ["a", "b"]       $ TFun  (TProd (TVar "a") (TVar "b")) (TVar "b")),
                ("if", Scheme ["b1"] $ TFun TBool (TFun (TVar "b1") (TFun (TVar "b1") (TVar "b1")))),

                ("cons",Scheme ["a"]   $ TFun  (TVar "a") (TFun (TList $ TVar "a") (TList $ TVar "a"))),
                ("hd"  ,Scheme ["a"]   $ TFun  (TList $ TVar "a") (TVar "a")),
                ("rest",Scheme ["a"]   $ TFun  (TList $ TVar "a") (TList $ TVar "a")),
                ("empty",Scheme ["a"]  $ TList $ TVar "a"),
                ("isNull",Scheme ["a"] $ TFun  (TList $ TVar "a") TBool)
               ]


operators :: M.Map String OpInfo
operators = M.fromList [plusS, timesS, minusS, 
                        ifThenElse, true', false', 
                        equals', buildPair', first', second',
                        hd', isNull', rest', cons', empty', and', or', neg'
                       ]

unary :: (t -> t1) -> [t] -> t1
unary  f [x] = f x 
unary  _  _  = undefined

binary :: (t -> t -> t1) -> [t] -> t1
binary f [x, y] = f x y
binary  _  _  = undefined

--ternary f [x, y, z] = f x y z
--ternary f  _  = undefined

definitions :: M.Map [Char] ([Expr] -> Expr)
definitions = 
    M.fromList
    [-- Arithmetic
     ("add", binary add), 
     ("times", binary times), 
     ("sub", binary minus),
     ("equals", binary equals), 
     -- Pairs
     ("buildPair", binary buildPair),
     ("fst", unary first),
     ("snd", unary second),
     -- Lists
     ("cons", binary cons),
     ("hd", unary hd),
     ("rest", unary rest),
     ("isNull", unary isNull),
     ("empty", const empty),
     -- Flow control
     ("true", const true),
     ("false", const false),
     ("if", ifthenelse),
     ("and", andLC),
     ("or",  orLC),
     ("neg", neg)
    ]

