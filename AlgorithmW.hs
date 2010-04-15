-- There's no way that I'd ever be able to write such a nice type checker.  This
-- appeared in: Martin Grabmüller: Algorithm W Step by Step, Draft paper, September
-- 2007.  Available at: http://www.grabmueller.de/martin/www/pub/pub.en.html I just
-- made small changes.

{-# LANGUAGE PackageImports #-}

module AlgorithmW  (  Exp(..),
                      Type(..),
                      Lit(..),
                      Scheme(..),
                      TypeEnv(..),
                      generalize,
                      gwiw,
                      ti,
                      runTI,
                      ftv
                   ) where

import qualified Data.Map as Map
import qualified Data.Set as Set
import "mtl" Control.Monad.Error
import "mtl" Control.Monad.Reader
import "mtl" Control.Monad.State
import "mtl" Control.Monad.Identity
import qualified Text.PrettyPrint as PP
import Data.List

data Exp     =  EVar String
             |  ELit Lit
             |  EApp Exp Exp
             |  EAbs String Exp
             |  ELet String Exp Exp
             deriving (Eq, Ord)

data Lit     =  LInt Integer
             |  LBool Bool
             |  LString String
             deriving (Eq, Ord)

data Type    =  TVar String
             |  TInt
             |  TBool
             |  TString
             |  TList Type -- Bad, bad boy.
             |  TProd Type Type  -- Let's be easy-going 
             |  TFun Type Type
             deriving (Eq, Ord)

data Scheme  =  Scheme [String] Type
             deriving Show
class Types a where
    ftv    ::  a -> Set.Set String
    apply  ::  Subst -> a -> a

instance Types Type where
    ftv (TVar n)      =  Set.singleton n
    ftv TInt          =  Set.empty
    ftv TBool         =  Set.empty
    ftv TString       =  Set.empty
    ftv (TFun t1 t2)  =  ftv t1 `Set.union` ftv t2
    ftv (TList t1)    =  ftv t1
    ftv (TProd t1 t2)  =  ftv t1 `Set.union` ftv t2
    apply s (TVar n)      =  case Map.lookup n s of
                               Nothing  -> TVar n
                               Just t   -> t
    apply s (TFun t1 t2)  = TFun  (apply s t1) (apply s t2)
    apply s (TList t1)    = TList (apply s t1)
    apply s (TProd t1 t2)  = TProd (apply s t1) (apply s t2)
    apply _ t             = t

instance Types Scheme where
    ftv (Scheme vars t)      =  (ftv t) `Set.difference` (Set.fromList vars)
    apply s (Scheme vars t)  =  Scheme vars (apply (foldr Map.delete s vars) t)

instance Types a => Types [a] where
    apply    =  map . apply
    ftv      =  foldr Set.union Set.empty . map ftv 

type Subst = Map.Map String Type

nullSubst  ::  Subst
nullSubst  =   Map.empty

composeSubst         :: Subst -> Subst -> Subst
composeSubst s1 s2   = Map.map (apply s1) s2 `Map.union` s1

newtype TypeEnv = TypeEnv (Map.Map String Scheme)
    deriving Show
    
remove                    ::  TypeEnv -> String -> TypeEnv
remove (TypeEnv env) var  =  TypeEnv (Map.delete var env)

instance Types TypeEnv where
    ftv (TypeEnv env)      =  ftv (Map.elems env)
    apply s (TypeEnv env)  =  TypeEnv (Map.map (apply s) env)
                              
generalize        ::  TypeEnv -> Type -> Scheme
generalize env t  =   Scheme vars t
  where vars = Set.toList ((ftv t) `Set.difference` (ftv env))

data TIEnv = TIEnv  {}

data TIState = TIState {  tiSupply :: Int,
                          tiSubst :: Subst}

type TI a = ErrorT String (ReaderT TIEnv (StateT TIState Identity)) a

runTI :: TI a -> Identity (Either String a, TIState)
runTI t = 
    do (res, st) <- runStateT (runReaderT (runErrorT t) initTIEnv) initTIState
       return (res, st)
  where initTIEnv = TIEnv{}
        initTIState = TIState{tiSupply = 0,
                              tiSubst = Map.empty}


gwiw :: Map.Map String Scheme -> Exp -> Either String Type
gwiw env expr = gwiw' $ typeInference env expr
    where 
      gwiw' = fst . runIdentity . runTI 

newTyVar :: String -> TI Type
newTyVar prefix =
    do  s <- get
        put s{tiSupply = tiSupply s + 1}
        return (TVar  (prefix ++ show (tiSupply s)))

instantiate :: Scheme -> TI Type
instantiate (Scheme vars t) = do  nvars <- mapM (\ _ -> newTyVar "a") vars
                                  let s = Map.fromList (zip vars nvars)
                                  return $ apply s t

mgu :: Type -> Type -> TI Subst
mgu (TFun l r) (TFun l' r')  =  
    do  s1 <- mgu l l'
        s2 <- mgu (apply s1 r) (apply s1 r')
        return (s1 `composeSubst` s2)

mgu (TList l) (TList l')  = mgu l l'

mgu (TProd l r) (TProd l' r')  =  
    do  s1 <- mgu l l'
        s2 <- mgu (apply s1 r) (apply s1 r')
        return (s1 `composeSubst` s2)

mgu (TVar u) t               =  varBind u t
mgu t (TVar u)               =  varBind u t
mgu TInt TInt                =  return nullSubst
mgu TBool TBool              =  return nullSubst
mgu TString TString          =  return nullSubst
mgu t1 t2                    =  throwError $ "types do not unify: " ++ show t1 ++ 
                                " vs. " ++ show t2

varBind :: String -> Type -> TI Subst
varBind u t  | t == TVar u           =  return nullSubst
             | u `Set.member` ftv t  =  throwError $ "occur check fails: " ++ u ++
                                         " vs. " ++ show t
             | otherwise             =  return (Map.singleton u t)

tiLit :: Lit -> TI (Subst, Type)
tiLit (LInt _)   =  return (nullSubst, TInt)
tiLit (LBool _)  =  return (nullSubst, TBool)
tiLit (LString _)  =  return (nullSubst, TString)

ti        ::  TypeEnv -> Exp -> TI (Subst, Type)
ti (TypeEnv env) (EVar n) = 
    case Map.lookup n env of
       Nothing     ->  throwError $ "unbound variable: " ++ n
       Just sigma  ->  do  t <- instantiate sigma
                           return (nullSubst, t)

ti env (ELit l) = tiLit l
ti env (EAbs n e) =
    do  tv <- newTyVar "a"
        let TypeEnv env' = remove env n
            env'' = TypeEnv (env' `Map.union` (Map.singleton n (Scheme [] tv)))
        (s1, t1) <- ti env'' e
        return (s1, TFun (apply s1 tv) t1)

ti env (EApp e1 e2) =
    do  tv <- newTyVar "a"
        (s1, t1) <- ti env e1
        (s2, t2) <- ti (apply s1 env) e2
        s3 <- mgu (apply s2 t1) (TFun t2 tv)
        return (s3 `composeSubst` s2 `composeSubst` s1, apply s3 tv)

ti env (ELet x e1 e2) =
    do  (s1, t1) <- ti env e1
        let TypeEnv env' = remove env x
            t' = generalize (apply s1 env) t1
            env'' = TypeEnv (Map.insert x t' env')
        (s2, t2) <- ti (apply s1 env'') e2
        return (s1 `composeSubst` s2, t2)

typeInference :: Map.Map String Scheme -> Exp -> TI Type
typeInference env e =
    do  (s, t) <- ti (TypeEnv env) e
        return (apply s t)

instance Show Type where
    showsPrec _ = shows . prType

prType             ::  Type -> PP.Doc
prType (TVar n)    =   PP.text n
prType TInt        =   PP.text "Int"
prType TBool       =   PP.text "Bool"
prType TString     =   PP.text "String"
prType (TList t)   =   PP.hcat [PP.text "[", prType t, PP.text "]"]
prType (TProd t s) =   
    let inside = prParenType t PP.<+> PP.text "*" PP.<+> prType s
    in PP.hcat [PP.text "(", inside, PP.text ")"]

prType (TFun t s)  =   prParenType t PP.<+> PP.text "->" PP.<+> prType s

prParenType     ::  Type -> PP.Doc
prParenType  t  =   case t of
                      TFun _ _  -> PP.parens (prType t)
                      _         -> prType t

instance Show Exp where
    showsPrec _ = shows . prExp

prExp                  ::  Exp -> PP.Doc
prExp (EVar name)      =   PP.text name
prExp (ELit lit)       =   prLit lit
prExp (ELet x b body)  =   PP.text "let" PP.<+> 
                           PP.text x PP.<+> PP.text "=" PP.<+>
                           prExp b PP.<+> PP.text "in" PP.$$
                           PP.nest 2 (prExp body)
prExp (EApp e1 e2)     =   prExp e1 PP.<+> prParenExp e2

prExp (EAbs n e)       =   PP.char '\\' PP.<+> PP.text n PP.<+>
                           PP.text "->" PP.<+>
                           prExp e
-- prExp (ETuple t)      =   
--     let commaSeparated = PP.hcat $ intersperse (PP.text ",") $ map prExp t
--     in PP.hcat [PP.text "(", commaSeparated, PP.text ")"]
                                                                   

prParenExp    ::  Exp -> PP.Doc
prParenExp t  =   case t of
                    ELet _ _ _  -> PP.parens (prExp t)
                    EApp _ _    -> PP.parens (prExp t)
                    EAbs _ _    -> PP.parens (prExp t)
                    _           -> prExp t

instance Show Lit where
    showsPrec _ = shows . prLit

prLit            ::  Lit -> PP.Doc
prLit (LInt i)    =  PP.integer i
prLit (LString s) =  PP.text $ show s
prLit (LBool b)   =  if b then PP.text "True" else PP.text "False"

