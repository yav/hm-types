{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module HMTypes.Example.Decls where

import HMType.AST
import HMType.Subst
import MonadLib
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Sequence as Seq
import Control.Monad.Fix
import Data.List

data Name       = Name String
                  deriving (Show,Eq,Ord)

data Decl       = DLet Decl Decl
                | DAnd Decl Decl
                | DRec Decl
                | DDef Name Expr
                  deriving Show

data Expr       = EApp Expr Expr
                | EFun Name Expr
                | EVar Name
                | ELet Decl Expr
                  deriving Show


inferDecl decl =
  case decl of

    DLet d1 d2 ->
      do env  <- inRec False $ inferDecl d1
         inExtEnv env (inferDecl d2)

    DAnd d1 d2 ->
      do env1 <- inferDecl d1
         env2 <- inferDecl d2
         mergeEnv env1 env2

    DDef x e ->
      do t   <- inferExpr e
         yes <- isInRec
         s <- if yes then return (mono t) else generalize t
         return (singleEnv x s)

    DRec d ->
      do xs <- defs d
         let monoType x = do t <- newTVar kStar
                             return (x, mono t)
         ts <- mapM monoType xs
         let env = Map.fromList ts
         env1 <- inExtEnv env $ inRec True $ inferDecl d
         env2 <- sameTypes env env1
         yes <- isInRec
         if yes then return env2 else generalizeEnv env2

defs = undefined
sameTypes = undefined
generalizeEnv = undefined


inferExpr expr =
  case expr of

    EApp e1 e2 ->
      do t1 <- inferExpr e1
         t2 <- inferExpr e2
         a  <- newTVar kStar
         unify t1 (tFun t2 a)
         return a

    EFun x e ->
      do a <- newTVar kStar
         t <- inExtEnv (singleEnv x (mono a)) (inferExpr e)
         return (tFun a t)

    EVar x ->
      do s <- lookupVar x
         instantiate s

    ELet d e ->
      do env <- inferDecl d
         inExtEnv env (inferExpr e)

--------------------------------------------------------------------------------
mono          = Forall [] []

kStar         = TCon $ TR 0 $ TParam "*"  $ error "sort"
kcFun         = TCon $ TR 1 $ TParam "->" $ error "sort"
kFun k1 k2    = kcFun `TApp` k1 `TApp` k2

tcFun         = TCon $ TR 0 $ TParam "->" $ kFun kStar $ kFun kStar kStar
tFun t1 t2    = tcFun `TApp` t1 `TApp` t2

type Env      = Map.Map Name (Qual Type)
data R        = R Env Bool    -- Bool: are we in Rec?
type W        = Seq.Seq Pred
data S        = S Subst Int
data E        = UndefinedVariable Name
              | UnificationError MguError
              | MultipleDefinitions (Set.Set Name)

newtype TI a  = TI (ReaderT R
                   (WriterT W
                   (StateT S
                   (ExceptionT E
                     Id))) a)
                deriving (Monad)


generalize = undefined

{-
-- XXX: Using the same predicates everywhere is only OK for the
-- recursive case.
generalize (TI m) =
  do (env,ps) <- TI $ collect m
     envCur   <- TI ask
     let fvs = Set.unions $ map freeTVars $ Map.elems envCur
         isGeneral p = Set.null $ Set.intersection fvs $ freeTVars p
         (ps1,ps2) = Seq.partition isGeneral ps
         ps1' = undefined ps1 --Seq.toList ps1
         genT t = let as = Set.fromList $ Set.difference (freeTVars t) fvs
                      su x@(TR _ p)  = do n <- elemIndex x as
                                          return $ TGen $ TR n p
                  in Forall as (apTVars su ps1') (apTVars su t)
     undefined -- addPreds ps2
     return $ Map.map genT env
-}

isInRec = TI $
  do R _ yes <- ask
     return yes

inRec yes (TI m) = TI $
  do R env _ <- ask
     local (R env yes) m

addPreds ps = TI $ put $ Seq.fromList ps

instantiate (Forall as ps t) =
  do ts <- mapM (newTVar . kindOf) as
     addPreds $ map (apGVars ts) ps 
     return $ apGVars ts t


inExtEnv env (TI m) = TI $
  do R envOld yes <- ask
     local (R (Map.union env envOld) yes) m

mergeEnv env1 env2 = TI $
  do let redef = Map.keysSet (Map.intersection env1 env2)
     if Set.null redef
       then return (Map.union env1 env2)
       else raise $ MultipleDefinitions redef
    

newTVar k   = TI $
  do S su n <- get
     set $ S su (n + 1)
     return $ TVar $ TR n $ TParam ('?' : show n) k

unify t1 t2 = TI $
  do S su n <- get
     let subst = apTVars (`lookupS` su)
     case mgu (subst t1) (subst t2) of
       Left err  -> raise $ UnificationError err
       Right su1 -> set $ S (compS su1 su) n

singleEnv x s = Map.singleton x s

lookupVar x = TI $
  do R env _ <- ask
     case Map.lookup x env of
       Just s   -> return s
       Nothing  -> raise $ UndefinedVariable x



