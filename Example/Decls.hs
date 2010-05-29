{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Example.Decls where

import HMType.AST
import HMType.Subst
import MonadLib
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Sequence as Seq
import Control.Monad.Fix
import Data.List
import Data.Maybe

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

inferDeclMono decl =
  case decl of

    DLet d1 d2 ->
      do env  <- inferDecl d1
         inExtEnv env $ inferDeclMono d2

    DAnd d1 d2 ->
      do env1 <- inferDeclMono d1
         env2 <- inferDeclMono d2
         mergeEnv env1 env2

    DDef x e ->
      do t <- inferExpr e
         return $ singleEnv x $ mono t

    DRec d ->
      do env1 <- monoEnv d
         env2 <- inExtEnv env1 $ inferDeclMono d
         zipWithM_ unify (monoEnvTypes env1) (monoEnvTypes env2)
         return env2


inferDecl decl =
  case decl of

    DLet d1 d2 ->
      do env  <- inferDecl d1
         inExtEnv env (inferDecl d2)

    DAnd d1 d2 ->
      do env1 <- inferDecl d1
         env2 <- inferDecl d2
         mergeEnv env1 env2

    DDef x e ->
      do (t,ps) <- getPreds $ inferExpr e
         (as,ps1,t1) <- generalize ps t
         return $ singleEnv x $ Forall as ps1 t1

    DRec d ->
      do env1 <- monoEnv d
         (env2,ps) <- getPreds $ inExtEnv env1 $ inferDeclMono d
         zipWithM_ unify (monoEnvTypes env1) (monoEnvTypes env2)
         generalizeEnv ps env2

monoEnv d = liftM (E . Map.fromList)
          $ forM (Set.toList (defs d))
          $ \x -> do t <- newTVar kStar
                     return (x, mono t)

fromMonoSchema (Forall _ _ t) = t

monoEnvTypes (E env) = map (fromMonoSchema . snd) $ Map.toList env


defs decl = case decl of
              DAnd d1 d2  -> Set.union (defs d1) (defs d2)
              DLet _ d    -> defs d
              DRec d      -> defs d
              DDef x _    -> Set.singleton x

generalizeEnv ps (E env) =
  do let (xs,ss) = unzip $ Map.toList env
     (as,ps1,ts1) <- generalize ps $ map fromMonoSchema ss
     let toS x t = (x, Forall as ps1 t)
     return $ E $ Map.fromList $ zipWith toS xs ts1


generalize ps t =
  do R (E env) <- TI $ ask
     let envVars    = freeTVars $ Map.elems env
         genVars    = freeTVars t `Set.difference` envVars
         isExtern p = Set.null (freeTVars p `Set.intersection` genVars)
         (externalPreds, localPreds) = partition isExtern $ seqToList ps

         as   = Set.toList genVars

         -- signature needed due to the monomorhism restriction
         apS :: HasTVars t => t -> t
         apS  = apTVars $ \x@(TR _ p) -> do n <- elemIndex x as
                                            return $ TAtom TGen $ TR n p

     addPreds externalPreds
     return ([ a | TR _ a <- as ], apS localPreds, apS t)



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

kStar         = TAtom TCon $ TR 0 $ TParam "*"  $ Nothing
kcFun         = TAtom TCon $ TR 1 $ TParam "->" $ Nothing
kFun k1 k2    = kcFun `TApp` k1 `TApp` k2

tcFun         = TAtom TCon $ TR 0 $ TParam "->"
                           $ Just $ kFun kStar $ kFun kStar kStar
tFun t1 t2    = tcFun `TApp` t1 `TApp` t2
--------------------------------------------------------------------------------



newtype Env   = E (Map.Map Name (Qual Type))
                  deriving Show


data R        = R Env
type W        = Seq.Seq Pred
data S        = S { subst         :: Subst
                  , names         :: Int
                  }

data E        = UndefinedVariable Name Type
              | UnificationError MguError
              | MultipleDefinitions (Set.Set Name)
                deriving Show

instance HasTVars E where
  apTVars f err =
    case err of
      UndefinedVariable x t -> UndefinedVariable x (apTVars f t)
      UnificationError e    -> UnificationError (apTVars f e)
      MultipleDefinitions x -> MultipleDefinitions x

  freeTVars err =
    case err of
      UndefinedVariable _ t -> freeTVars t
      UnificationError e    -> freeTVars e
      MultipleDefinitions x -> Set.empty



newtype TI a  = TI (ReaderT R
                   (WriterT W
                   (WriterT (Seq.Seq E)
                   (StateT S
                     Id))) a)
                deriving (Monad)


runTI (TI m)  = (apS a, apS (seqToList errs), apS (seqToList ps))
  where
  apS x = apTVars (`lookupS` subst s) x
  (((a,ps), errs), s)
    = runId
    $ runStateT S { subst = emptyS, names = 0 }
    $ runWriterT
    $ runWriterT
    $ runReaderT (R emptyEnv) m



addErrs es = TI $ lift $ lift $ put $ Seq.fromList es

addPreds ps = TI $ put $ Seq.fromList ps

getPreds (TI m) = TI $ collect m

-- By this point, all as should have kinds.
instantiate (Forall as ps t) =
  do ts <- mapM newTVar $ mapMaybe kindOf as
     addPreds $ map (apGVars ts) ps
     return $ apGVars ts t


inExtEnv (E env) (TI m) = TI $
  do R (E envOld) <- ask
     local (R $ E (Map.union env envOld)) m

mergeEnv (E env1) (E env2) =
  do let redef = Map.keysSet (Map.intersection env1 env2)
     unless (Set.null redef) $ addErrs [ MultipleDefinitions redef ]
     -- In case of error, we use the first definition.
     return $ E $ Map.union env1 env2

newTVar k   = TI $
  do s <- get
     let n = names s
     set s { names = n + 1 }
     return $ TAtom TVar $ TR n $ TParam "" $ Just k

unify t1 t2 =
  do s <- TI $ get
     let su1        = subst s
         substF     = apTVars (`lookupS` su1)
         (su2,errs) = mgu (substF t1) (substF t2)
     -- In case of error, we just use the partially computed subsitution.
     TI $ set s { subst = compS su2 su1 }
     addErrs $ map UnificationError errs

lkpEnv x (E m)  = Map.lookup x m
emptyEnv        = E Map.empty
singleEnv x s   = E $ Map.singleton x s
listEnv (E m)   = Map.toList m

instance HasTVars Env where
  apTVars f (E m) = E $ Map.map (apTVars f) m
  freeTVars (E m) = Set.unions $ map freeTVars $ Map.elems m

lookupVar x =
  do R env <- TI $ ask
     case lkpEnv x env of
       Just s   -> return s
       -- In case of error, we assume some monomorphic type.
       -- Note that this results in a new error for each separate
       -- use of an undefined variable.  Perhaps, it is better to
       -- report only the first one?
       Nothing  ->
        do t <- newTVar kStar
           addErrs [ UndefinedVariable x t ]
           return $ mono t


seqToList = Seq.foldrWithIndex (const (:)) []

