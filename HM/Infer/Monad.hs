{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module HM.Infer.Monad
  ( TI
  , runTI
  , addErrs
  , addPreds, getPreds
  , inExtEnv, getEnv
  , newTVar, unify

  , mergeEnv
  , generalize, instantiate
  ) where

import qualified HM.Infer.Env as Env
import HM.Infer.Env(Env)
import HM.Infer.Error

import HM.Type.AST
import HM.Type.Subst

import MonadLib
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import Data.List(elemIndex, partition)
import Data.Maybe(mapMaybe)


data R n      = R (Env n)
type W        = Seq.Seq Pred
data S        = S { subst         :: Subst
                  , names         :: Int
                  }

newtype TI n a= TI (ReaderT (R n)
                   (WriterT W
                   (WriterT (Seq.Seq (Error n))
                   (StateT S
                     Id))) a)
                deriving (Monad)


runTI :: HasTVars a => TI n a -> (a, [Error n], [Pred])
runTI (TI m)  = (apSu a, apSu (seqToList errs), apSu (seqToList ps))
  where
  apSu x = apTVars (`lookupS` subst s) x
  (((a,ps), errs), s)
    = runId
    $ runStateT S { subst = emptyS, names = 0 }
    $ runWriterT
    $ runWriterT
    $ runReaderT (R Env.empty) m


addErrs        :: [Error n] -> TI n ()
addErrs es      = TI $ lift $ lift $ put $ Seq.fromList es

addPreds       :: [Pred] -> TI n ()
addPreds ps     = TI $ put $ Seq.fromList ps

getPreds       :: TI n a -> TI n (a, [Pred])
getPreds (TI m) = TI $
  do (a,ps) <- collect m
     return (a, seqToList ps)

inExtEnv :: Ord n => Env n -> TI n a -> TI n a
inExtEnv env (TI m) = TI $
  do R envOld <- ask
     local (R $ fst $ Env.union env envOld) m

newTVar :: Kind -> TI n Type
newTVar k = TI $
  do s <- get
     let n = names s
     set s { names = n + 1 }
     return $ TAtom TVar $ TR n $ TParam "" $ Just k

unify :: Type -> Type -> TI n ()
unify t1 t2 =
  do s <- TI $ get
     let su1        = subst s
         substF     = apTVars (`lookupS` su1)
         (su2,errs) = mgu (substF t1) (substF t2)
     -- In case of error, we just use the partially computed subsitution.
     TI $ set s { subst = compS su2 su1 }
     addErrs $ map UnificationError errs

getEnv :: TI n (Env n)
getEnv = TI $
  do R env <- ask
     return env

seqToList :: Seq.Seq a -> [a]
seqToList = Seq.foldrWithIndex (const (:)) []



--------------------------------------------------------------------------------
generalize :: (Ord n, HasTVars t) => [Pred] -> t -> TI n (Qual t)
generalize ps t =
  do env <- getEnv
     let envVars    = freeTVars env
         genVars    = freeTVars t `Set.difference` envVars
         isExtern p = Set.null (freeTVars p `Set.intersection` genVars)
         (externalPreds, localPreds) = partition isExtern ps

         as   = Set.toList genVars

         -- signature needed due to the monomorhism restriction
         apSu :: HasTVars t => t -> t
         apSu  = apTVars $ \x@(TR _ p) -> do n <- elemIndex x as
                                             return $ TAtom TGen $ TR n p

     addPreds externalPreds
     return $ Forall [ a | TR _ a <- as ] (apSu localPreds) (apSu t)





mergeEnv :: Ord n => [Env n] -> TI n (Env n)
mergeEnv es =
  do let (e1, redef) = Env.unions es
     unless (Set.null redef) $ addErrs [ MultipleDefinitions redef ]
     return e1

-- By this point, all as should have kinds.
instantiate :: HasGVars t => Qual t -> TI n t
instantiate (Forall as ps t) =
  do ts <- mapM newTVar $ mapMaybe kindOf as
     addPreds $ map (apGVars ts) ps
     return $ apGVars ts t


