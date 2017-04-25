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


data S c      = S { subst         :: Subst c
                  , names         :: Int
                  }

newtype TI c n e a =
  TI (ReaderT (Env c n)
     (WriterT (Seq.Seq (Pred c))
     (WriterT (Seq.Seq (Error c n e))
     (StateT (S c)
      Id))) a) deriving (Monad)


runTI :: (HasTVars a c, HasTVars e c)
      => TI c n e a -> (a, [Error c n e], [Pred c])
runTI (TI m)  = (apSu a, apSu (seqToList errs), apSu (seqToList ps))
  where
  apSu x = apTVars (`lookupS` subst s) x
  (((a,ps), errs), s)
    = runId
    $ runStateT S { subst = emptyS, names = 0 }
    $ runWriterT
    $ runWriterT
    $ runReaderT Env.empty m


addErrs        :: [Error c n e] -> TI c n e ()
addErrs es      = TI $ lift $ lift $ put $ Seq.fromList es

addPreds       :: [Pred c] -> TI c n e ()
addPreds ps     = TI $ put $ Seq.fromList ps

getPreds       :: TI c n e a -> TI c n e (a, [Pred c])
getPreds (TI m) = TI $
  do (a,ps) <- collect m
     return (a, seqToList ps)

inExtEnv :: Ord n => Env c n -> TI c n e a -> TI c n e a
inExtEnv env (TI m) = TI $
  do envOld <- ask
     local (fst $ Env.union env envOld) m

newTVar :: Kind c -> TI c n e (Type c)
newTVar k = TI $
  do s <- get
     let n = names s
     set s { names = n + 1 }
     return $ TVar $ TV n $ TParam "" $ Just k

unify :: IsTCon c => Type c -> Type c -> TI c n e ()
unify t1 t2 =
  do s <- TI $ get
     let su1        = subst s
         substF     = apTVars (`lookupS` su1)
         (su2,errs) = mgu (substF t1) (substF t2)
     -- In case of error, we just use the partially computed subsitution.
     TI $ set s { subst = compS su2 su1 }
     addErrs $ map UnificationError errs

getEnv :: TI c n e (Env c n)
getEnv = TI $
  do env <- ask
     return env

seqToList :: Seq.Seq a -> [a]
seqToList = Seq.foldrWithIndex (const (:)) []



--------------------------------------------------------------------------------
generalize :: (Ord n, HasTVars t c) => [Pred c] -> t -> TI c n e (Qual c t)
generalize ps t =
  do env <- getEnv
     let envVars    = freeTVars env
         genVars    = freeTVars t `Set.difference` envVars
         isExtern p = Set.null (freeTVars p `Set.intersection` genVars)
         (externalPreds, localPreds) = partition isExtern ps

         as     = Set.toList genVars
         apSu z = apTVars (\x@(TV _ p) -> do n <- elemIndex x as
                                             return $ TGen $ GV n p) z

     addPreds externalPreds
     return $ Forall [ a | TV _ a <- as ] (apSu localPreds) (apSu t)





mergeEnv :: Ord n => [Env c n] -> TI c n e (Env c n)
mergeEnv es =
  do let (e1, redef) = Env.unions es
     unless (Set.null redef) $ addErrs [ MultipleDefinitions redef ]
     return e1

-- By this point, all as should have kinds.
instantiate :: HasGVars t c => Qual c t -> TI c n e t
instantiate (Forall as ps t) =
  do ts <- mapM newTVar $ mapMaybe kindOf as
     addPreds $ map (apGVars ts) ps
     return $ apGVars ts t


