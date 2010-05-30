{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Example.Infer.Monad
  ( TI
  , runTI
  , addErrs
  , addPreds, getPreds
  , inExtEnv, getEnv
  , newTVar, unify
  ) where

import qualified Example.Infer.Env as Env
import Example.Infer.Env(Env)
import Example.Infer.Error

import HMType.AST
import HMType.Subst

import MonadLib
import qualified Data.Sequence as Seq


data R        = R Env
type W        = Seq.Seq Pred
data S        = S { subst         :: Subst
                  , names         :: Int
                  }

newtype TI a  = TI (ReaderT R
                   (WriterT W
                   (WriterT (Seq.Seq Error)
                   (StateT S
                     Id))) a)
                deriving (Monad)


runTI :: HasTVars a => TI a -> (a, [Error], [Pred])
runTI (TI m)  = (apSu a, apSu (seqToList errs), apSu (seqToList ps))
  where
  apSu x = apTVars (`lookupS` subst s) x
  (((a,ps), errs), s)
    = runId
    $ runStateT S { subst = emptyS, names = 0 }
    $ runWriterT
    $ runWriterT
    $ runReaderT (R Env.empty) m


addErrs        :: [Error] -> TI ()
addErrs es      = TI $ lift $ lift $ put $ Seq.fromList es

addPreds       :: [Pred] -> TI ()
addPreds ps     = TI $ put $ Seq.fromList ps

getPreds       :: TI a -> TI (a, [Pred])
getPreds (TI m) = TI $
  do (a,ps) <- collect m
     return (a, seqToList ps)

inExtEnv :: Env -> TI a -> TI a
inExtEnv env (TI m) = TI $
  do R envOld <- ask
     local (R $ fst $ Env.union env envOld) m

newTVar :: Kind -> TI Type
newTVar k = TI $
  do s <- get
     let n = names s
     set s { names = n + 1 }
     return $ TAtom TVar $ TR n $ TParam "" $ Just k

unify :: Type -> Type -> TI ()
unify t1 t2 =
  do s <- TI $ get
     let su1        = subst s
         substF     = apTVars (`lookupS` su1)
         (su2,errs) = mgu (substF t1) (substF t2)
     -- In case of error, we just use the partially computed subsitution.
     TI $ set s { subst = compS su2 su1 }
     addErrs $ map UnificationError errs

getEnv :: TI Env
getEnv = TI $
  do R env <- ask
     return env

seqToList :: Seq.Seq a -> [a]
seqToList = Seq.foldrWithIndex (const (:)) []

