{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module HMType.Monad.UnifyT
  ( UnifyT
  , runUnifyT
  , unify
  ) where

import MonadLib
import HMType.AST
import HMType.Subst
import Control.Monad.Fix

newtype UnifyT m a  = I (ExceptionT MguError (StateT Subst m) a)
                        deriving (Functor,Monad,MonadFix)

instance MonadT UnifyT where
  lift m = I (lift (lift m))

runUnifyT :: UnifyT m a -> m (Either MguError a, Subst)
runUnifyT (I m) = runStateT emptyS $ runExceptionT m

unify :: Monad m => Type -> Type -> UnifyT m ()
unify t1 t2 = I $
  do su1 <- get
     case mgu (apS su1 t1) (apS su1 t2) of
       Right su2 -> set (compS su2 su1)
       Left err  -> raise err

                      
                     
