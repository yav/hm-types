{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module HMType.Monad.UnifyT
  ( UnifyT
  , runUnifyT
  , unify
  ) where

import MonadLib
import HMType.AST
import HMType.Subst

newtype UnifyT tc k m a = I ( ExceptionT (MguError tc k)
                            ( StateT (Subst tc k)
                              m) a)
      deriving (Functor,Monad)

instance MonadT (UnifyT tc k) where
  lift m = I (lift (lift m))

runUnifyT :: UnifyT tc k m a -> m (Either (MguError tc k) a, Subst tc k)
runUnifyT (I m) = runStateT emptyS $ runExceptionT m

unify :: (Monad m, Eq tc, KindOf tc k) =>
          HMType tc k -> HMType tc k -> UnifyT tc k m ()
unify t1 t2 = I $
  do su1 <- get
     case mgu (apS su1 t1) (apS su1 t2) of
       Right su2 -> set (compS su2 su1)
       Left err  -> raise err

                      
                     
