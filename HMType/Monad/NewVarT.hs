{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module HMType.Monad.NewVarT
  ( NewVarT
  , runNewVarT
  , newUVar
  ) where

import MonadLib
import HMType.AST

newtype NewVarT m a = I (StateT Int m a) deriving (Functor,Monad)

instance MonadT NewVarT where
  lift m = I (lift m)

runNewVarT :: Monad m => NewVarT m a -> m a
runNewVarT (I m) = do (a,_) <- runStateT 0 m
                      return a

newUVar :: Monad m => TParam k -> NewVarT m (HMType tc k)
newUVar p = I $
  do x <- get
     set (x+1)
     return (TVar (TV x p))


                      
                     
