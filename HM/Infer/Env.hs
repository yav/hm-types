{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
module HM.Infer.Env
  ( Env
  , empty
  , lookup
  , singleton
  , union
  , unions
  , toList
  , fromList
  ) where

import HM.Type.AST
import qualified Data.Map as Map
import qualified Data.Set as Set

import Prelude hiding (lookup)


newtype Env c n = E (Map.Map n (Schema c))
                  deriving Show

empty          :: Env c n
empty           = E Map.empty

lookup         :: Ord n => n -> Env c n -> Maybe (Schema c)
lookup x (E m)  = Map.lookup x m

singleton      :: Ord n => n -> Schema c -> Env c n
singleton x s   = E (Map.singleton x s)

toList         :: Env c n -> [(n, Schema c)]
toList (E m)    = Map.toList m

fromList       :: Ord n => [(n, Schema c)] -> Env c n
fromList xs     = E (Map.fromList xs)

-- | Left biased
union          :: Ord n => Env c n -> Env c n -> (Env c n, Set.Set n)
union (E m1) (E m2)  = (E (Map.union m1 m2), redef)
  where redef = Map.keysSet (Map.intersection m1 m2)

-- | Left biased
unions         :: Ord n => [Env c n] -> (Env c n, Set.Set n)
unions          = foldr jn (empty, Set.empty)
  where jn env (env1, redef1) = let (env2, redef2) = union env env1
                                in (env2, Set.union redef2 redef1) 
       

instance Ord n => HasTVars (Env c n) c where
  apTVars f (E m) = E $ Map.map (apTVars f) m
  freeTVars (E m) = Set.unions $ map freeTVars $ Map.elems m


