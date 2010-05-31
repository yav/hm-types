module HM.Infer.Env
  ( Env
  , empty
  , lookup
  , singleton
  , union
  , toList
  , fromList
  ) where

import HM.Type.AST
import qualified Data.Map as Map
import qualified Data.Set as Set

import Prelude hiding (lookup)


newtype Env n   = E (Map.Map n (Qual Type))
                  deriving Show

empty          :: Env n
empty           = E Map.empty

lookup         :: Ord n => n -> Env n -> Maybe (Qual Type)
lookup x (E m)  = Map.lookup x m

singleton      :: Ord n => n -> Qual Type -> Env n
singleton x s   = E (Map.singleton x s)

toList         :: Env n -> [(n, Qual Type)]
toList (E m)    = Map.toList m

fromList       :: Ord n => [(n, Qual Type)] -> Env n
fromList xs     = E (Map.fromList xs)

-- | Left biased
union          :: Ord n => Env n -> Env n -> (Env n, Set.Set n)
union (E m1) (E m2)  = (E (Map.union m1 m2), redef)
  where redef = Map.keysSet (Map.intersection m1 m2)

instance Ord n => HasTVars (Env n) where
  apTVars f (E m) = E $ Map.map (apTVars f) m
  freeTVars (E m) = Set.unions $ map freeTVars $ Map.elems m



