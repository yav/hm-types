module Example.Infer.Env
  ( Env
  , empty
  , lookup
  , singleton
  , union
  , toList
  , fromList
  ) where

import Example.Decls
import HMType.AST
import qualified Data.Map as Map
import qualified Data.Set as Set

import Prelude hiding (lookup)


newtype Env     = E (Map.Map Name (Qual Type))
                  deriving Show

empty          :: Env
empty           = E Map.empty

lookup         :: Name -> Env -> Maybe (Qual Type)
lookup x (E m)  = Map.lookup x m

singleton      :: Name -> Qual Type -> Env
singleton x s   = E (Map.singleton x s)

toList         :: Env -> [(Name, Qual Type)]
toList (E m)    = Map.toList m

fromList       :: [(Name, Qual Type)] -> Env
fromList xs     = E (Map.fromList xs)

-- | Left biased
union          :: Env -> Env -> (Env, Set.Set Name)
union (E m1) (E m2)  = (E (Map.union m1 m2), redef)
  where redef = Map.keysSet (Map.intersection m1 m2)

instance HasTVars Env where
  apTVars f (E m) = E $ Map.map (apTVars f) m
  freeTVars (E m) = Set.unions $ map freeTVars $ Map.elems m



