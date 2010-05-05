{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}   -- pfft
module HMType.Subst
  (
  -- * Substitutions
    Subst
  , apS
  , lookupS
  , compS
  , mergeS
  , emptyS
  , singleS
  , mgu
  , match

  , TVarBindError(..)
  , MguError(..)
  ) where

import HMType.AST
import qualified Data.IntMap as M
import qualified Data.Set as S
import Text.PrettyPrint.HughesPJClass


-- | What may go wrong when we try to bind a type variable to a type.
data TVarBindError  = KindMismatch | RecursiveType
                      deriving (Eq,Show)

-- | What may go wrong while computing the most general unifier of two types.
data MguError tc k  = TVarBindError TVarBindError (TVar k) (HMType tc k)
                    | TypeMismatch (HMType tc k) (HMType tc k)
                      deriving Eq

-- | A substitution associating type variables with terms.
newtype Subst tc k  = Su (M.IntMap (HMType tc k, String))

instance (Pretty k, Pretty (TConApp tc k)) => Pretty (Subst tc k) where
  pPrintPrec l _ (Su su) = braces $ fsep $ map pp $ M.toList su
    where pp (_,(t,n))  = text n <+> char '=' <+> pPrintPrec l 0 t

-- | Find the binding for a unfication variable, if any.
lookupS :: TVar k -> Subst tc k -> Maybe (HMType tc k)
lookupS (TV x _) (Su m) = fst `fmap` M.lookup x m

-- | Compute the most general unifier of two terms, if possible.
mgu :: (KindOf tc k, Eq tc) => HMType tc k -> HMType tc k
             -> Either (MguError tc k) (Subst tc k)
mgu (TVar x) t = bindVar x t
mgu t (TVar x) = bindVar x t
mgu (TApp s1 s2) (TApp t1 t2) =
  case mgu s1 t1 of
    Left err -> Left err
    Right su1 ->
      case mgu (apS su1 s2) (apS su1 t2) of
        Left err  -> Left err
        Right su2 -> Right (compS su2 su1) 
mgu (TCon c) (TCon d) | c == d  = Right emptyS
mgu t1 t2 = Left (TypeMismatch t1 t2)

bindVar :: KindOf tc k => TVar k -> HMType tc k
        -> Either (MguError tc k) (Subst tc k)
bindVar x t = case singleS x t of
                Left err -> Left (TVarBindError err x t)
                Right s  -> Right s
              

-- | Check if a type pattern (1st argument) matches a type (2nd argument).
-- Unification variables in the type are treated as constants:
-- 
--   * they are equal only to themselves,
--
--   * they cannot be bound, and
--
--   * they are considered to be distinct from the variables in the pattern.
--
-- Examples:
--
--  * @match /x/        [a]         == Just { /x/ = [a] }@
--
--  * @match [/x/]      a           == Nothing@
--
--  * @match /x/        [x]         == Just { /x/ = [x] }@
--
--  * @match (/x/,/x/)  ([a], [a])  == Just { /x/ = [a] }@
--
--  * @match (/x/,/x/)  ([a], [b])  == Nothing@
match :: (KindOf tc k, Eq tc)
      => HMType tc k -> HMType tc k
      -> Maybe (Subst tc k)
match (TVar x) t
  | kindOf x /= kindOf t  = Nothing
match (TVar (TV x (TParam name _))) t   = Just (Su (M.singleton x (t,name)))
match (TApp s1 s2) (TApp t1 t2) =
  do su1 <- match s1 t1
     su2 <- match s2 t2
     mergeS su2 su1
match (TCon c) (TCon d) | c == d  = return emptyS
match _ _                         = Nothing


apS :: HasUVars t tc k => Subst tc k -> t -> t
apS su = apUni (`lookupS` su)

compS :: Subst tc k -> Subst tc k -> Subst tc k
compS s2@(Su su2) (Su su1) = Su (M.union (apS2 `M.map` su1) su2)
  where apS2 (t,n) = (apS s2 t, n)

mergeS :: Eq tc => Subst tc k -> Subst tc k -> Maybe (Subst tc k)
mergeS (Su su1) (Su su2)
  | M.fold (&&) True (M.intersectionWith (\a b -> fst a == fst b) su1 su2) =
          Just (Su (M.union su1 su2))
  | otherwise = Nothing

emptyS :: Subst tc k
emptyS = Su M.empty

singleS :: KindOf tc k
        => TVar k -> HMType tc k
        -> Either TVarBindError (Subst tc k)
singleS x (TVar y) | x == y                   = Right emptyS
singleS v t        | kindOf v /= kindOf t     = Left KindMismatch
singleS v t        | v `S.member` freeTVars t = Left RecursiveType
singleS (TV x (TParam name _)) t        = Right (Su (M.singleton x (t,name)))

--------------------------------------------------------------------------------




