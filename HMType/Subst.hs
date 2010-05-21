module HMType.Subst
  ( Subst
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


-- | What may go wrong when we try to bind a type variable to a type.
data TVarBindError  = KindMismatch | RecursiveType
                      deriving (Eq,Show)

-- | What may go wrong while computing the most general unifier of two types.
data MguError       = TVarBindError TVarBindError TRef Type
                    | TypeMismatch Type Type
                      deriving Eq

-- | A substitution associating type variables with terms.
newtype Subst       = Su (M.IntMap (Type, String))

-- | Find the binding for a unfication variable, if any.
lookupS :: TRef -> Subst -> Maybe Type
lookupS (TR x _) (Su m) = fst `fmap` M.lookup x m

-- | Compute the most general unifier of two terms, if possible.
-- If the list of errors is empty, then the first component is
-- the most general unifier.  Otherwise, we return a "best-effort"
-- partial unifier.  This may be useful to support reporing multiple
-- errors.
mgu :: Type -> Type -> (Subst, [MguError])
mgu (TVar x) t = bindVar x t
mgu t (TVar x) = bindVar x t
mgu (TApp s1 s2) (TApp t1 t2) =
  let (su1,errs1) = mgu s1 t1
      (su2,errs2) = mgu (apS su1 s2) (apS su1 t2)
  in (compS su2 su1, errs1 ++ errs2)
mgu (TCon c) (TCon d) | c == d  = (emptyS, [])
mgu t1 t2 = (emptyS, [TypeMismatch t1 t2])

bindVar :: TRef -> Type -> (Subst, [MguError])
bindVar x t = case singleS x t of
                Left err -> (emptyS, [TVarBindError err x t])
                Right s  -> (s, [])


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
match :: Type -> Type -> Maybe Subst
match (TVar x) t
  | kindOf x /= kindOf t  = Nothing
match (TVar (TR x (TParam name _))) t   = Just (Su (M.singleton x (t,name)))
match (TApp s1 s2) (TApp t1 t2) =
  do su1 <- match s1 t1
     su2 <- match s2 t2
     mergeS su2 su1
match (TCon c) (TCon d) | c == d  = return emptyS
match _ _                         = Nothing


apS :: HasTVars t => Subst -> t -> t
apS su = apTVars (`lookupS` su)

compS :: Subst -> Subst -> Subst
compS s2@(Su su2) (Su su1) = Su (M.union (apS2 `M.map` su1) su2)
  where apS2 (t,n) = (apS s2 t, n)

mergeS :: Subst -> Subst -> Maybe Subst
mergeS (Su su1) (Su su2)
  | M.fold (&&) True (M.intersectionWith (\a b -> fst a == fst b) su1 su2) =
          Just (Su (M.union su1 su2))
  | otherwise = Nothing

emptyS :: Subst
emptyS = Su M.empty

singleS :: TRef -> Type -> Either TVarBindError Subst
singleS x (TVar y) | x == y                   = Right emptyS
singleS v t        | kindOf v /= kindOf t     = Left KindMismatch
singleS v t        | v `S.member` freeTVars t = Left RecursiveType
singleS (TR x (TParam name _)) t        = Right (Su (M.singleton x (t,name)))




