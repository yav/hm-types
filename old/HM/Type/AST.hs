{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
module HM.Type.AST
  ( -- * Types
    Type(..)
  , TVar(..)
  , GVar(..)
  , Kind
  , IsTCon
  , splitTApp
  , tApp
  , Pred
  , Schema
  , TParam(..)
  , Qual(..)
  , NameOf(..)

  -- * Kinds
  , KindOf(..)
  -- , HasKinds(..)

  -- * Substitutions
  , HasTVars(..)
  , HasGVars(..)
  -- , HasTCons(..)
  ) where

import Data.Maybe(fromMaybe)
import qualified Data.Set as S


--------------------------------------------------------------------------------
-- | Simple types.
data Type c = TApp (Type c) (Type c)  -- ^ Type application
            | TCon c                  -- ^ Type constant
            | TVar (TVar c)           -- ^ Unification variable (unknown type).
            | TGen (GVar c)           -- ^ Universally quanitifed variable
              deriving (Eq,Show)

data TVar c = TV Int (TParam c) deriving Show
data GVar c = GV Int (TParam c) deriving Show

instance Eq  (TVar c) where TV x _ == TV y _          = x == y
instance Ord (TVar c) where compare (TV x _) (TV y _) = compare x y

instance Eq  (GVar c) where GV x _ == GV y _          = x == y
instance Ord (GVar c) where compare (GV x _) (GV y _) = compare x y


-- | Kinds, classifing types.
type Kind = Type

-- | Predicates, restricting polymorphism.
type Pred = Type

-- | Qualified types.
type Schema c   = Qual c (Type c)

-- | Type \"paramaters\".
-- A sugegsted name for display purposes, together with the type (kind)
-- of a parameter.
data TParam c     = TParam String (Maybe (Kind c))
                    deriving Show


-- | Split-off all type applications.
-- The first component of the result will not be an application node.
splitTApp :: Type c -> (Type c, [Type c])
splitTApp ty = split ty []
  where split (TApp t1 t2) ts = split t1 (t2 : ts)
        split t ts            = (t,ts)

-- | Apply a type to the given type arguments.
-- A convenience function.
tApp :: Type c -> [Type c] -> Type c
tApp = foldl TApp

-- | The type of qualified entities.
data Qual c a = Forall [TParam c] [Pred c] a
                  deriving Show


class NameOf t where
  nameOf :: t -> String

instance NameOf (TParam c) where
  nameOf (TParam s _) = s

instance NameOf (TVar c) where
  nameOf (TV _ p) = nameOf p

instance NameOf (GVar c) where
  nameOf (GV _ p) = nameOf p

--------------------------------------------------------------------------------

class (Eq c, KindOf c c) => IsTCon c


-- | This class defines a method for accessing the kind of something.
class KindOf t c | t -> c where
  kindOf :: t -> Maybe (Kind c)

instance KindOf (TParam c) c where
  kindOf (TParam _ k) = k

instance KindOf (TVar c) c where
  kindOf (TV _ p) = kindOf p

instance KindOf (GVar c) c where
  kindOf (GV _ p) = kindOf p

instance KindOf c c => KindOf (Type c) c where
  kindOf ty =
    case ty of
      TVar tv   -> kindOf tv
      TGen gv   -> kindOf gv
      TCon c    -> kindOf c      
      TApp t1 _ -> do TApp _ res <- kindOf t1
                      return res

--------------------------------------------------------------------------------

-- | Fill-in some of the type variables in something.
class HasTVars t c | t -> c where
  apTVars   :: (TVar c -> Maybe (Type c)) -> t -> t
  freeTVars :: t -> S.Set (TVar c)

instance HasTVars (Type c) c where
  apTVars su ty =
    case ty of
      TApp t1 t2 -> TApp (apTVars su t1) (apTVars su t2)
      TCon _     -> ty
      TVar tvar  -> fromMaybe ty (su tvar)
      TGen _     -> ty

  freeTVars ty =
    case ty of
      TApp t1 t2  -> S.union (freeTVars t1) (freeTVars t2)
      TVar tv     -> S.singleton tv
      TGen _      -> S.empty
      TCon _      -> S.empty

instance HasTVars t c => HasTVars [t] c where
  apTVars su xs = map (apTVars su) xs
  freeTVars xs  = S.unions (map freeTVars xs)

instance HasTVars t c => HasTVars (Qual c t) c where
  apTVars su (Forall as ps t) = Forall as (apTVars su1 ps) (apTVars su1 t)
    where su1 x = avoidCapture as `fmap` su x
  freeTVars (Forall _ ps t)   = S.union (freeTVars ps) (freeTVars t)


-- | Eliminate some of the generic variables, as part of instantiation.
class HasGVars t c | t -> c where
  apGVars :: [Type c] -> t -> t

instVar :: TParam c -> [Type c] -> Int -> Type c
instVar _ (t : _)  0 = t
instVar p (_ : ts) n = instVar p ts (n-1)
instVar p []       n = TGen (GV n p)

avoidCapture :: [TParam c] -> Type c -> Type c
avoidCapture [] = id
avoidCapture as = inc
  where
  bound = length as

  rename (TParam n k) | n `elem` names  = TParam ('^' : n) k
    where names = [ x | TParam x _ <- as ]
  rename p                              = p

  inc ty = case ty of
             TApp t1 t2     -> TApp (inc t1) (inc t2)
             TVar _         -> ty
             TCon _         -> ty
             TGen (GV x p)  -> TGen (GV (x + bound) (rename p))


instance HasGVars (Type c) c where
  apGVars su ty =
    case ty of
      TApp t1 t2    -> TApp (apGVars su t1) (apGVars su t2)
      TCon _        -> ty
      TVar _        -> ty
      TGen (GV n p) -> instVar p su n

instance HasGVars t c => HasGVars [t] c where
  apGVars su xs  = map (apGVars su) xs

instance HasGVars t c => HasGVars (Qual c t) c where
  apGVars su (Forall as ps t) = Forall as (apGVars su1 ps) (apGVars su1 t)
    where su1 = zipWith unchanged [0..] as ++ map (avoidCapture as) su
          unchanged n p = TGen (GV n p)


--------------------------------------------------------------------------------
-- Kinds, the types of types

{-
-- | Apply a function to all the kinds in the given entity.
class HasKinds t where
  mapKinds :: (Maybe Kind -> Maybe Kind) -> t -> t

instance HasKinds TParam where
  mapKinds f (TParam s k) = TParam s (f k)

instance HasKinds TRef where
  mapKinds f (TR n p)     = TR n (mapKinds f p)

-- We are updating the types of the types here.
instance HasKinds Type where
  mapKinds f ty =
    case ty of
      TApp t1 t2    -> TApp (mapKinds f t1) (mapKinds f t2)
      TAtom atom r  -> TAtom atom (mapKinds f r)

instance HasKinds t => HasKinds [t] where
  mapKinds f xs  = map (mapKinds f) xs

instance HasKinds t => HasKinds (Qual t) where
  mapKinds f (Forall as ps t) =
                      Forall (mapKinds f as) (mapKinds f ps) (mapKinds f t)


--------------------------------------------------------------------------------
class HasTCons t where
  mapTCons :: (TRef -> TRef) -> t -> t

instance HasTCons Type where
  mapTCons f ty =
    case ty of
      TApp t1 t2  -> TApp (mapTCons f t1) (mapTCons f t2)
      TAtom atom r ->
        case atom of
          TCon -> TAtom TCon (f r)
          TVar -> ty
          TGen -> ty

instance HasTCons t => HasTCons [t] where
  mapTCons f xs  = map (mapTCons f) xs

instance HasTCons t => HasTCons (Qual t) where
  mapTCons f (Forall as ps t) = Forall as (mapTCons f ps) (mapTCons f t)
-}




