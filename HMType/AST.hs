module HMType.AST
  ( -- * Types
    Type(..)
  , Atom(..)
  , Kind
  , splitTApp
  , Pred
  , TParam(..)
  , TRef (..)
  , Qual(..)
  , NameOf(..)

  -- * Kinds
  , KindOf(..)
  , HasKinds(..)

  -- * Substitutions
  , HasTVars(..)
  , HasGVars(..)
  , HasTCons(..)
  ) where

import Data.Maybe(fromMaybe)
import qualified Data.Set as S


--------------------------------------------------------------------------------
-- | Simple types.
data Type = TApp Type Type    -- ^ Type application
          | TAtom Atom TRef
            deriving (Eq,Show)

-- | The shapes of atomic types.
data Atom = TCon  -- ^ Type constant
          | TVar  -- ^ Unification variable (unknown type).
          | TGen  -- ^ Generic variable (placeholder in a schema).
            deriving (Eq,Show)

-- | Kinds, classifing types.
type Kind = Maybe Type

-- | Predicates, restricting polymorphism.
type Pred = Type

-- | Type \"reference\".
-- This type is used for unifciation and generic variables,
-- and also type constructors.
data TRef = TR Int TParam deriving Show

instance Eq TRef where
  TR x _ == TR y _ = x == y

instance Ord TRef where
  compare (TR x _) (TR y _) = compare x y


-- | Type \"paramaters\".
-- A sugegsted name for display purposes, together with the type (kind)
-- of a parameter.
data TParam       = TParam String Kind
                    deriving Show


-- | Split-off all type applications.
-- The first component of the result will not be an application node.
splitTApp :: Type -> (Atom, TRef, [Type])
splitTApp t  = split t []
  where split (TApp t1 t2) ts = split t1 (t2 : ts)
        split (TAtom a r) ts  = (a,r,ts)

-- | The type of qualified entities.
data Qual a = Forall [TParam] [Pred] a
                deriving Show


class NameOf t where
  nameOf :: t -> String

instance NameOf TParam where
  nameOf (TParam s _) = s

instance NameOf TRef where
  nameOf (TR _ p) = nameOf p

--------------------------------------------------------------------------------

-- | This class defines a method for accessing the kind of something.
class KindOf t where
  kindOf :: t -> Kind

instance KindOf TParam where
  kindOf (TParam _ k) = k

instance KindOf TRef where
  kindOf (TR _ p) = kindOf p

instance KindOf Type where
  kindOf ty =
    case ty of
      TAtom _ r -> kindOf r
      TApp t1 _ -> do TApp _ res <- kindOf t1
                      return res

--------------------------------------------------------------------------------

-- | Fill-in some of the type variables in something.
class HasTVars t where
  apTVars   :: (TRef -> Maybe Type) -> t -> t
  freeTVars :: t -> S.Set TRef

instance HasTVars Type where
  apTVars su ty =
    case ty of
      TApp t1 t2 -> TApp (apTVars su t1) (apTVars su t2)
      TAtom atom tvar ->
        case atom of
          TVar -> fromMaybe ty (su tvar)
          TGen -> ty
          TCon -> ty

  freeTVars ty =
    case ty of
      TApp t1 t2  -> S.union (freeTVars t1) (freeTVars t2)
      TAtom atom r ->
        case atom of
          TVar -> S.singleton r
          TGen -> S.empty
          TCon -> S.empty

instance HasTVars t => HasTVars [t] where
  apTVars su xs = map (apTVars su) xs
  freeTVars xs  = S.unions (map freeTVars xs)

instance HasTVars t => HasTVars (Qual t) where
  apTVars su (Forall as ps t) = Forall as (apTVars su1 ps) (apTVars su1 t)
    where su1 x = avoidCapture as `fmap` su x
  freeTVars (Forall _ ps t)   = S.union (freeTVars ps) (freeTVars t)


-- | Eliminate some of the generic variables, as part of instantiation.
class HasGVars t where
  apGVars :: [Type] -> t -> t

instVar :: TParam -> [Type] -> Int -> Type
instVar _ (t : _)  0 = t
instVar p (_ : ts) n = instVar p ts (n-1)
instVar p []       n = TAtom TGen (TR n p)


avoidCapture :: [TParam] -> Type -> Type
avoidCapture [] = id
avoidCapture as = inc
  where
  bound = length as

  rename (TParam n k) | n `elem` names  = TParam ('^' : n) k
    where names = [ x | TParam x _ <- as ]
  rename p                              = p

  inc ty = case ty of
             TApp t1 t2 -> TApp (inc t1) (inc t2)
             TAtom atom (TR x p) ->
               case atom of
                 TGen -> TAtom TGen (TR (x + bound) (rename p))
                 TVar -> ty
                 TCon -> ty





instance HasGVars Type where
  apGVars su ty =
    case ty of
      TApp t1 t2 -> TApp (apGVars su t1) (apGVars su t2)
      TAtom atom (TR n p) ->
        case atom of
          TGen -> instVar p su n
          TCon -> ty
          TVar -> ty

instance HasGVars t => HasGVars [t] where
  apGVars su xs  = map (apGVars su) xs

instance HasGVars t => HasGVars (Qual t) where
  apGVars su (Forall as ps t) = Forall as (apGVars su1 ps) (apGVars su1 t)
    where su1 = zipWith unchanged [0..] as ++ map (avoidCapture as) su
          unchanged n p = TAtom TGen (TR n p)


--------------------------------------------------------------------------------
-- Kinds, the types of types


-- | Apply a function to all the kinds in the given entity.
class HasKinds t where
  mapKinds :: (Kind -> Kind) -> t -> t

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





