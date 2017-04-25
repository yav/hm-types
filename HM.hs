-- | Implements basic functionality for Hindley-Miler type inference.
module HM
  (
  -- * Types
    Type(..), TCon(..), TVar(..)

  -- * Kinds of well-kinded types
  , HasKind(..), Kind
  , aKind, Sort

  -- * Unification and matching
  , mgu
  , match, TypePat

  -- * Substitutions
  , Subst, emptySubst, singleSubst, (@@), mergeSubst, substLookup, apSubst
  , HasTVars(..)

  -- * Schemas
  , generalize, instantiate, Qual(..), TParam(..), Prop, Schema, Rank2(..)
  ) where

import           Data.Text(Text)
import           Data.Vector(Vector)
import qualified Data.Vector as Vector
import           Data.Set(Set)
import qualified Data.Set as Set
import           Data.Map(Map)
import qualified Data.Map as Map
import           Data.Maybe(fromMaybe)
import           Data.List(foldl')
import           Data.Coerce(coerce)
import           Control.Monad(liftM,ap)


-- | Sorts classify kinds
type Sort     = Type

-- | Kinds classify types
type Kind     = Type

-- | A constraint
type Prop     = Type

-- | A type pattern
type TypePat  = Type

-- | A simple type
data Type     = TVar !TVar        -- ^ Unification variable
              | TCon !TCon        -- ^ Type constructor, or quantified variable
              | TApp !Type !Type  -- ^ Type application
                deriving Eq

-- | Type constrcutors and quantified vairables
data TCon     = TVBound !Int !Kind    -- ^ A quantified variable
              | TC !Int !Kind !Text   -- ^ A type constructor of the given kind
              | TCKind                -- ^ The sort of kinds


-- | Unificatoin variables
data TVar     = TV { tvId   :: !Int   -- Variable name
                   , tvKind :: !Kind  -- Variable kind
                   , tvText :: !Text  -- User readable name
                   }

-- | Type parameters
data TParam   = TParam { tpKind :: !Kind  -- ^ Kind
                       , tpText :: !Text  -- ^ User readable name
                       }

-- | A qualified entity
data Qual a   =
  Forall
  { qTParams :: {-# UNPACK #-} !(Vector TParam) -- ^ Type parameters
  , qProps   :: {-# UNPACK #-} !(Vector Prop)   -- ^ Constraints
  , qQual    :: !a                              -- ^ Qualified things
  }

-- | A rank-2 type
data Rank2    = PolyArg !(Qual Type) !Rank2 -- ^ Polymorphic parameter
              | MonoRes !Type               -- ^ Simple type

-- | A schema:  a qualifed rank-2 type
type Schema   = Qual Rank2

-- | The sort of all kinds
aKind :: Sort
aKind = TCon TCKind

--------------------------------------------------------------------------------
instance Eq TVar where
  x == y = tvId x == tvId y

instance Ord TVar where
  compare x y = compare (tvId x) (tvId y)

instance Eq TCon where
  x == y =
    case (x,y) of
      (TVBound a _,  TVBound b _) -> a == b
      (TC a _ _,       TC b _ _)  -> a == b
      (TCKind,         TCKind)    -> True
      _                           -> False

instance Ord TCon where
  compare x y =
    case (x,y) of
      (TVBound a _, TVBound b _)  -> compare a b
      (TVBound {}, _)             -> LT
      (_, TVBound {})             -> GT
      (TC a _ _, TC b _ _)        -> compare a b
      (TC _ _ _, _)               -> LT
      (_, TC _ _ _)               -> GT
      (TCKind, TCKind)            -> EQ

--------------------------------------------------------------------------------



-- | A finite mapping from type varibles to types.
-- This is used both for unification and matching.
--
--    * When used for unification, the keys are type variables,
--      and the substituion should be idempotent.
--    * When used for matching, the keys are type-pattern variables,
--      and the substitution is "conceptually" idempotent as the
--      variables in the domain and range are in different namespaces.
newtype Subst = Subst (Map TVar Type)

-- | The empty substitution does not bind anything.
emptySubst :: Subst
emptySubst = Subst Map.empty

-- | A singleton substitution.
-- Assumes that the variable does not appear in the types
singleSubst :: TVar -> Type -> Subst
singleSubst x t = Subst (Map.singleton x t)

-- | Compose two unfication-variable substitutions.
-- The RHS substitution is applied first.
-- Assumes that the variables bound by the first (RHS) substituion
-- do not appear free in the second (LHS) substituion.
(@@) :: Subst -> Subst -> Subst
su2@(Subst mp2) @@ su1 = Subst (Map.union mp1 mp2)
  where Subst mp1 = apSubst su2 su1

-- | Take the union of two type-pattern substitutions.
-- Fails if the two substitutions have different values
-- for the same pattern vairalbe.
mergeSubst :: Subst -> Subst -> Maybe Subst
mergeSubst (Subst su1) (Subst su2)
  | and (Map.intersectionWith (==) su1 su2) = Just $! Subst (Map.union su1 su2)
  | otherwise = Nothing

-- | Lookup the value of a type or type-pattern variable in a substitution
substLookup :: Subst -> TVar -> Maybe Type
substLookup (Subst mp) x = Map.lookup x mp

substEnterScopeMaybe :: Int -> Subst -> Maybe Subst
substEnterScopeMaybe n (Subst mp) = coerce (anyJust (enterScopeMaybe n) mp)

substEnterScope :: Int -> Subst -> Subst
substEnterScope n su = fromMaybe su (substEnterScopeMaybe n su)

-- | Apply a substituion to something that has free variables.
apSubst :: HasTVars t => Subst -> t -> t
apSubst su t = fromMaybe t (apSubstMaybe su t)


-- | Identify entities with free variables
class HasTVars t where
  fvs          :: t -> Set TVar
  -- ^ Compute the set of free variables

  apSubstMaybe :: Subst -> t -> Maybe t
  -- ^ Apply a substitution.
  -- Returns 'Nothing', if the substitution had no effect
  -- on the input (i.e., it is identity on the free variables of the input).

instance HasTVars Type where
  fvs ty = case ty of
             TVar x     -> Set.singleton x
             TCon {}    -> Set.empty
             TApp t1 t2 -> Set.union (fvs t1) (fvs t2)

  apSubstMaybe su ty =
    case ty of
      TVar x     -> substLookup su x
      TCon {}    -> Nothing
      TApp t1 t2 -> anyJust2 TApp (apSubstMaybe su) (apSubstMaybe su) t1 t2

instance HasTVars Subst where
  fvs (Subst mp) = foldl' add Set.empty mp
    where add x t = Set.union (fvs t) x

  apSubstMaybe su (Subst mp) = coerce (anyJust (apSubstMaybe su) mp)

instance HasTVars a => HasTVars (Vector a) where
  fvs = foldl' add Set.empty
    where add xs x = fvs x `Set.union` xs

  apSubstMaybe su = anyJust (apSubstMaybe su)

instance HasTVars a => HasTVars (Qual a) where
  fvs s = Set.union (fvs (qProps s)) (fvs (qQual s))
  apSubstMaybe su s = anyJust2 mk (apSubstMaybe su1) (apSubstMaybe su1)
                                  (qProps s) (qQual s)
    where
    mk ps t = s { qProps = ps, qQual = t }
    su1     = substEnterScope (Vector.length (qTParams s)) su



--------------------------------------------------------------------------------

enterScope :: Int -> Type -> Type
enterScope n t = fromMaybe t (enterScopeMaybe n t)

enterScopeMaybe :: Int -> Type -> Maybe Type
enterScopeMaybe 0   _ = Nothing
enterScopeMaybe n ty0 = go ty0
  where
  go ty = case ty of
            TVar {} -> Nothing
            TCon tc -> case tc of
                         TVBound x k -> Just $! TCon (TVBound (x + n) k)
                         _           -> Nothing
            TApp t1 t2 -> anyJust2 TApp go go t1 t2
--------------------------------------------------------------------------------


-- | Abstract the given variables and constraints from the simple type,
-- to compute a qualified type.
generalize :: Set TVar -> Vector Prop -> Type -> Qual Type
generalize vs ps t =
  Forall { qTParams = tps
         , qProps   = fmap gen ps
         , qQual    = gen t
         }
  where
  as        = Set.toList vs
  tps       = Vector.fromList (map toTP as)
  su        = Subst (Map.fromList (zipWith toSu [ 0 .. ] as))

  gen ty    = apSubst su (enterScope (Vector.length tps) ty)

  toTP tv   = TParam { tpKind = tvKind tv, tpText = tvText tv }
  toSu n tv = let ty = TCon (TVBound n (tvKind tv))
              in ty `seq` (tv, ty)



-- | Instantiate a qualified type with the given types.
-- Assumes that the provided types have the same length as the
-- type parameters of the qualified type.
instantiate :: Qual Type -> Vector Type -> (Vector Prop, Type)
instantiate s ts =
  let ps = fmap inst (qProps s)
      ty = inst (qQual s)
  in ps `seq` ty `seq` (ps,ty)
  where
  pNum         = Vector.length (qTParams s)

  inst ty      = fromMaybe ty (instMaybe ty)
  instMaybe ty =
    case ty of
      TVar {} -> Nothing
      TCon tc ->
        case tc of
          TC {} -> Nothing
          TCKind -> Nothing
          TVBound n k -> Just $!
            (if n < pNum then ts Vector.! n
                         else TCon (TVBound (n - pNum) k))
      TApp t1 t2 -> anyJust2 TApp instMaybe instMaybe t1 t2
--------------------------------------------------------------------------------


-- | Entities that have kinds
class HasKind t where
  kindOf :: t -> Kind
  -- ^ Compute the kind of the given entitity.
  -- Assumes that the entity has been validated.

instance HasKind TVar where
  kindOf = tvKind

instance HasKind TCon where
  kindOf tc =
    case tc of
      TVBound _ k -> k
      TC _ k _    -> k
      TCKind      -> TCon TCKind

instance HasKind Type where
  kindOf ty =
    case ty of
      TVar x -> kindOf x
      TCon c -> kindOf c
      TApp t1 _ -> case kindOf t1 of
                     TApp _ k    -> k
                     TCon TCKind -> TCon TCKind
                     _           -> error "kindOf: malformed kind."
--------------------------------------------------------------------------------


-- | Compute the most general unifier of two types, if any
mgu :: Type -> Type -> Maybe Subst
mgu t1 t2 =
  case (t1,t2) of
    (TVar x, _) -> bindVar x t2
    (_, TVar x) -> bindVar x t1
    (TCon c, TCon d) -> if c == d then Just emptySubst else Nothing
    (TApp x1 x2, TApp y1 y2) ->
      do s1 <- mgu x1 y1
         s2 <- mgu (apSubst s1 x2) (apSubst s1 y2)
         return $! (s2 @@ s1)
    _ -> Nothing


bindVar :: TVar -> Type -> Maybe Subst
bindVar x t =
  case t of
    TVar y | x == y          -> Just emptySubst
    _ | kindOf x /= kindOf t -> Nothing
      | occursIn t           -> Nothing
      | otherwise            -> Just $! singleSubst x t

  where
  occursIn ty =
    case ty of
      TVar y     -> x == y
      TCon _     -> False
      TApp t1 t2 -> occursIn t1 || occursIn t2


-- | Check to see if the type patterns matches the given type.
-- Note that the variables in the pattern are in a different
-- "namespace" than the variables in the type. As a result,
-- the variables in the keys, and in the type are conceptually
-- different.
match :: TypePat -> Type -> Maybe Subst
match pat ty =
  case (pat, ty) of
    (TVar x, _) -> bindVar x ty
    (TCon c, TCon d) -> if c == d then Just emptySubst else Nothing
    (TApp x1 x2, TApp y1 y2) ->
        do s1 <- match x1 y1
           s2 <- match x2 y2
           mergeSubst s1 s2
    _ -> Nothing

--------------------------------------------------------------------------------


-- | Apply a function to all elements of a container.
-- Returns `Nothing` if nothing changed, and @Just container@ otherwise.
anyJust :: Traversable t => (a -> Maybe a) -> t a -> Maybe (t a)
anyJust f m = mk $ (`runS` False) $ traverse upd m
  where
  mk (a,changes) = if changes then Just a else Nothing

  upd x = case f x of
            Just y  -> S (\_ -> (y,True))
            Nothing -> return x

newtype S a = S { runS :: Bool -> (a,Bool) }

instance Functor S where
  fmap = liftM

instance Applicative S where
  pure a = S (\s -> (a,s))
  (<*>)  = ap

instance Monad S where
  S m >>= f = S (\s -> case m s of
                         (a,s1) -> runS (f a) s1)

anyJust2 ::
  (a -> b -> c) -> (a -> Maybe a) -> (b -> Maybe b) -> a -> b -> Maybe c
anyJust2 mk f g a b =
  case (f a, g b) of
    (Nothing, Nothing) -> Nothing
    (Just a', Nothing) -> Just $! mk a' b
    (Nothing, Just b') -> Just $! mk a  b'
    (Just a', Just b') -> Just $! mk a' b'



