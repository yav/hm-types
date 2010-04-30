{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}   -- pfft
module HMType.AST
  ( -- * Types
    HMType(..)
  , splitTApp
  , Pred
  , TParam(..)
  , TVar(..)
  , Qual(..)
  , Schema

  -- * Kinds
  , IsKind(..)
  , KindOf(..)
  , HasKinds(..)

  -- * Substitutions
  , UnificationMonad(..)
  , HasUVars(..)
  , HasGVars(..)
  , Subst
  , lookupS
  , compS
  , emptyS
  , singleS
  , mgu

  -- * Pretty printing
  , TConApp(..)
  , prettyTApp
  ) where

import qualified Data.IntMap as M
import qualified Data.Set as S
import Text.PrettyPrint.HughesPJClass


--------------------------------------------------------------------------------
-- | Simple types.
data HMType tc k  = TCon tc                           -- ^ Type constructor
                  | TApp (HMType tc k) (HMType tc k)  -- ^ Type application
                  | TVar (TVar k)                     -- ^ Unification variable
                  | TGen (TVar k)                     -- ^ Generic variable
                    deriving Eq


-- | Split-off all type applications.
-- The first component of the result will not be an application node.
splitTApp :: HMType tc k -> (HMType tc k, [HMType tc k])
splitTApp t  = split t []
  where split (TApp t1 t2) ts = split t1 (t2 : ts)
        split tf ts           = (tf, ts)

-- | Predicates, restricting polymorphism.
type Pred         = HMType

-- | Type paramaters.
data TParam k     = TParam String k

-- | Type variables.
-- This type is used for both unifciation and generic variables. 
data TVar k       = TV Int (TParam k)

-- | The type of qualified entities.
data Qual tc k a    = Forall [TParam k] [Pred tc k] a

-- | A schema is qualified type.
type Schema tc k    = Qual tc k (HMType tc k)


instance Eq (TVar k) where
  TV x _ == TV y _ = x == y

instance Ord (TVar k) where
  compare (TV x _) (TV y _) = compare x y
--------------------------------------------------------------------------------


-- | Identifies kinds that correspond to kind functions.
class Eq k => IsKind k where
  isKFun :: k -> Maybe (k,k)

-- | This class defines a method for accessing the kind of something.
class IsKind k => KindOf t k | t -> k where
  kindOf :: t -> k

instance IsKind k => KindOf (TParam k) k where
  kindOf (TParam _ k) = k

instance IsKind k => KindOf (TVar k) k where
  kindOf (TV _ p) = kindOf p

instance KindOf tc k => KindOf (HMType tc k) k where
  kindOf ty =
    case ty of
      TCon tcon   -> kindOf tcon
      TVar tvar   -> kindOf tvar
      TGen tvar   -> kindOf tvar
      TApp t1 _   ->
        case isKFun (kindOf t1) of
          Just (_,b)  -> b
          _           -> error "BUG: Malformed type."




--------------------------------------------------------------------------------

-- | Fill-in some of the unfication variables in something.
class HasUVars t tc k | t -> tc k where
  apS       :: Subst tc k -> t -> t
  freeTVars :: t -> S.Set (TVar k)

instance HasUVars (HMType tc k) tc k where
  apS su ty =
    case ty of
      TVar tvar ->
        case lookupS tvar su of
          Just t  -> t
          Nothing -> ty
      TApp t1 t2  -> TApp (apS su t1) (apS su t2)
      TGen _      -> ty
      TCon _      -> ty

  freeTVars ty =
    case ty of
      TVar tvar   -> S.singleton tvar
      TApp t1 t2  -> S.union (freeTVars t1) (freeTVars t2)
      TGen _      -> S.empty
      TCon _      -> S.empty


instance HasUVars t tc k => HasUVars [t] tc k where
  apS su xs     = map (apS su) xs
  freeTVars xs  = S.unions (map freeTVars xs)

instance HasUVars t tc k => HasUVars (Qual tc k t) tc k where
  apS su (Forall as ps t)   = Forall as (apS su ps) (apS su t)
  freeTVars (Forall _ ps t) = S.union (freeTVars ps) (freeTVars t)



-- | Eliminate some of the generic variables, as part of instantiation.
class HasGVars t tc k | t -> tc k where
  apGS :: [HMType tc k] -> t -> t

instVar :: TParam k -> [HMType tc k] -> Int -> HMType tc k
instVar _ (t : _)  0 = t  -- check that the kinds match?
instVar p (_ : ts) n = instVar p ts (n-1)
instVar p []       n = TGen (TV n p)

instance HasGVars (HMType tc k) tc k where
  apGS su ty =
    case ty of
      TGen (TV n p)  -> instVar p su n
      TApp t1 t2     -> TApp (apGS su t1) (apGS su t2)
      TCon _         -> ty
      TVar _         -> ty

instance HasGVars t tc k => HasGVars [t] tc k where
  apGS su xs  = map (apGS su) xs

instance HasGVars t tc k => HasGVars (Qual tc k t) tc k where
  apGS su (Forall as ps t) = Forall as (apGS su1 ps) (apGS su1 t)
    where su1 = zipWith unchanged [0..] as ++ map inc su
          unchanged n p = TGen (TV n p)

          bound = length as

          rename (TParam n k) | n `elem` names  = TParam ('^' : n) k
            where names = [ x | TParam x _ <- as ]
          rename p                              = p

          inc ty = case ty of
                     TGen (TV x p)  -> TGen (TV (x + bound) (rename p))
                     TVar _         -> ty
                     TCon _         -> ty
                     TApp t1 t2     -> TApp (inc t1) (inc t2)


--------------------------------------------------------------------------------
-- Kinds, the types of types


class HasKinds t k | t -> k where
  mapKinds :: (k -> k) -> t -> t

instance HasKinds (TParam k) k where
  mapKinds f (TParam s k) = TParam s (f k)

instance HasKinds (TVar k) k where
  mapKinds f (TV n p)     = TV n (mapKinds f p)

-- We are updating the types of the types here.
instance (HasKinds tc k) => HasKinds (HMType tc k) k where
  mapKinds f ty =
    case ty of
      TApp t1 t2  -> TApp (mapKinds f t1) (mapKinds f t2)
      TCon tcon   -> TCon (mapKinds f tcon)
      TVar tvar   -> TVar (mapKinds f tvar)
      TGen tvar   -> TGen (mapKinds f tvar)

instance HasKinds t k => HasKinds [t] k where
  mapKinds f xs  = map (mapKinds f) xs

instance (HasKinds tc k, HasKinds t k) => HasKinds (Qual tc k t) k where
  mapKinds f (Forall as ps t) =
                      Forall (mapKinds f as) (mapKinds f ps) (mapKinds f t)
--------------------------------------------------------------------------------



--------------------------------------------------------------------------------

data TConApp tc k = TConApp tc [ HMType tc k ]

instance Pretty k => Pretty (TParam k) where
  pPrintPrec (PrettyLevel 0) _ (TParam s _) = text s
  pPrintPrec (PrettyLevel l) _ (TParam s k) =
    parens (text s <+> text "::" <+> pPrintPrec (PrettyLevel (l-1)) 0 k)

instance Pretty k => Pretty (TVar k) where
  pPrintPrec l n (TV _ p) = pPrintPrec l n p

instance (Pretty k, Pretty (TConApp tc k)) => Pretty (HMType tc k) where
  pPrintPrec l n ty =
    case t of
      TVar tvar -> prettyTApp l n (char '?' <> pPrintPrec l 0 tvar) ts
      TGen tvar -> prettyTApp l n (pPrintPrec l 0 tvar) ts
      TCon tcon -> pPrintPrec l n (TConApp tcon ts)
      TApp _ _  -> error "BUG: 'splitTApp' returned an applications"

    where
    (t,ts)    = splitTApp ty

prettyTApp :: Pretty a => PrettyLevel -> Rational -> Doc -> [a] -> Doc
prettyTApp l n f ps = prettyParen (not (null ps) && n >= 9)
                        (f <+> fsep (map (pPrintPrec l 9) ps))

instance (Pretty (TConApp tc k), Pretty k, Pretty t) 
                                          => Pretty (Qual tc k t) where
  pPrintPrec l n (Forall _ [] t)  = pPrintPrec l n t
  pPrintPrec l n (Forall _ ps t)  = prettyParen (n /= 0)
                                     (preds <+> text "=>" <+> pPrintPrec l 0 t)
    where preds = parens $ hsep $ punctuate comma $ map (pPrintPrec l 0) ps
--------------------------------------------------------------------------------



-- Substitutions ---------------------------------------------------------------
class (Monad m, KindOf tc k, Eq tc) => UnificationMonad m k tc
  | m -> k tc where
  kindMismatch  :: k -> k -> m a
  typeMismatch  :: HMType tc k -> HMType tc k -> m a
  recursiveType :: TVar k -> HMType tc k -> m a

newtype Subst tc k      = Su (M.IntMap (HMType tc k))

lookupS :: TVar k -> Subst tc k -> Maybe (HMType tc k)
lookupS (TV x _) (Su m) = M.lookup x m


mgu :: UnificationMonad m k tc
    => HMType tc k -> HMType tc k -> m (Subst tc k)
mgu (TVar x) t = singleS x t
mgu t (TVar x) = singleS x t
mgu (TApp s1 s2) (TApp t1 t2) =
  do su1 <- mgu s1 t1
     su2 <- mgu (apS su1 s2) (apS su1 t2)
     return (compS su2 su1)
mgu (TCon c) (TCon d) | c == d  = return emptyS
mgu t1 t2 = typeMismatch t1 t2


compS :: Subst tc k -> Subst tc k -> Subst tc k
compS s2@(Su su2) (Su su1) = Su (M.union (apS s2 `fmap` su1) su2)

emptyS :: Subst tc k
emptyS = Su M.empty

singleS :: UnificationMonad m k tc => TVar k -> HMType tc k -> m (Subst tc k)
singleS x (TVar y)
  | x == y              = return emptyS

singleS (TV _ k) t
  | k1 /= k2            = kindMismatch k1 k2
    where k1 = kindOf k
          k2 = kindOf t

singleS v t
  | v `S.member` freeTVars t = recursiveType v t

singleS (TV x _) t = return (Su (M.singleton x t))

--------------------------------------------------------------------------------




