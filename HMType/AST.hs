{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
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
  , HasUVars(..)
  , HasGVars(..)

  -- * Pretty printing
  , PrettyTCon(..)
  , prettyTApp
  ) where

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
  apUni     :: (TVar k -> Maybe (HMType tc k)) -> t -> t
  freeTVars :: t -> S.Set (TVar k)

instance HasUVars (HMType tc k) tc k where
  apUni su ty =
    case ty of
      TVar tvar ->
        case su tvar of
          Just t  -> t
          Nothing -> ty
      TApp t1 t2  -> TApp (apUni su t1) (apUni su t2)
      TGen _      -> ty
      TCon _      -> ty

  freeTVars ty =
    case ty of
      TVar tvar   -> S.singleton tvar
      TApp t1 t2  -> S.union (freeTVars t1) (freeTVars t2)
      TGen _      -> S.empty
      TCon _      -> S.empty


instance HasUVars t tc k => HasUVars [t] tc k where
  apUni su xs     = map (apUni su) xs
  freeTVars xs  = S.unions (map freeTVars xs)

instance HasUVars t tc k => HasUVars (Qual tc k t) tc k where
  apUni su (Forall as ps t)   = Forall as (apUni su ps) (apUni su t)
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

-- | Apply a function to all the kinds in the given entity.
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

class HasTCons t tcon | t -> tcon where
  mapTCons :: (tcon -> tcon) -> t -> t

instance HasTCons (HMType tcon kind) tcon where
  mapTCons f ty =
    case ty of
      TApp t1 t2  -> TApp (mapTCons f t1) (mapTCons f t2)
      TCon tcon   -> TCon (f tcon)
      TVar _      -> ty
      TGen _      -> ty

instance HasTCons t tcon => HasTCons [t] tcon where
  mapTCons f xs  = map (mapTCons f) xs

instance HasTCons t tcon => HasTCons (Qual tcon kind t) tcon where
  mapTCons f (Forall as ps t) =
                      Forall as (mapTCons f ps) (mapTCons f t)

--------------------------------------------------------------------------------

class Pretty kind => PrettyTCon tcon kind | tcon -> kind where
  pPrintTCon :: PrettyLevel -> Rational -> tcon -> [HMType tcon kind] -> Doc

instance Pretty k => Pretty (TParam k) where
  pPrintPrec (PrettyLevel 0) _ (TParam s _) = text s
  pPrintPrec (PrettyLevel l) _ (TParam s k) =
    parens (text s <+> text "::" <+> pPrintPrec (PrettyLevel (l-1)) 0 k)

instance Pretty k => Pretty (TVar k) where
  pPrintPrec l n (TV _ p) = pPrintPrec l n p

instance PrettyTCon tcon kind => Pretty (HMType tcon kind) where
  pPrintPrec l n ty =
    case t of
      TVar tvar -> prettyTApp l n (char '?' <> pPrintPrec l 0 tvar) ts
      TGen tvar -> prettyTApp l n (pPrintPrec l 0 tvar) ts
      TCon tcon -> pPrintTCon l n tcon ts
      TApp _ _  -> error "BUG: 'splitTApp' returned an applications"

    where
    (t,ts)    = splitTApp ty

prettyTApp :: Pretty a => PrettyLevel -> Rational -> Doc -> [a] -> Doc
prettyTApp l n f ps = prettyParen (not (null ps) && n >= 9)
                        (f <+> fsep (map (pPrintPrec l 9) ps))

instance (PrettyTCon tcon kind, Pretty t) => Pretty (Qual tcon kind t) where
  pPrintPrec l n (Forall _ [] t)  = pPrintPrec l n t
  pPrintPrec l n (Forall _ ps t)  = prettyParen (n /= 0)
                                     (preds <+> text "=>" <+> pPrintPrec l 0 t)
    where preds = parens $ hsep $ punctuate comma $ map (pPrintPrec l 0) ps
--------------------------------------------------------------------------------





