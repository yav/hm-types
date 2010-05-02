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
  , Subst
  , lookupS
  , compS
  , mergeS
  , emptyS
  , singleS
  , mgu
  , match

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



--------------------------------------------------------------------------------

-- | An application of a constructor to some types.
-- Used for pretty-printing.
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

-- | What may go wrong when we try to bind a type variable to a type.
data TVarBindError  = KindMismatch | RecursiveType

-- | What may go wrong while computing the most general unifier of two types.
data MguError tc k  = TVarBindError TVarBindError (TVar k) (HMType tc k)
                    | TypeMismatch (HMType tc k) (HMType tc k)

-- | A substitution associating type variables with terms.
newtype Subst tc k      = Su (M.IntMap (HMType tc k))

-- | Find the binding for a unfication variable, if any.
lookupS :: TVar k -> Subst tc k -> Maybe (HMType tc k)
lookupS (TV x _) (Su m) = M.lookup x m

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
-- Unification variables in the pattern are treated as constants:
--   * they are equal only to themselves,
--   * they cannot be bound, and
--   * they are considered to be distinct from the variables in the pattern.
-- Examples:
-- "x"      "List y"            ---> Just { x = List y }
-- "List x" "y"                 ---> Nothing
-- "x"      "List x"            ---> Just { x = List x }
-- "(x,x)"  "(List a, List a)"  ---> Just { x = List a }
-- "(x,x)"  "(List a, List b)"  ---> Nothing
match :: (KindOf tc k, Eq tc)
      => HMType tc k -> HMType tc k
      -> Maybe (Subst tc k)
match (TVar x) t
  | kindOf x /= kindOf t  = Nothing
match (TVar (TV x _)) t   = Just (Su (M.singleton x t))
match (TApp s1 s2) (TApp t1 t2) =
  do su1 <- match s1 t1
     su2 <- match s2 t2
     mergeS su2 su1
match (TCon c) (TCon d) | c == d  = return emptyS
match _ _                         = Nothing




compS :: Subst tc k -> Subst tc k -> Subst tc k
compS s2@(Su su2) (Su su1) = Su (M.union (apS s2 `fmap` su1) su2)

mergeS :: Eq tc => Subst tc k -> Subst tc k -> Maybe (Subst tc k)
mergeS (Su su1) (Su su2)
  | M.fold (&&) True (M.intersectionWith (==) su1 su2) =
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
singleS (TV x _) t                            = Right (Su (M.singleton x t))

--------------------------------------------------------------------------------




