{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
module HMType.Example.Bits where

import HMType.AST
import HMType.Sort
import Text.PrettyPrint.HughesPJClass

-- Kinds -----------------------------------------------------------------------
data KCon         = KFun            -- The kind of type constructors.
                  | KStar           -- The kind of ordinary value types.
                  | KNum            -- The kind of numeric types.
                  | KPred           -- The kind of predicates
                    deriving Eq

newtype Kind      = K (HMType KCon Sort)
                    deriving Eq

kFun             :: Kind -> Kind -> Kind
kFun (K a) (K b)  = K (TCon KFun `TApp` a `TApp` b)

kStar            :: Kind
kStar             = K (TCon KStar)

kNum             :: Kind
kNum              = K (TCon KNum)

kPred            :: Kind
kPred             = K (TCon KPred)

instance IsKind Kind where
  isKFun (K (TCon KFun `TApp` a `TApp` b))  = Just (K a, K b)
  isKFun _                                  = Nothing

instance Pretty (TConApp KCon Sort) where

  pPrintPrec l n (TConApp KFun [t1,t2]) = prettyParen (n > 5) $
    pPrintPrec l 6 t1 <+> text "->" <+> pPrintPrec l 5 t2

  pPrintPrec l n (TConApp t ts) = prettyTApp l n (text short) ts 
    where short = case t of
                    KFun  -> "(->)"
                    KStar -> "*"
                    KNum  -> "nat"
                    KPred -> "prop"

instance Pretty Kind where
  pPrintPrec l n (K k) = pPrintPrec l n k

--------------------------------------------------------------------------------


-- Type constructors -----------------------------------------------------------
data TCon         = TFun            -- ^ The type of funciton values
                  | TSeq            -- ^ The type of sized sequences
                  | TBit            -- ^ The type of bits
                  | TNum Integer    -- ^ A numeric types (uninhabited)

                  | PAdd
                  | PMul
                  | PExp
                  | PLeq

-- x - y = z    <==>  (z + y = x)
-- x / y = a b  <==>  (a * y = ay, ay + b = x, b + 1 <= y)
-- log b r = p  <==>  (b ** p = r)

-- exact div: x / y = a 0  --> (a * y = x, 1 <= y)

newtype Type      = T (HMType TCon Kind)

tFun             :: Type -> Type -> Type
tFun (T a) (T b)  = T (TCon TFun `TApp` a `TApp` b)

tSeq             :: Type -> Type -> Type
tSeq (T a) (T b)  = T (TCon TSeq `TApp` a `TApp` b)

tBit             :: Type
tBit              = T (TCon TBit)

tNum             :: Integer -> Type
tNum n            = T (TCon (TNum n))


instance KindOf TCon Kind where
  kindOf tcon =
    case tcon of
      TFun        -> kStar `kFun` (kStar `kFun` kStar)
      TSeq        -> kNum  `kFun` (kStar `kFun` kStar)
      TBit        -> kStar
      TNum _      -> kNum

      PAdd        -> three
      PMul        -> three
      PExp        -> three
      PLeq        -> two
    where three = kNum `kFun` two
          two   = kNum `kFun` (kNum `kFun` kPred)



instance KindOf Type Kind where
  kindOf (T t)  = kindOf t

instance HasKinds TCon Kind where
  mapKinds _ k = k

instance HasKinds Type Kind where
  mapKinds f (T t)  = T (mapKinds f t)



instance Pretty (TConApp TCon Kind) where

  pPrintPrec l n (TConApp TFun [t1,t2]) = prettyParen (n > 5)
    $ pPrintPrec l 6 t1 <+> text "->" <+> pPrintPrec l 5 t2

  pPrintPrec l n (TConApp TSeq [t1,t2]) = prettyParen (n > 6)
    $ brackets (pPrintPrec l 0 t1) <> pPrintPrec l 6 t2

  pPrintPrec _ _ (TConApp TBit []) = text "Bit"

  pPrintPrec l n (TConApp (TNum x) []) = pPrintPrec l n x

  -- XXX: rules to print arithmetic nicely

  pPrintPrec l n (TConApp t ts) = prettyTApp l n (text short) ts
    where short = case t of
                    TFun   -> "(->)"
                    TSeq   -> "[]"
                    TBit   -> "Bit"
                    TNum i -> show i

                    PAdd   -> "(+)"
                    PMul   -> "(*)"
                    PExp   -> "(**)"
                    PLeq   -> "(<=)"

instance Pretty Type where
  pPrintPrec l n (T t)  = pPrintPrec l n t

