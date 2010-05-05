{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
module HMType.Example.Bits where

import HMType.AST
import HMType.Sort
import HMType.Kind
import Text.PrettyPrint.HughesPJClass

-- Kinds -----------------------------------------------------------------------
data KCon         = KFun            -- The kind of type constructors.
                  | KStar           -- The kind of ordinary value types.
                  | KNum            -- The kind of numeric types.
                  | KPred           -- The kind of predicates
                    deriving Eq

type BKind        = Kind KCon

kStar            :: BKind
kStar             = K (TCon KStar)

kNum             :: BKind
kNum              = K (TCon KNum)

kPred            :: BKind
kPred             = K (TCon KPred)

instance IsKindCon KCon where
  kFunCon = KFun

instance PrettyTCon KCon Sort where

  pPrintTCon l n KFun [t1,t2] = prettyParen (n > 5) $
    pPrintPrec l 6 t1 <+> text "->" <+> pPrintPrec l 5 t2

  pPrintTCon l n t ts = prettyTApp l n (text short) ts 
    where short = case t of
                    KFun  -> "(->)"
                    KStar -> "*"
                    KNum  -> "nat"
                    KPred -> "prop"

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

newtype Type      = T (HMType TCon BKind)

tFun             :: Type -> Type -> Type
tFun (T a) (T b)  = T (TCon TFun `TApp` a `TApp` b)

tSeq             :: Type -> Type -> Type
tSeq (T a) (T b)  = T (TCon TSeq `TApp` a `TApp` b)

tBit             :: Type
tBit              = T (TCon TBit)

tNum             :: Integer -> Type
tNum n            = T (TCon (TNum n))


instance KindOf TCon BKind where
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



instance KindOf Type BKind where
  kindOf (T t)  = kindOf t

instance HasKinds TCon BKind where
  mapKinds _ k = k

instance HasKinds Type BKind where
  mapKinds f (T t)  = T (mapKinds f t)



instance PrettyTCon TCon BKind where

  pPrintTCon l n TFun [t1,t2] = prettyParen (n > 5)
    $ pPrintPrec l 6 t1 <+> text "->" <+> pPrintPrec l 5 t2

  pPrintTCon l n TSeq [t1,t2] = prettyParen (n > 6)
    $ brackets (pPrintPrec l 0 t1) <> pPrintPrec l 6 t2

  pPrintTCon _ _ TBit [] = text "Bit"

  pPrintTCon l n (TNum x) [] = pPrintPrec l n x

  -- XXX: rules to print arithmetic nicely

  pPrintTCon l n t ts = prettyTApp l n (text short) ts
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

