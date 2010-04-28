{-# LANGUAGE MultiParamTypeClasses #-}
module Example.Bits where

import AST
import Sort

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
--------------------------------------------------------------------------------


-- Type constructors -----------------------------------------------------------
data TCon         = TFun            -- ^ The type of funciton values
                  | TSeq            -- ^ The type of sized sequences
                  | TBit            -- ^ The type of bits
                  | TNum Integer    -- ^ A numeric types (uninhabited)
                  | TOne            -- ^ A singleton type

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

tOne             :: Type -> Type
tOne (T a)        = T (TCon TOne `TApp` a)


instance KindOf TCon Kind where
  kindOf tcon =
    case tcon of
      TFun        -> kStar `kFun` (kStar `kFun` kStar)
      TSeq        -> kNum  `kFun` (kStar `kFun` kStar)
      TBit        -> kStar
      TNum _      -> kNum
      TOne        -> kNum `kFun` kStar

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



instance Linearize TCon where
  ppTCon n TFun [t1,t2] = wrapUnless (n <= 5)
                        $ ppPrec 6 t1 . showString " -> " . ppPrec 5 t2
  ppTCon n TSeq [t1,t2] = wrapUnless (n <= 6)
                        $ showChar '[' . pp t1 . showChar ']' . ppPrec 6 t2
  ppTCon _ TBit []      = showString "Bit"
  ppTCon _ (TNum n) []  = shows n
  ppTCon _ TOne [t]     = showChar '`' . pp t

  -- XXX: rules to print arithmetic nicely

  ppTCon n t ts         = ppTApp n (showString short) ts
    where short = case t of
                    TFun   -> "(->)"
                    TSeq   -> "[]"
                    TBit   -> "Bit"
                    TNum i -> show i
                    TOne   -> "(`)"

                    PAdd   -> "(+)"
                    PMul   -> "(*)"
                    PExp   -> "(**)"
                    PLeq   -> "(<=)"

instance PP Type where
  ppPrec n (T t)  = ppPrec n t

