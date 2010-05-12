{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
module HMType.Kind where

import HMType.Sort
import HMType.AST

import Text.PrettyPrint.HughesPJClass

newtype Kind k    = K (HMType (KCon k) Sort)
                    deriving Eq

data KCon k       = KFun | KPred | KCon k
                    deriving Eq

kFun             :: Kind k -> Kind k -> Kind k
kFun (K a) (K b)  = K (TCon KFun `TApp` a `TApp` b)

kPred            :: Kind k
kPred             = K (TCon KPred)

kCon             :: k -> Kind k
kCon k            = K (TCon (KCon k))

instance Eq k => IsKind (Kind k) where
  isKFun (K (TCon KFun `TApp` a `TApp` b)) = Just (K a, K b)
  isKFun _                                 = Nothing

instance PrettyTCon (KCon k) Sort => Pretty (Kind k) where
  pPrintPrec l n (K k) = pPrintPrec l n k

instance KindOf (KCon k) Sort where
  kindOf _ = Sort

instance KindOf (Kind k) Sort where
  kindOf _ = Sort


