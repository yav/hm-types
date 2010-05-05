{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
module HMType.Kind where

import HMType.Sort
import HMType.AST

import Text.PrettyPrint.HughesPJClass

newtype Kind k    = K (HMType k Sort)
                    deriving Eq

class Eq k => IsKindCon k where
  kFunCon :: k

kFun             :: IsKindCon k => Kind k -> Kind k -> Kind k
kFun (K a) (K b)  = K (TCon kFunCon `TApp` a `TApp` b)

kCon             :: k -> Kind k
kCon k            = K (TCon k)

instance IsKindCon k => IsKind (Kind k) where
  isKFun (K (TCon c `TApp` a `TApp` b)) | c == kFunCon  = Just (K a, K b)
  isKFun _                                              = Nothing

instance PrettyTCon k Sort => Pretty (Kind k) where
  pPrintPrec l n (K k) = pPrintPrec l n k

instance KindOf (Kind k) Sort where
  kindOf _ = Sort


