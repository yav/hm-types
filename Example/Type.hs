{-# LANGUAGE MultiParamTypeClasses #-}
module Example.Type where

import qualified HM.Type.AST as HM
import HM.Type.AST hiding (Schema,Type,Kind,Pred)

type Schema   = HM.Schema TCon
type Type     = HM.Type TCon
type Pred     = HM.Pred TCon
type Kind     = HM.Kind TCon


data TCon     = KStar | KFun
              | TFun
                deriving (Eq,Show)

instance KindOf TCon TCon where
  kindOf KStar  = Nothing
  kindOf KFun   = Nothing
  kindOf TFun   = Just $ kFun kStar $ kFun kStar kStar

instance IsTCon TCon

kStar        :: Kind
kStar         = TCon KStar

kFun         :: Kind -> Kind -> Kind
kFun k1 k2    = TCon KFun `TApp` k1 `TApp` k2

tFun         :: Type -> Type -> Type
tFun t1 t2    = TCon TFun `TApp` t1 `TApp` t2

mono         :: Type -> Schema
mono          = Forall [] []

