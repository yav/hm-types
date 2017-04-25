{-# LANGUAGE MultiParamTypeClasses #-}
module Example.Type where

import qualified HM.Type.AST as HM
import HM.Type.AST hiding (Schema,Type,Kind,Pred)
import HM.Type.Pretty
import qualified Data.Set as Set

type Schema   = HM.Schema TCon
type Type     = HM.Type TCon
type PatType  = HM.Qual TCon PatT
type Pred     = HM.Pred TCon
type Kind     = HM.Kind TCon

data PatFails = MayFail | MayNotFail
data PatT     = PatT Type [Type] PatFails

instance HasTVars PatT TCon where
  apTVars su (PatT t ts f) = PatT (apTVars su t) (apTVars su ts) f
  freeTVars (PatT t ts _)  = Set.union (freeTVars t) (freeTVars ts)

instance HasGVars PatT TCon where
  apGVars su (PatT t ts f) = PatT (apGVars su t) (apGVars su ts) f


data TCon     = KStar | KFun | KNat
              | TFun | TNat Integer
                deriving (Eq,Show)

instance KindOf TCon TCon where
  kindOf tcon =
    case tcon of
      KStar   -> Nothing
      KFun    -> Nothing
      KNat    -> Nothing
      TFun    -> Just $ kFun kStar $ kFun kStar kStar
      TNat n  -> if n >= 0 then Just kNat else Nothing

instance IsTCon TCon

instance PrettyCon TCon where
  ppCon _ r tcon args =
    case tcon of
      KStar   -> ppDoc (char '*')
      KFun    -> ppFun
      KNat    -> ppDoc (text "nat")
      TFun    -> ppFun
      TNat n  -> ppDoc (integer n)
    where ppFun   = case args of
                      [t1,t2] -> wrap 5 r (t1 6 <+> text "->" <+> t2 5)
                      _       -> ppTApp r (text "->") args
          ppDoc d = ppTApp r d args
      

kStar        :: Kind
kStar         = TCon KStar

kNat         :: Kind
kNat          = TCon KNat

kFun         :: Kind -> Kind -> Kind
kFun k1 k2    = TCon KFun `TApp` k1 `TApp` k2

tFun         :: Type -> Type -> Type
tFun t1 t2    = TCon TFun `TApp` t1 `TApp` t2

mono         :: Type -> Schema
mono          = Forall [] []

