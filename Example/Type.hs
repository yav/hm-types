module Example.Type where

import HM.Type.AST

type Schema   = Qual Type

kStar        :: Kind
kStar         = TAtom TCon $ TR 0 $ TParam "*"  $ Nothing

kcFun        :: Kind
kcFun         = TAtom TCon $ TR 1 $ TParam "->" $ Nothing

kFun         :: Kind -> Kind -> Kind
kFun k1 k2    = kcFun `TApp` k1 `TApp` k2


tcFun        :: Type
tcFun         = TAtom TCon $ TR 0 $ TParam "->"
                           $ Just $ kFun kStar $ kFun kStar kStar

tFun         :: Type -> Type -> Type
tFun t1 t2    = tcFun `TApp` t1 `TApp` t2


mono         :: Type -> Qual Type
mono          = Forall [] []

