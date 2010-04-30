module HMType.Sort where

import HMType.AST
import Text.PrettyPrint.HughesPJClass

data Sort = Sort deriving Eq

instance IsKind Sort where
  isKFun _ = Just (Sort, Sort)

instance Pretty Sort where
  pPrint _ = text "*1"


