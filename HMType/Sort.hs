module HMType.Sort where

import HMType.AST

data Sort = Sort deriving Eq

instance IsKind Sort where
  isKFun _ = Just (Sort, Sort)


