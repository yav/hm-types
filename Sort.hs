module Sort where

import AST

data Sort = Sort deriving Eq

instance IsKind Sort where
  isKFun _ = Just (Sort, Sort)


