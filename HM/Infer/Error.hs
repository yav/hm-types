{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
module HM.Infer.Error where

import HM.Type.AST
import HM.Type.Subst

import qualified Data.Set as Set


data Error c n  = UndefinedVariable n (Type c)
                | UnificationError (MguError c)
                | MultipleDefinitions (Set.Set n)
                  deriving Show

instance HasTVars (Error c n) c where
  apTVars f err =
    case err of
      UndefinedVariable x t -> UndefinedVariable x (apTVars f t)
      UnificationError e    -> UnificationError (apTVars f e)
      MultipleDefinitions x -> MultipleDefinitions x

  freeTVars err =
    case err of
      UndefinedVariable _ t -> freeTVars t
      UnificationError e    -> freeTVars e
      MultipleDefinitions _ -> Set.empty


