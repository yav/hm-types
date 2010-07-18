{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
module HM.Infer.Error
  ( module HM.Infer.Error
  , MguError(..)
  , MguErrorType(..)
  ) where

import HM.Type.AST
import HM.Type.Subst

import qualified Data.Set as Set


data Error c n err  = UndefinedVariable n (Type c)
                    | UnificationError (MguError c)
                    | MultipleDefinitions (Set.Set n)
                    | OtherError err
                      deriving Show

instance HasTVars err c => HasTVars (Error c n err) c where
  apTVars f err =
    case err of
      UndefinedVariable x t -> UndefinedVariable x (apTVars f t)
      UnificationError e    -> UnificationError (apTVars f e)
      MultipleDefinitions x -> MultipleDefinitions x
      OtherError e          -> OtherError (apTVars f e)

  freeTVars err =
    case err of
      UndefinedVariable _ t -> freeTVars t
      UnificationError e    -> freeTVars e
      MultipleDefinitions _ -> Set.empty
      OtherError e          -> freeTVars e


