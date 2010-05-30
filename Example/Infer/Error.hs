module Example.Infer.Error where

import Example.Decls (Name)

import HMType.AST
import HMType.Subst

import qualified Data.Set as Set


data Error    = UndefinedVariable Name Type
              | UnificationError MguError
              | MultipleDefinitions (Set.Set Name)
                deriving Show

instance HasTVars Error where
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


