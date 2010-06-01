module Example.AST where

import qualified Data.Set as Set

data Name       = Name String
                  deriving (Show,Eq,Ord)

data Decl       = DLet Decl Decl
                | DAnd Decl Decl
                | DRec Decl
                | DDef Name Expr
                  deriving Show

data Expr       = EApp Expr Expr
                | EFun Name Expr
                | EVar Name
                | ELet Decl Expr
                  deriving Show

defs :: Decl -> Set.Set Name
defs decl = case decl of
              DAnd d1 d2  -> Set.union (defs d1) (defs d2)
              DLet _ d    -> defs d
              DRec d      -> defs d
              DDef x _    -> Set.singleton x


