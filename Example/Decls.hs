module Example.Decls where

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

