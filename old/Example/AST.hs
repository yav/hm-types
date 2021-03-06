module Example.AST where

import qualified Data.Set as Set

data Name       = Name String
                  deriving (Show,Eq,Ord)

data Decl       = DLet Decl Decl
                | DAnd [Decl]
                | DRec Decl
                | DDef Name Expr
                  deriving Show

data Expr       = EApp Expr Expr
                | EFun Name Expr
                | EVar Name
                | ELet Decl Expr
                | ECase Mat Expr
                  deriving Show

data Pat        = PVar Name
                | PWild
                | PCon Name [Pat]
                  deriving Show

data Grd        = GPat Pat Expr
                | GLet Decl
                  deriving Show

data Mat        = MIs Expr
                | MGrd Grd Mat
                | MOr Mat Mat
                  deriving Show
    


defs :: Decl -> Set.Set Name
defs decl = case decl of
              DAnd ds   -> Set.unions (map defs ds)
              DLet _ d  -> defs d
              DRec d    -> defs d
              DDef x _  -> Set.singleton x


splitFun :: Expr -> ([Name],Expr)
splitFun (EFun x e) = (x:xs,e1)
  where (xs,e1) = splitFun e
splitFun e = ([], e)


splitApp :: Expr -> (Expr, [Expr])
splitApp e = loop e []
  where loop (EApp e1 e2) es  = loop e1 (e2 : es)
        loop e1 es            = (e1,es)

