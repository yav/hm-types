{-# LANGUAGE FlexibleInstances #-}
module Example.Prelude where

import Example.AST

define x  = DDef (Name x)
unused :: a -> Expr -> a
unused a _ = a



fun' x f = EFun (Name x) $ f $ EVar $ Name x

class Fun t where
  fun :: (t -> Expr) -> Expr

instance Fun Expr where
  fun = fun' "x"

instance Fun (Expr,Expr) where
  fun f = fun' "x" $ \x -> fun' "y" $ \y -> f (x,y)

class App t where
  app :: t -> Expr


prelude =
  DLet
    (DAnd [
      define "id"    $ fun $ \x -> x,
      define "const" $ fun $ \(x,y) -> x `unused` y
    ])
    $ DAnd [
        define "test"  $ EApp (EVar (Name "const")) (EVar (Name "id")),
        define "id"    $ EVar (Name "id")
    ]
  
