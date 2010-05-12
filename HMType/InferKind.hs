{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module HMType.InferKind where

import HMType.Sort
import HMType.Kind
import HMType.AST
import HMType.Monad.NewVarT
import HMType.Monad.UnifyT

import qualified Data.Map as M
import MonadLib

type Type tc kc   = HMType tc (Kind kc)
type VarMap tc kc = M.Map String (Type tc kc)

newtype KindM tc kc a = KT ( StateT ( Maybe (VarMap tc kc)  -- uni vars
                                    , Maybe (VarMap tc kc)  -- gen vars
                                    )
                           ( NewVarT    -- kind vars
                           ( NewVarT    -- type vars
                           ( UnifyT (KCon kc) Sort Id))) a)
                              deriving (Functor,Monad)

newKind :: KindM tc kc (Kind kc)
newKind = KT $
  do tv <- lift $ newUVar $ TParam "" Sort
     return $ K tv

newTVar :: TParam (Kind kc) -> KindM tc kc (Type tc kc)
newTVar p = KT $ lift $ lift $ newUVar p


unifyK :: (Eq kc) => Kind kc -> Kind kc -> KindM tc kc ()
unifyK (K t1) (K t2)  = KT $ lift $ lift $ lift $ unify t1 t2


checkType :: (Eq kc, KindOf tc (Kind kc))
          => Type tc kc -> KindM tc kc (Type tc kc, Kind kc)
checkType ty =
  case ty of
    TVar tvar   ->
      do (u,g) <- KT get
         case u of
           Nothing -> undefined -- error, free vars not allowed
           Just m ->
             let name = nameOf tvar in
             case M.lookup name m of
                Just t  -> return (t, kindOf t)
                Nothing ->
                  do k  <- newKind
                     t1 <- newTVar (TParam name k)
                     KT $ set (Just (M.insert name t1 m), g)
                     return (t1, k)

    TGen tvar   ->
      do (u,g) <- KT get
         case g of
           Nothing -> undefined -- error, generic vars not allowed

           Just m ->
             let name = nameOf tvar in
             case M.lookup name m of
               Just t  -> return (t, kindOf t)    -- adjust, or lazy?
               Nothing -> undefined -- careful with nested binders
                   -- e.g., methods in a class
                   -- class C a where
                   --   f :: b -> a    -- here b is 0, a is 1
                   --   g :: a         -- here a is 0


      
    TCon tcon   -> return (ty, kindOf tcon) -- or make up a new kind?
    TApp t1 t2  ->
      do (s1,k1) <- checkType t1
         (s2,k2) <- checkType t2
         k  <- newKind
         unifyK k1 (kFun k2 k)
         return (TApp s1 s2, k)


