{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DoRec #-}
module HMType.InferKind where

import HMType.Sort
import HMType.Kind
import HMType.AST
import HMType.Monad.NewVarT
import HMType.Monad.UnifyT

import Data.List (sortBy)
import qualified Data.Map as M
import MonadLib
import Control.Monad.Fix

type Type tc kc     = HMType tc (Kind kc)
type VarMap tc kc   = M.Map String (Type tc kc)

newtype KindM tc kc a = KT ( ReaderT (VarMap tc kc)         -- outer qs
                           ( StateT ( Maybe (VarMap tc kc)  -- uni vars
                                    , Maybe (VarMap tc kc)  -- gen vars
                                    )
                           ( NewVarT    -- kind vars
                           ( NewVarT    -- type vars
                           ( UnifyT (KCon kc) Sort Id)))) a)
                              deriving (Functor,Monad,MonadFix)

newKind :: KindM tc kc (Kind kc)
newKind = KT $
  do tv <- lift $ lift $ newUVar $ TParam "" Sort
     return $ K tv

newTVar :: TParam (Kind kc) -> KindM tc kc (Type tc kc)
newTVar p = KT $ lift $ lift $ lift $ newUVar p


unifyK :: (Eq kc) => Kind kc -> Kind kc -> KindM tc kc ()
unifyK (K t1) (K t2)  = KT $ lift $ lift $ lift $ lift $ unify t1 t2

newScopeQ :: VarMap tc kc -> KindM tc kc a -> KindM tc kc a
newScopeQ o (KT m) = KT (local o m)


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
               Just t  -> return (t, kindOf t)
               Nothing ->
                 do outer <- KT ask
                    case M.lookup name outer of
                      Just t  -> return (t, kindOf t)
                      Nothing ->
                        do k <- newKind
                           let t1 = TVar (TV (M.size m) (TParam name k))
                           KT $ set (u, Just (M.insert name t1 m))
                           return (t1, k)
      
    TCon tcon   -> return (ty, kindOf tcon) -- or make up a new kind?
    TApp t1 t2  ->
      do (s1,k1) <- checkType t1
         (s2,k2) <- checkType t2
         k  <- newKind
         unifyK k1 (kFun k2 k)
         return (TApp s1 s2, k)


checkPred :: (Eq kc, KindOf tc (Kind kc))
          => Type tc kc -> KindM tc kc (Type tc kc)
checkPred p =
  do (p1,k) <- checkType p
     unifyK k kPred
     return p1


checkQual :: (Eq kc, KindOf tc (Kind kc))
          => (t -> KindM tc kc t)
          -> Qual tc (Kind kc) t
          -> KindM tc kc (Qual tc (Kind kc) t)  
  
checkQual check (Forall _ ps t) =
  do (u,g) <- KT get
     outer <- KT ask

     rec 
       let qnum = M.size g1   -- See bellow
           up (TVar (TV n p)) = TVar (TV (n + qnum) p)
           up t2              = t2  -- Should not happend
           outer1 | qnum == 0 = outer
                  | otherwise = M.map up outer

       KT $ set (u, Just M.empty)
       (ps1,t1) <- newScopeQ outer1 $ liftM2 (,) (mapM checkPred ps) (check t)
  
       (u1,Just g1) <- KT get    -- 'g1' is defined here.

     KT $ set (u1,g)

     let compareIx (TV x _) (TV y _) = compare x y
         as = [ a | TV _ a <- sortBy compareIx [ tv | TVar tv <- M.elems g1 ] ]

     return (Forall as ps1 t1)


