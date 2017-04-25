{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Example.Monad where

import Example.AST(Name)
import Example.Type
import qualified HM.Infer.Env   as HM
import qualified HM.Infer.Monad as HM
import qualified HM.Infer.Error as HM
import qualified HM.Type.AST as HM
import qualified Data.Map as Map
import qualified Data.Set as Set
import MonadLib


type Env    = HM.Env TCon Name
data Err    = PatErr Name Type PatErr
data PatErr = PatUndefined    [Type]
            | PatTooManyArgs  [Type]
            | PatTooFewArgs   [Type]
 

instance HM.HasTVars PatErr TCon where
  apTVars f r =
    case r of
      PatUndefined ts   -> PatUndefined   $ HM.apTVars f ts
      PatTooManyArgs ts -> PatTooManyArgs $ HM.apTVars f ts
      PatTooFewArgs ts  -> PatTooFewArgs $ HM.apTVars f ts

  freeTVars r =
    case r of
      PatUndefined ts   -> HM.freeTVars ts
      PatTooManyArgs ts -> HM.freeTVars ts
      PatTooFewArgs ts  -> HM.freeTVars ts


instance HM.HasTVars Err TCon where
  apTVars f (PatErr n t e)  = PatErr n (HM.apTVars f t) (HM.apTVars f e)
  freeTVars (PatErr _ t e)  = Set.union (HM.freeTVars t) (HM.freeTVars e)

type Error  = HM.Error TCon Name Err

type Infer  = ReaderT (Map.Map Name PatType) HMM
type HMM    = HM.TI TCon Name Err



runTI :: HM.HasTVars a TCon => Infer a -> (a, [Error], [Pred])
runTI m = HM.runTI $ runReaderT Map.empty m

liftArg :: (HMM a -> HMM b) -> Infer a -> Infer b
liftArg f m =
  do e <- ask
     lift $ f $ runReaderT e m

lift1          :: (a -> HMM b) -> a -> Infer b
lift1 f x       = lift $ f x

lift2          :: (a -> b -> HMM c) -> a -> b -> Infer c
lift2 f x y     = lift $ f x y

inExtEnv       :: Env -> Infer a -> Infer a
inExtEnv        = liftArg . HM.inExtEnv

getPreds       :: Infer a -> Infer (a, [Pred])
getPreds        = liftArg HM.getPreds

unify          :: Type -> Type -> Infer ()
unify           = lift2 HM.unify

getEnv         :: Infer Env
getEnv          = lift HM.getEnv

mergeEnv       :: [Env] -> Infer Env
mergeEnv        = lift1 HM.mergeEnv

addErrs        :: [Error] -> Infer ()
addErrs         = lift1 HM.addErrs

newTVar        :: Kind -> Infer Type
newTVar         = lift1 HM.newTVar

instantiate    :: HM.HasGVars t TCon => HM.Qual TCon t -> Infer t
instantiate     = lift1 HM.instantiate

generalize     :: HM.HasTVars t TCon => [Pred] -> t -> Infer (HM.Qual TCon t)
generalize      = lift2 HM.generalize

lookupPat      :: Name -> [Type] -> Infer Type
lookupPat c ts  =
  do pats <- ask
     case Map.lookup c pats of
       Just s  ->
         do PatT t ts1 f <- instantiate s
            res <- unifyMany ts ts1
            case res of
              Nothing -> return ()
              Just (Left extra) ->
                addErrs [ HM.OtherError $ PatErr c t $ PatTooManyArgs extra ]
              Just (Right missing) ->
                addErrs [ HM.OtherError $ PatErr c t $ PatTooFewArgs missing ]
            return t 

       Nothing ->
         do t <- newTVar kStar
            addErrs [ HM.OtherError $ PatErr c t $ PatUndefined ts ]
            return t

unifyMany :: [Type] -> [Type] -> Infer (Maybe (Either [Type] [Type]))
unifyMany [] []             = return Nothing
unifyMany (t:ts) (t1 : ts1) = unify t t1 >> unifyMany ts ts1
unifyMany ts []             = return (Just (Left ts))
unifyMany [] ts             = return (Just (Right ts))




