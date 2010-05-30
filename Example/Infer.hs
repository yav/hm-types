module Example.Infer (infer) where

import Example.Decls
import qualified Example.Infer.Env as Env
import Example.Infer.Env(Env)
import Example.Infer.Monad
import Example.Infer.Error
import Example.Type

import HMType.AST

import MonadLib
import qualified Data.Set as Set
import Data.List
import Data.Maybe

infer :: Decl -> (Env, [Error], [Pred])
infer = runTI . inferDecl


inferDeclMono :: Decl -> TI Env
inferDeclMono decl =
  case decl of

    DLet d1 d2 ->
      do env  <- inferDecl d1
         inExtEnv env $ inferDeclMono d2

    DAnd d1 d2 ->
      do env1 <- inferDeclMono d1
         env2 <- inferDeclMono d2
         mergeEnv env1 env2

    DDef x e ->
      do t <- inferExpr e
         return $ Env.singleton x $ mono t

    DRec d ->
      do env1 <- monoEnv d
         env2 <- inExtEnv env1 $ inferDeclMono d
         zipWithM_ unify (monoEnvTypes env1) (monoEnvTypes env2)
         return env2


inferDecl :: Decl -> TI Env
inferDecl decl =
  case decl of

    DLet d1 d2 ->
      do env  <- inferDecl d1
         inExtEnv env (inferDecl d2)

    DAnd d1 d2 ->
      do env1 <- inferDecl d1
         env2 <- inferDecl d2
         mergeEnv env1 env2

    DDef x e ->
      do (t,ps) <- getPreds $ inferExpr e
         s <- generalize ps t
         return $ Env.singleton x s

    DRec d ->
      do env1 <- monoEnv d
         (env2,ps) <- getPreds $ inExtEnv env1 $ inferDeclMono d
         zipWithM_ unify (monoEnvTypes env1) (monoEnvTypes env2)
         generalizeEnv ps env2

inferExpr :: Expr -> TI Type
inferExpr expr =
  case expr of

    EApp e1 e2 ->
      do t1 <- inferExpr e1
         t2 <- inferExpr e2
         a  <- newTVar kStar
         unify t1 (tFun t2 a)
         return a

    EFun x e ->
      do a <- newTVar kStar
         t <- inExtEnv (Env.singleton x (mono a)) (inferExpr e)
         return (tFun a t)

    EVar x ->
      do s <- lookupVar x
         instantiate s

    ELet d e ->
      do env <- inferDecl d
         inExtEnv env (inferExpr e)


--------------------------------------------------------------------------------


monoEnv :: Decl -> TI Env
monoEnv d = liftM Env.fromList
          $ forM (Set.toList (defs d))
          $ \x -> do t <- newTVar kStar
                     return (x, mono t)

fromMonoSchema :: Schema -> Type
fromMonoSchema (Forall _ _ t) = t

monoEnvTypes :: Env -> [Type]
monoEnvTypes env = map (fromMonoSchema . snd) $ Env.toList env


defs :: Decl -> Set.Set Name
defs decl = case decl of
              DAnd d1 d2  -> Set.union (defs d1) (defs d2)
              DLet _ d    -> defs d
              DRec d      -> defs d
              DDef x _    -> Set.singleton x

generalizeEnv :: [Pred] -> Env -> TI Env
generalizeEnv ps env =
  do let (xs,ss) = unzip $ Env.toList env
     Forall as ps1 ts1 <- generalize ps (map fromMonoSchema ss)
     let toS x t = (x, Forall as ps1 t)
     return $ Env.fromList $ zipWith toS xs ts1


generalize :: HasTVars t => [Pred] -> t -> TI (Qual t)
generalize ps t =
  do env <- getEnv
     let envVars    = freeTVars env
         genVars    = freeTVars t `Set.difference` envVars
         isExtern p = Set.null (freeTVars p `Set.intersection` genVars)
         (externalPreds, localPreds) = partition isExtern ps

         as   = Set.toList genVars

         -- signature needed due to the monomorhism restriction
         apSu :: HasTVars t => t -> t
         apSu  = apTVars $ \x@(TR _ p) -> do n <- elemIndex x as
                                             return $ TAtom TGen $ TR n p

     addPreds externalPreds
     return $ Forall [ a | TR _ a <- as ] (apSu localPreds) (apSu t)





mergeEnv :: Env -> Env -> TI Env
mergeEnv e1 e2 =
  do let (e3, redef) = Env.union e1 e2
     unless (Set.null redef) $ addErrs [ MultipleDefinitions redef ]
     return e3

-- By this point, all as should have kinds.
instantiate :: Schema -> TI Type
instantiate (Forall as ps t) =
  do ts <- mapM newTVar $ mapMaybe kindOf as
     addPreds $ map (apGVars ts) ps
     return $ apGVars ts t

lookupVar :: Name -> TI Schema
lookupVar x =
  do env <- getEnv
     case Env.lookup x env of
       Just s   -> return s
       -- In case of error, we assume some monomorphic type.
       -- Note that this results in a new error for each separate
       -- use of an undefined variable.  Perhaps, it is better to
       -- report only the first one?
       Nothing  ->
        do t <- newTVar kStar
           addErrs [ UndefinedVariable x t ]
           return $ mono t



