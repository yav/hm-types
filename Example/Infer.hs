module Example.Infer (infer) where

import Example.AST
import Example.Type
import qualified HM.Infer.Env as Env
import HM.Infer.Monad
import HM.Infer.Error

import HM.Type.AST

import qualified Data.Set as Set
import Control.Monad (zipWithM_, liftM, forM)


infer :: Decl -> (Env, [Error Name], [Pred])
infer = runTI . inferDecl


type Env    = Env.Env Name
type Infer  = TI Name

inferDeclMono :: Decl -> Infer Env
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


inferDecl :: Decl -> Infer Env
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

inferExpr :: Expr -> Infer Type
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


monoEnv :: Decl -> Infer Env
monoEnv d = liftM Env.fromList
          $ forM (Set.toList (defs d))
          $ \x -> do t <- newTVar kStar
                     return (x, mono t)

fromMonoSchema :: Schema -> Type
fromMonoSchema (Forall _ _ t) = t

monoEnvTypes :: Env -> [Type]
monoEnvTypes env = map (fromMonoSchema . snd) $ Env.toList env

generalizeEnv :: [Pred] -> Env -> Infer Env
generalizeEnv ps env =
  do let (xs,ss) = unzip $ Env.toList env
     Forall as ps1 ts1 <- generalize ps (map fromMonoSchema ss)
     let toS x t = (x, Forall as ps1 t)
     return $ Env.fromList $ zipWith toS xs ts1


lookupVar :: Ord n => n -> TI n Schema
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


