import Example.AST
import Example.Infer
import qualified HM.Infer.Env as Env (Env, toList)

import HM.Type.Pretty
import Text.PrettyPrint

main :: IO ()
main = test ex1

test :: Decl -> IO ()
test d = do putStrLn "Env:"
            print (ppEnv env)
            putStrLn "Preds:"
            print $ vcat $ map ppType preds
            putStrLn "Errors:"
            putStrLn $ unlines $ map show errs
  where (env,errs,preds) = infer d


ex1 :: Decl
ex1 = DRec $ DDef x $ EFun x (EVar x) `EApp` EVar x
  where x = Name "x"

ppEnv :: Env.Env Name -> Doc
ppEnv m = vcat $ map ppSig $ Env.toList m
  where ppSig (Name x,s) = text x <+> text "::" <+> ppQual ppType s



