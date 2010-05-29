import HMType.AST
import HMType.Pretty
import Example.Decls
import Text.Show.Pretty
import Text.PrettyPrint

main = ex1

test d = do putStrLn "Env:"
            print (ppEnv env)
            putStrLn "Preds:"
            print $ vcat $ map ppType preds
            putStrLn "Errors:"
            putStrLn $ unlines $ map show errs
  where (env,errs,preds) = runTI (inferDecl d)


ex1 = test $ DDef x $ EFun x (EVar x) `EApp` EVar x
  where x = Name "x"

ppEnv m = vcat $ map ppSig $ listEnv m
  where ppSig (Name x,s) = text x <+> text "::" <+> ppQual ppType s



