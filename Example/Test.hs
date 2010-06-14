import Example.AST
import Example.Infer

main :: IO ()
main = test ex1

test :: Decl -> IO ()
test d = do putStrLn "Env:"
            print env
            putStrLn "Preds:"
            print $ unlines $ map show preds
            putStrLn "Errors:"
            putStrLn $ unlines $ map show errs
  where (env,errs,preds) = infer d


ex1 :: Decl
ex1 = DRec $ DDef x $ EFun x (EVar x) `EApp` EVar x
  where x = Name "x"


