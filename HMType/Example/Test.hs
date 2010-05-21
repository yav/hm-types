import HMType.AST
import HMType.Example.Decls
import Text.Show.Pretty

test d = putStrLn $ ppShow $ runTI $ inferDecl d


ex1 = test $ DRec $ DDef x $ EVar x
  where x = Name "x"





