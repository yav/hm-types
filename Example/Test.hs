import HMType.AST
import Example.Decls
import Text.Show.Pretty

main = ex1

test d = writeFile "out.hs" $ ppShow $ runTI $ inferDecl d


ex1 = test $ DRec $ DDef x $ EVar x
  where x = Name "x"





