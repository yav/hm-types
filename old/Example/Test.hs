import Example.AST
import Example.Infer

import HM.Infer.Env as Env (toList)
import HM.Type.Pretty
import HM.Infer.Error hiding (Error)
import qualified Data.Set as Set
import Example.Prelude

main :: IO ()
main = test prelude

test :: Decl -> IO ()
test d = do putStrLn "Env:"
            print $ ppEnv env
            putStrLn "Preds:"
            print $ vcat $ map (pp How 0) preds
            putStrLn "Errors:"
            print $ vcat $ map ppError errs
  where (env,errs,preds) = infer d




ppName :: Name -> Doc
ppName (Name n) = text n

ppEnv :: Env -> Doc
ppEnv env = vcat $ map ppTy $ Env.toList env
  where ppTy (x,s) = ppName x <+> text "::" <+> pp How 0 s

ppError :: Error -> Doc
ppError err =
  case err of
    UndefinedVariable x t ->
      text "Undefinde variable" <+> ppName x <+> text "::" <+> pp How 0 t

    MultipleDefinitions n ->
      text "Multiple definitions of" <+>
        fsep (punctuate comma (map ppName (Set.toList n)))

    UnificationError (MguError err1 t1 t2) ->
      text msg $$ nest 2 (pp How 0 t1 $$ pp How 0 t2)
      where msg = case err1 of
                    KindMismatch  -> "Kind mismatch:"
                    RecursiveType -> "Recursive type:"
                    ShapeMismatch -> "Shape mismatch:"

    OtherError (PatErr n t e) -> 
      case e of
        PatUndefined ts ->
          (text "Undefined pattern" <+> ppName n <+> text "::" <+> pp How 0 t
            <+> text "with sub-patterns of type:")
            $$ ppSub ts

        PatTooManyArgs ts ->
          (text "Pattern" <+> ppName n <+> text "::" <+> pp How 0 t
            <+> text "was applied to additional sub-patterns of type:")
            $$ ppSub ts

        PatTooFewArgs ts ->
          (text "Pattern" <+> ppName n <+> text "::" <+> pp How 0 t
            <+> text "requires more sub-patterns of type:")
            $$ ppSub ts

      where ppSub = nest 2 . vcat . map (pp How 0)
 


 

