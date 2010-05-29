module HMType.Pretty where

import HMType.AST
import Text.PrettyPrint

ppQual :: (t -> Doc) -> Qual t -> Doc
ppQual pp (Forall _ [] t) = pp t
ppQual pp (Forall _ ps t) = hd <+> text "=>" <+> pp t
  where hd = parens $ fsep $ punctuate comma $ map ppType ps

ppType :: Type -> Doc
ppType ty =
  case splitTApp ty of
    (a, r, ts) -> ppAtom a <> ppTRef r <+> fsep (map ppParam ts)
      where wrap (TAtom _ _)  = id
            wrap (TApp _ _)   = parens
            ppParam t         = wrap t (ppType t)

ppAtom :: Atom -> Doc
ppAtom atom =
  case atom of
    TCon -> char ':'
    TVar -> char '?'
    TGen -> char '!'

ppTRef :: TRef -> Doc
ppTRef (TR n (TParam s _)) = if null s then int n else text s


