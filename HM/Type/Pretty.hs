module HM.Type.Pretty where

{-
import HM.Type.AST
import Text.PrettyPrint

ppQual :: (t -> Doc) -> Qual t -> Doc
ppQual pp (Forall _ [] t) = pp pC t
ppQual pp (Forall _ ps t) = hd <+> text "=>" <+> pp t
  where hd = parens $ fsep $ punctuate comma $ map ppType ps
        bd

ppType :: (c -> Doc) -> Type c -> Doc
ppType pp ty = ppAtom pp f <+> fsep (map (ppAtom pp) ts)
  where (f,ts) = splitTApp ty

ppAtom :: (c -> Doc) -> Type c -> Doc
ppAtom pp atom =
  case atom of
    TCon c      -> pp c -- XXX: Assumes that these do not need wrapping.
    TVar tv     -> ppTVar tv
    TGen gv     -> ppGVar gv
    TApp _ _    -> parens (ppType atom)

ppTVar (TV n (TParam s _)) = char '?' <> (if null s then int n else text s)
ppTGar (TV n (TParam s _)) = char '!' <> (if null s then int n else text s)
-}


