module HMType.Pretty where

{-
import Text.PrettyPrint.HughesPJClass


class Pretty kind => PrettyTCon tcon kind | tcon -> kind where
  pPrintTCon :: PrettyLevel -> Rational -> tcon -> [Type tcon kind] -> Doc

instance => Pretty TParam where
  pPrintPrec (PrettyLevel 0) _ (TParam s _) = text s
  pPrintPrec (PrettyLevel l) _ (TParam s k) =
    parens (text s <+> text "::" <+> pPrintPrec (PrettyLevel (l-1)) 0 k)

instance Pretty TRef where
  pPrintPrec l n (TV _ p) = pPrintPrec l n p

instance PrettyTCon tcon kind => Pretty (Type tcon kind) where
  pPrintPrec l n ty =
    case t of
      TVar tvar -> prettyTApp l n (char '?' <> pPrintPrec l 0 tvar) ts
      TGen tvar -> prettyTApp l n (pPrintPrec l 0 tvar) ts
      TCon tcon -> pPrintTCon l n tcon ts
      TApp _ _  -> error "BUG: 'splitTApp' returned an applications"

    where
    (t,ts)    = splitTApp ty

prettyTApp :: Pretty a => PrettyLevel -> Rational -> Doc -> [a] -> Doc
prettyTApp l n f ps = prettyParen (not (null ps) && n >= 9)
                        (f <+> fsep (map (pPrintPrec l 9) ps))

instance (PrettyTCon tcon kind, Pretty t) => Pretty (Qual tcon kind t) where
  pPrintPrec l n (Forall _ [] t)  = pPrintPrec l n t
  pPrintPrec l n (Forall _ ps t)  = prettyParen (n /= 0)
                                     (preds <+> text "=>" <+> pPrintPrec l 0 t)
    where preds = parens $ hsep $ punctuate comma $ map (pPrintPrec l 0) ps

-}


