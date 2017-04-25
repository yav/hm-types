module HM.Type.Pretty
  ( module HM.Type.Pretty
  , module Text.PrettyPrint
  ) where

import HM.Type.AST
import Text.PrettyPrint

data How = How

class Pretty t where
  pp :: How -> Rational -> t -> Doc


class PrettyCon c where
  ppCon :: How -> Rational -> c -> [Rational -> Doc] -> Doc

instance (Pretty t, PrettyCon c) => Pretty (Qual c t) where
  pp how r (Forall _ [] t)  = pp how r t
  pp how 0 (Forall _ ps t)  =  hd <+> text "=>" <+> pp how 0 t
    where hd = parens $ fsep $ punctuate comma $ map (pp how 0) ps
  pp how _ s                = parens (pp how 0 s)


instance PrettyCon c => Pretty (Type c) where
  pp how r ty =
    case f of
      TCon c    -> ppCon how r c  docs
      TVar tv   -> ppCon how r tv docs
      TGen gv   -> ppCon how r gv docs
      TApp _ _  -> error "BUG: splitTApp return TApp"

    where (f,ts) = splitTApp ty
          docs   = [ \r1 -> pp how r1 t | t <- ts ] 


wrap :: Rational -> Rational -> Doc -> Doc
wrap n r d = if r <= n then d else parens d

ppTApp :: Rational -> Doc -> [ Rational -> Doc ] -> Doc
ppTApp _ d [] = d
ppTApp r d ds = wrap 9 r (d <+> fsep (map ($ 9) ds))

instance PrettyCon c => PrettyCon (TVar c) where
  ppCon _ r (TV n (TParam s _)) = ppTApp r (char '?' <> name)
    where name = if null s then int n else text s 

instance PrettyCon c => PrettyCon (GVar c) where
  ppCon _ r (GV n (TParam s _)) = ppTApp r (char '!' <> name)
    where name = if null s then int n else text s 

prettyCon :: PrettyCon c => How -> Rational -> c -> Doc
prettyCon how r c = ppCon how r c []

instance PrettyCon c => Pretty (TVar c) where pp = prettyCon
instance PrettyCon c => Pretty (GVar c)   where pp = prettyCon



