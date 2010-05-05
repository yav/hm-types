{-# LANGUAGE MultiParamTypeClasses #-}
module HMType.Example.Haskell where

import HMType.AST
import HMType.Sort
import Text.PrettyPrint.HughesPJClass

-- Kinds -----------------------------------------------------------------------
data KCon         = KFun            -- The kind of type constructors.
                  | KStar           -- The kind of ordinary value types.
                  | KPred           -- The kind of predicates.
                    deriving Eq

newtype Kind      = K (HMType KCon Sort)
                    deriving Eq

kFun             :: Kind -> Kind -> Kind
kFun (K a) (K b)  = K (TCon KFun `TApp` a `TApp` b)

kStar            :: Kind
kStar             = K (TCon KStar)

kPred            :: Kind
kPred             = K (TCon KPred)

instance IsKind Kind where
  isKFun (K (TCon KFun `TApp` a `TApp` b))  = Just (K a, K b)
  isKFun _                                  = Nothing

instance PrettyTCon KCon Sort where

  pPrintTCon l n KFun [t1,t2] = prettyParen (n > 5) $
    pPrintPrec l 6 t1 <+> text "->" <+> pPrintPrec l 5 t2

  pPrintTCon l n t ts = prettyTApp l n (text short) ts 
    where short = case t of
                    KFun  -> "(->)"
                    KStar -> "*"
                    KPred -> "prop"

instance Pretty Kind where
  pPrintPrec l n (K k) = pPrintPrec l n k
--------------------------------------------------------------------------------


-- Type constructors -----------------------------------------------------------
data TCon         = TFun
                  | TList
                  | TTuple Int
                  | TUser String Kind

newtype Type      = T (HMType TCon Kind)

tFun             :: Type -> Type -> Type
tFun (T a) (T b)  = T (TCon TFun `TApp` a `TApp` b)

tList            :: Type -> Type
tList (T a)       = T (TCon TList `TApp` a)

tTuple           :: [Type] -> Type
tTuple ts         = T (foldl TApp (TCon (TTuple (length ts))) [ t | T t <- ts])

tUser            :: String -> Kind -> Type
tUser x t         = T (TCon (TUser x t))


instance KindOf TCon Kind where
  kindOf tcon =
    case tcon of
      TFun        -> kStar `kFun` (kStar `kFun` kStar)
      TList       -> kStar `kFun` kStar
      TTuple n    -> foldr kFun kStar (replicate n kStar)
      TUser _ k   -> k

instance KindOf Type Kind where
  kindOf (T t)  = kindOf t

instance HasKinds TCon Kind where
  mapKinds f (TUser s k)  = TUser s (f k)
  mapKinds _ k            = k

instance HasKinds Type Kind where
  mapKinds f (T t)  = T (mapKinds f t)



instance PrettyTCon TCon Kind where

  pPrintTCon l n TFun [t1,t2] = prettyParen (n > 5) $
    pPrintPrec l 6 t1 <+> text "->" <+> pPrintPrec l 5 t2

  pPrintTCon l _ TList [t1] =
    brackets (pPrintPrec l 0 t1)

  pPrintTCon l _ (TTuple n) ts | length ts == n =
    parens $ fsep $ punctuate comma $ map (pPrintPrec l 0) ts

  pPrintTCon l n t ts = prettyTApp l n (text short) ts 
    where short = case t of
                    TFun      -> "(->)"
                    TList     -> "[]"
                    TTuple 0  -> "()"
                    TTuple 1  -> "(_)"
                    TTuple a  -> "(" ++ replicate a ',' ++ ")"
                    TUser s _ -> s

instance Pretty Type where
  pPrintPrec l n (T t)  = pPrintPrec l n t

