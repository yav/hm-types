{-# LANGUAGE MultiParamTypeClasses #-}
module HMType.Example.Haskell where

import HMType.AST
import HMType.Sort
import Text.PrettyPrint

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



instance PPTCon TCon where
  ppTCon n TFun [t1,t2] = wrapUnless (n <= 5)
                        $ ppPrec 6 t1 <+> text "->" <+> ppPrec 5 t2
  ppTCon _ TList [t1]   = brackets (pp t1)
  ppTCon _ (TTuple n) ts | length ts == n
                        = parens $ fsep $ punctuate comma $ map pp ts
  ppTCon n t ts         = ppTApp n (text short) ts 
    where short = case t of
                    TFun      -> "(->)"
                    TList     -> "[]"
                    TTuple 0  -> "()"
                    TTuple 1  -> "(_)"
                    TTuple a  -> "(" ++ replicate a ',' ++ ")"
                    TUser s _ -> s

instance PP Type where
  ppPrec n (T t)  = ppPrec n t

