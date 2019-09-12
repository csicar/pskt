{-# LANGUAGE TemplateHaskell, TypeFamilies, DeriveTraversable, StandaloneDeriving #-}
module CodeGen.KtCore where

import Prelude hiding (print)
import Protolude (zipWithM, liftM2)
import Data.Text (Text)
import Data.Maybe (catMaybes, fromMaybe, Maybe(..))
import Data.List (nub)
import Debug.Trace (trace)
import qualified Data.Text as T
import Control.Monad (forM, replicateM, void)
import Control.Monad.Supply.Class (MonadSupply, fresh)
import           Data.Functor.Classes (Show1(..))
import Language.PureScript.CoreFn.Expr
import Language.PureScript.CoreFn.Module
import Language.PureScript.CoreFn.Meta
import Language.PureScript.CoreFn.Ann
import Language.PureScript.AST.Literals
import Language.PureScript.Names
import Language.PureScript.CoreFn.Traversals
import Language.PureScript.CoreFn.Binders
import Language.PureScript.PSString (prettyPrintStringJS, PSString)
import Data.Text.Prettyprint.Doc
import Text.Pretty.Simple (pShow)
import Language.PureScript.AST.SourcePos (displayStartEndPos)
import CodeGen.Constants
import Data.Functor.Foldable
import Data.Functor.Foldable.TH
import Text.Show.Deriving (deriveShow, deriveShow1)
import Control.Arrow ((>>>))

data WhenCase a
  -- Conditions, return value
  = WhenCase [a] a
  | ElseCase a deriving (Show, Functor, Foldable, Traversable, Eq)

$(deriveShow1 ''WhenCase)
$(deriveShow1 ''Literal)

deriving instance Traversable Literal

data BinOp
  = Equals
  | IsType
  | And
  | To -- for Pairs: `1 to "Hi"`
  | Add
  deriving (Show, Eq)

newtype KtIdent = MkKtIdent Text deriving (Show, Eq)
runKtIdent (MkKtIdent a) = a

forMLiteral :: Monad m => Literal a -> (a -> m b) -> m (Literal b)
forMLiteral lit f = forMLitKey lit (const f) (const f)

forMLitKey :: Monad m => Literal a -> (Integer -> a -> m b) -> (PSString -> a -> m b) -> m (Literal b)
forMLitKey (ArrayLiteral ls) f _ = do
    ls' <- zipWithM f [0 ..] ls
    return $ ArrayLiteral ls'
forMLitKey (ObjectLiteral ls) _ g = do
    ls' <- mapM (\entry -> (fst entry,) <$> uncurry g entry) ls
    return $ ObjectLiteral ls' 
forMLitKey (NumericLiteral a) _ _ = return $ NumericLiteral a
forMLitKey (StringLiteral a) _ _ = return $ StringLiteral a
forMLitKey (CharLiteral a) _ _ = return $ CharLiteral a
forMLitKey (BooleanLiteral a) _ _ = return $ BooleanLiteral a

instance Foldable Literal where
  foldr f e (ArrayLiteral ls) = foldr f e ls
  foldr f e (ObjectLiteral ls) = foldr (f . snd) e ls
  foldr _ e _ = e

-- mapLiteralParts :: ([a] -> b) -> ([(PSString, a)] -> [(PSString, b)]) -> ((Literal a) -> (Literal b)) -> Literal b

data KtModifier
  = Sealed
  | Data
  deriving (Show, Eq)

data KtExpr
  = Package [ProperName Namespace]
  | Import [ProperName Namespace] KtIdent
  | Stmt [KtExpr]
  | ObjectDecl (Maybe KtIdent) [KtExpr] KtExpr
  -- ^ object: name; extends; body
  | ClassDecl [KtModifier] KtIdent [KtIdent] [KtExpr] KtExpr
  -- ^ class modifier; name; arguments; extends; body
  | If KtExpr KtExpr (Maybe KtExpr)
  | While KtExpr KtExpr
  | WhenExpr [WhenCase KtExpr]
  | VariableIntroduction KtIdent KtExpr
  | Binary BinOp KtExpr KtExpr
  | Property KtExpr KtExpr
  | ArrayAccess KtExpr KtExpr
  | ObjectAccess KtExpr KtExpr
  | VarRef (Qualified KtIdent)
  | Cast KtExpr KtExpr
  | Fun (Maybe KtIdent) [KtIdent] KtExpr
  | FunRef (Qualified KtIdent)
  | Lambda KtIdent KtExpr
  | Defer KtExpr -- Lambda without arguments; in Kotlin: { <body> }
  | Call KtExpr [KtExpr]
  | Const (Literal KtExpr)
  | Annotated KtExpr KtExpr
  deriving (Show, Eq)

makeBaseFunctor ''KtExpr
deriveShow ''KtExprF

mapType :: KtIdent
mapType = MkKtIdent "Map<String, Any>"

freshText :: MonadSupply m => Text -> m Text
freshText hint = (("_" <> hint) <>) . T.pack . show <$> fresh

unreserve :: Text -> Text
unreserve = renameUnused
  >>> prefixUnderscore 
  >>> escapeReserved 
  >>> T.concatMap escapeSpecialChars
  where
    renameUnused "$__unused" = "_"
    renameUnused t = t

    prefixUnderscore "_" = "_"
    prefixUnderscore txt | "_" `T.isPrefixOf` txt = "_" <> txt
    prefixUnderscore txt = txt

    escapeReserved txt | isReserved txt = "_" <> txt
    escapeReserved txt = txt

    escapeSpecialChars '$' = "_dollar"
    escapeSpecialChars '\'' = "_tick"
    escapeSpecialChars c = T.singleton c

identFromNameSpace :: MonadSupply m => ProperName Namespace -> m KtIdent
identFromNameSpace (ProperName txt) = return $ MkKtIdent $ unreserve txt

identFromCtorName :: MonadSupply m => ProperName ConstructorName -> m KtIdent
identFromCtorName (ProperName txt) = return $ MkKtIdent $ unreserve txt

identFromTypeName :: MonadSupply m => ProperName TypeName -> m KtIdent
identFromTypeName (ProperName txt) = return $ MkKtIdent $ ("_Type_"<>) $ unreserve txt

ktIdentFromIdent :: MonadSupply m => Ident -> m KtIdent
ktIdentFromIdent ident = return $ MkKtIdent $ unreserve $ runIdent ident

varRefUnqual = VarRef . Qualified Nothing

qualifiedToKt :: MonadSupply m => (a -> m b) -> Qualified a -> m (Qualified b)
qualifiedToKt f (Qualified mModName a) = do
  res <- f a
  return $ Qualified (go <$> mModName) res
  where
      go (ModuleName ls) = ModuleName $ psNamespace : ls ++ [moduleNamespace]

qualifiedIdentToKt :: MonadSupply m => Qualified Ident -> m KtExpr
qualifiedIdentToKt qualIdent = VarRef <$> qualifiedToKt ktIdentFromIdent qualIdent

getLength :: KtExpr -> KtExpr
getLength a = Property (ktAsList a) (varRefUnqual $ MkKtIdent "size")

getEntryCount :: KtExpr -> KtExpr
getEntryCount a = Property (Cast a (varRefUnqual $ MkKtIdent "Map<String, Any>")) (varRefUnqual $ MkKtIdent "size")

ktInt :: Integer -> KtExpr
ktInt = Const . NumericLiteral . Left

ktString :: PSString -> KtExpr
ktString = Const . StringLiteral

ktJvmValue :: KtExpr -> KtExpr
ktJvmValue = Annotated (varRefUnqual $ MkKtIdent "JvmField")

pattern KtAsBool a = Cast a (VarRef (Qualified Nothing (MkKtIdent "Boolean")))

ktAsAny :: KtExpr -> KtExpr
ktAsAny a = Cast a (varRefUnqual $ MkKtIdent "Any")

ktAsString :: KtExpr -> KtExpr
ktAsString a = Cast a (varRefUnqual $ MkKtIdent "String")

ktAsList :: KtExpr -> KtExpr
ktAsList a = Cast a (varRefUnqual $ MkKtIdent "List<Any>")

ktFun a b = Fun a [b]

-- <a>.app(<b>)
pattern CallAppF a b = (CallF (Property a (VarRef (Qualified Nothing (MkKtIdent "app")))) [b])
pattern CallApp a b = (Call (Property a (VarRef (Qualified Nothing (MkKtIdent "app")))) [b])

-- <a>.appRun()
pattern RunF a = (CallF (Property a (VarRef (Qualified Nothing (MkKtIdent "appRun")))) [])
pattern Run a = (Call (Property a (VarRef (Qualified Nothing (MkKtIdent "appRun")))) [])

pattern Unit = VarRef (Qualified Nothing (MkKtIdent "Unit"))

pattern Not a = Property (KtAsBool a) (Call (VarRef (Qualified Nothing (MkKtIdent "not"))) [])