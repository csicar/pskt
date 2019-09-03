{-# LANGUAGE TemplateHaskell #-}
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
import Text.Show.Deriving (deriveShow1)
import Control.Arrow ((>>>))

data WhenCase a
  -- Conditions, return value
  = WhenCase [a] a
  | ElseCase a deriving (Show, Functor)

$(deriveShow1 ''WhenCase)
$(deriveShow1 ''Literal)


data BinOp
  = Equals
  | IsType
  | And
  | To -- for Pairs: `1 to "Hi"`
  | Add
  deriving (Show)

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
  deriving (Show)

data KtExprF r
  = Package [ProperName Namespace]
  | Import [ProperName Namespace] KtIdent
  | Stmt [r]
  | ObjectDecl (Maybe KtIdent) [r] r
  -- ^ object: name; extends; body
  | ClassDecl [KtModifier] KtIdent [KtIdent] [r] r
  -- ^ class modifier; name; arguments; extends; body
  | If r r (Maybe r)
  | WhenExpr [WhenCase r]
  | VariableIntroduction KtIdent r
  | Binary BinOp r r
  | Property r r
  | ArrayAccess r r
  | ObjectAccess r r
  | VarRef (Qualified KtIdent)
  | Cast r r
  | Fun (Maybe KtIdent) [KtIdent] r
  | FunRef (Qualified KtIdent)
  | Lambda KtIdent r
  | Call r [r]
  | Const (Literal r)
  | Annotated r r
  deriving (Show, Functor)

$(deriveShow1 ''KtExprF)

type KtExpr = Fix KtExprF

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

varRefUnqual = Fix . VarRef . Qualified Nothing

qualifiedToKt :: MonadSupply m => (a -> m b) -> Qualified a -> m (Qualified b)
qualifiedToKt f (Qualified mModName a) = do
  res <- f a
  return $ Qualified (go <$> mModName) res
  where
      go (ModuleName ls) = ModuleName $ psNamespace : ls ++ [moduleNamespace]

qualifiedIdentToKt :: MonadSupply m => Qualified Ident -> m KtExpr
qualifiedIdentToKt qualIdent = Fix . VarRef <$> qualifiedToKt ktIdentFromIdent qualIdent

getLength :: KtExpr -> KtExpr
getLength a = ktProperty (ktAsList a) (varRefUnqual $ MkKtIdent "size")

getEntryCount :: KtExpr -> KtExpr
getEntryCount a = ktProperty (ktCast a (varRefUnqual $ MkKtIdent "Map<String, Any>")) (varRefUnqual $ MkKtIdent "size")

ktInt :: Integer -> KtExpr
ktInt = ktConst . NumericLiteral . Left

ktString :: PSString -> KtExpr
ktString = ktConst . StringLiteral

ktJvmValue :: KtExpr -> KtExpr
ktJvmValue = ktAnnotated (varRefUnqual $ MkKtIdent "JvmField")

ktAsBool :: KtExpr -> KtExpr
ktAsBool a = ktCast a (varRefUnqual $ MkKtIdent "Boolean")

ktAsAny :: KtExpr -> KtExpr
ktAsAny a = ktCast a (varRefUnqual $ MkKtIdent "Any")

ktAsString :: KtExpr -> KtExpr
ktAsString a = ktCast a (varRefUnqual $ MkKtIdent "String")

ktAsList :: KtExpr -> KtExpr
ktAsList a = ktCast a (varRefUnqual $ MkKtIdent "List<Any>")

ktConst = Fix . Const

ktEq a b = Fix $ Binary Equals a b
ktIsType a b = Fix $ Binary IsType a b
ktPair a b = Fix $ Binary To a b
ktAdd a b = Fix $ Binary Add a b

ktProperty a b = Fix $ Property a b

ktVariable a b = Fix $ VariableIntroduction a b

ktVarRef = Fix . VarRef

ktCall a b = Fix $ Call a b

ktStmt = Fix . Stmt 

ktArrayAccess a b = Fix $ ArrayAccess a b 
ktObjectAccess a b = Fix $ ObjectAccess a b

ktLambda a b = Fix $ Lambda a b
ktObjectDecl a b c = Fix $ ObjectDecl (Just a) b c

ktUnnamedObj b c = Fix $ ObjectDecl Nothing b c

ktClassDecl a b c d e = Fix $ ClassDecl a b c d e

ktWhenExpr = Fix . WhenExpr

ktPackage = Fix . Package

ktImport a b = Fix $ Import a b

ktFun' a b c = Fix $ Fun a b c

ktFun a b = ktFun' a [b]

ktFunRef = Fix . FunRef

ktCast a b = Fix $ Cast a b

ktAnnotated a b = Fix $ Annotated a b


-- <a>.app(<b>)
pattern CallApp a b = (Call (Fix (Property a (Fix (VarRef (Qualified Nothing (MkKtIdent "app")))))) [b])