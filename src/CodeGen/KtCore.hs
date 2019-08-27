module CodeGen.KtCore where

import Prelude hiding (print)
import Data.Text (Text)
import Data.Maybe (catMaybes, fromMaybe, Maybe(..))
import Data.List (nub)
import Debug.Trace (trace)
import qualified Data.Text as T
import Control.Monad (forM, replicateM, void)
import Control.Monad.Supply.Class (MonadSupply, fresh)
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

newtype WhenExpr a = WhenExpr [WhenCase a]

data WhenCase a
  -- Conditions, return value
  = WhenCase [a] a

data BinOp
  = Equals
  | IsType
  | And
  deriving (Show)

newtype KtIdent = MkKtIdent Text deriving (Show)

freshText :: MonadSupply m => Text -> m Text
freshText hint = (("_" <> hint) <>) . T.pack . show <$> fresh

unreserve :: Text -> Text
unreserve txt | "_" `T.isPrefixOf` txt = "_" <> txt
unreserve txt | isReserved txt = "_" <> txt
unreserve txt = txt

identFromNameSpace :: MonadSupply m => ProperName Namespace -> m KtIdent
identFromNameSpace (ProperName txt) = return $ MkKtIdent $ unreserve txt

identFromCtorName :: MonadSupply m => ProperName ConstructorName -> m KtIdent
identFromCtorName (ProperName txt) = return $ MkKtIdent $ unreserve txt

identFromTypeName :: MonadSupply m => ProperName TypeName -> m KtIdent
identFromTypeName (ProperName txt) = return $ MkKtIdent $ ("_Type"<>) $ unreserve txt

ktIdentFromIdent :: MonadSupply m => Ident -> m KtIdent
ktIdentFromIdent ident = return $ MkKtIdent $ unreserve $ runIdent ident

qualifiedIdentToKt :: MonadSupply m => Qualified Ident -> m KtExpr
qualifiedIdentToKt (Qualified mModName ident) = do
    ktIdent <- ktIdentFromIdent ident
    return $ VarRef (Qualified (prependPsNamespace <$> mModName) ktIdent)
    where
      prependPsNamespace (ModuleName ls) = ModuleName $ psNamespace : ls

forMLiteral :: Monad m => Literal a -> (a -> m b) -> m (Literal b)
forMLiteral (ArrayLiteral ls) f = do
    ls' <- mapM f ls
    return $ ArrayLiteral ls'
forMLiteral (ObjectLiteral ls) f = do
    ls' <- mapM (\(key, val) -> (key,) <$> f val) ls
    return $ ObjectLiteral ls' 
forMLiteral (NumericLiteral a) _ = return $ NumericLiteral a
forMLiteral (StringLiteral a) _ = return $ StringLiteral a
forMLiteral (CharLiteral a) _ = return $ CharLiteral a
forMLiteral (BooleanLiteral a) _ = return $ BooleanLiteral a

data KtModifier
  = Sealed 
  deriving (Show)

data KtExpr
  = Package [ProperName Namespace]
  | Stmt [KtExpr]
  | ObjectDecl KtIdent KtExpr
  -- ^ object: name; body
  | ClassDecl [KtModifier] KtIdent [KtIdent] [KtExpr] KtExpr
  -- ^ class modifier; name; arguments; extends; body
  | If KtExpr KtExpr (Maybe KtExpr)
  | VariableIntroduction KtIdent KtExpr
  | Binary BinOp KtExpr KtExpr
  | Property KtExpr KtExpr
  | ArrayAccess KtExpr KtExpr
  | ObjectAccess KtExpr KtExpr
  | VarRef (Qualified KtIdent)
  | Cast KtExpr KtExpr
  | Fun (Maybe KtIdent) KtIdent KtExpr
  | Lambda KtIdent KtExpr
  | Call KtExpr [KtExpr]
  | Const (Literal KtExpr)
  deriving (Show)


nest' = nest 4

joinWith :: Doc a -> [Doc a] -> Doc a
joinWith c = concatWith f
   where
   f a b = a <> c <> softline <> b

commaSep :: [Doc a] -> Doc a
commaSep = joinWith ", "

braceNested :: Doc a -> Doc a
braceNested d = vsep [nest' $ vsep ["{", d], "}"]