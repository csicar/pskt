module CodeGen.KtCore where

import Prelude hiding (print)
import Protolude (zipWithM)
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

data WhenCase a
  -- Conditions, return value
  = WhenCase [a] a deriving (Show)

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

varRefUnqual = VarRef . Qualified Nothing

qualifiedToKt :: MonadSupply m => (a -> m b) -> Qualified a -> m (Qualified b)
qualifiedToKt f (Qualified mModName a) = do
  res <- f a
  return $ Qualified (prependPsNamespace <$> mModName) res
  where
      prependPsNamespace (ModuleName ls) = ModuleName $ psNamespace : ls

qualifiedIdentToKt :: MonadSupply m => Qualified Ident -> m KtExpr
qualifiedIdentToKt qualIdent = VarRef <$> qualifiedToKt ktIdentFromIdent qualIdent
    

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
  deriving (Show)

data KtExpr
  = Package [ProperName Namespace]
  | Stmt [KtExpr]
  | ObjectDecl KtIdent KtExpr
  -- ^ object: name; body
  | ClassDecl [KtModifier] KtIdent [KtIdent] [KtExpr] KtExpr
  -- ^ class modifier; name; arguments; extends; body
  | If KtExpr KtExpr (Maybe KtExpr)
  | WhenExpr [WhenCase KtExpr]
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

getLength :: KtExpr -> KtExpr
getLength a = Property a $ varRefUnqual $ MkKtIdent "size"

ktInt :: Integer -> KtExpr
ktInt = Const . NumericLiteral . Left

ktString :: PSString -> KtExpr
ktString = Const . StringLiteral

nest' = nest 4

joinWith :: Doc a -> [Doc a] -> Doc a
joinWith c = concatWith f
   where
   f a b = a <> c <> softline <> b

commaSep :: [Doc a] -> Doc a
commaSep = joinWith ", "

braceNested :: Doc a -> Doc a
braceNested d = vsep [nest' $ vsep ["{", d], "}"]