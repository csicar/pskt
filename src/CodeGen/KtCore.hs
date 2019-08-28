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
  deriving (Show)

newtype KtIdent = MkKtIdent Text deriving (Show)

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
  | ObjectDecl KtIdent r
  -- ^ object: name; body
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
  | Fun (Maybe KtIdent) KtIdent r
  | Lambda KtIdent r
  | Call r [r]
  | Const (Literal r)
  deriving (Show, Functor)

$(deriveShow1 ''KtExprF)

type KtExpr = Fix KtExprF

-- everywhere :: MonadSupply m => (KtExpr -> m KtExpr) -> KtExpr -> m KtExpr
-- everywhere f (Stmt as) = Stmt <$> mapM f as
-- everywhere f (ObjectDecl a b) = ObjectDecl a <$> f b
-- everywhere f (ClassDecl a b c args body) = do
--   args' <- mapM (everywhere f) 
--   pure $ ClassDecl a b c args' body'
-- everywhere f (If KtExpr KtExpr (Maybe KtExpr)) = Stmt 
-- everywhere f (WhenExpr [WhenCase KtExpr]) = Stmt 
-- everywhere f (VariableIntroduction KtIdent KtExpr) = Stmt 
-- everywhere f (Binary BinOp KtExpr KtExpr) = Stmt 
-- everywhere f (Property KtExpr KtExpr) = Stmt 
-- everywhere f (ArrayAccess KtExpr KtExpr) = Stmt 
-- everywhere f (ObjectAccess KtExpr KtExpr) = Stmt 
-- everywhere f (VarRef (Qualified KtIdent)) = Stmt 
-- everywhere f (Cast KtExpr KtExpr) = Stmt 
-- everywhere f (Fun (Maybe KtIdent) KtIdent KtExpr) = Stmt 
-- everywhere f (Lambda KtIdent KtExpr) = Stmt 
-- everywhere f (Call KtExpr [KtExpr]) = Stmt 
-- everywhere f (Const (Literal KtExpr)) = Stmt 

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

varRefUnqual = Fix . VarRef . Qualified Nothing

qualifiedToKt :: MonadSupply m => (a -> m b) -> Qualified a -> m (Qualified b)
qualifiedToKt f (Qualified mModName a) = do
  res <- f a
  return $ Qualified (prependPsNamespace <$> mModName) res
  where
      prependPsNamespace (ModuleName ls) = ModuleName $ psNamespace : ls

qualifiedIdentToKt :: MonadSupply m => Qualified Ident -> m KtExpr
qualifiedIdentToKt qualIdent = Fix . VarRef <$> qualifiedToKt ktIdentFromIdent qualIdent

getLength :: KtExpr -> KtExpr
getLength a = ktProperty a $ varRefUnqual $ MkKtIdent "size"

ktInt :: Integer -> KtExpr
ktInt = ktConst . NumericLiteral . Left

ktString :: PSString -> KtExpr
ktString = ktConst . StringLiteral

ktConst = Fix . Const

ktEq a b = Fix $ Binary Equals a b
ktIsType a b = Fix $ Binary IsType a b

ktProperty a b = Fix $ Property a b

ktVariable a b = Fix $ VariableIntroduction a b

ktVarRef = Fix . VarRef

ktCall a b = Fix $ Call a b

ktStmt = Fix . Stmt 

ktArrayAccess a b = Fix $ ArrayAccess a b 
ktObjectAccess a b = Fix $ ObjectAccess a b

ktLambda a b = Fix $ Lambda a b
ktObjectDecl a b = Fix $ ObjectDecl a b

ktClassDecl a b c d e = Fix $ ClassDecl a b c d e

ktWhenExpr = Fix . WhenExpr

ktPackage = Fix . Package

ktImport a b = Fix $ Import a b