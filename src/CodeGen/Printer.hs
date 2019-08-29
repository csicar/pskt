module CodeGen.Printer where

import Prelude hiding (print)
import Protolude (zipWithM, liftM2)
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
import CodeGen.KtCore
import Data.Functor.Foldable
import CodeGen.CoreImp

moduleToText :: Module Ann -> SimpleDocStream ()
moduleToText mod = layoutPretty defaultLayoutOptions $ vsep $ addAnnotations $ printExpr <$> moduleToKt' mod
   where
      addAnnotations a = "@file:Suppress(\"UNCHECKED_CAST\")" : a

nest' = nest 4

joinWith :: Doc a -> [Doc a] -> Doc a
joinWith c = concatWith f
   where
   f a b = a <> c <> softline <> b

commaSep :: [Doc a] -> Doc a
commaSep = joinWith ", "

braceNested :: Doc a -> Doc a
braceNested d = vsep [nest' $ vsep ["{", d], "}"]

printKtIdent :: KtIdent -> Doc ()
printKtIdent (MkKtIdent txt) = pretty txt

printKtModifier :: KtModifier -> Doc ()
printKtModifier Sealed = "sealed"
printKtModifier Data = "data"

printQualified :: (a -> Doc()) -> Qualified a -> Doc ()
printQualified p (Qualified Nothing a) = p a
printQualified p (Qualified (Just mod) a) = pretty (runModuleName mod) <> "." <> p a

printLiteral :: Literal (Doc ()) -> Doc ()
printLiteral (NumericLiteral (Left a))= pretty (show a)
printLiteral (NumericLiteral (Right a))= pretty (show a)
printLiteral (StringLiteral a) = pretty (show a)
printLiteral (CharLiteral a) = pretty (show a)
printLiteral (BooleanLiteral a) = if a then "true" else "false"
printLiteral (ArrayLiteral as) = printExprAlg (Call "listOf" as)
printLiteral (ObjectLiteral as) = printExprAlg $ Call "mapOf" $ (\(key, val) -> pretty (show key) <+> "to" <+> val) <$> as


printExpr :: KtExpr -> Doc ()
printExpr = cata printExprAlg

extendsDoc = \case  
   [] -> ""
   extends -> ":" <+> hsep extends

printExprAlg :: KtExprF (Doc ()) -> Doc ()
printExprAlg (Package ns) = "package" <+> joinWith "." (pretty . runProperName <$> ns)
printExprAlg (Import ns val) = "import" <+> joinWith "." (pretty . runProperName <$> ns) <> "." <> printKtIdent val
printExprAlg (Stmt []) = "{}"
printExprAlg (Stmt stmts) = braceNested $ vsep stmts
printExprAlg (ObjectDecl ident extends body) = "object" <+> printKtIdent ident <+> extendsDoc extends <+> body
printExprAlg (ClassDecl mods name args extends body) =
      hsep (printKtModifier <$> mods) <+> "class" <+> printKtIdent name 
         <+> parens (commaSep $ ("val"<+>) . (<+> ": Any") . printKtIdent <$> args) <+> extendsDoc extends <+> body
printExprAlg (VarRef qualIdent) = printQualified printKtIdent qualIdent
printExprAlg (Call a args) = a <> parens (commaSep args)
printExprAlg (VariableIntroduction ident a) = "val" <+> printKtIdent ident <+> "=" <+> a
printExprAlg (Lambda arg body) = braceNested $ printKtIdent arg <+> ": Any ->" <+> body
printExprAlg (Fun mName arg body) = 
   "fun" <+> maybe "" printKtIdent mName <> parens (printKtIdent arg <+> ": Any") <> ": Any =" <+> body
printExprAlg (Property a b) = a <> "." <> b
printExprAlg (FunRef qualIdent) = parens $ "::" <> printQualified printKtIdent qualIdent
printExprAlg (Const lit) = printLiteral lit
printExprAlg (WhenExpr cases) = "when" <+> braceNested (vsep $ printWhenCases <$> cases)
printExprAlg (Binary op a b) = a <+> printOp op <+> b
printExprAlg (ObjectAccess obj key) = obj <> brackets key
printExprAlg (Cast a b) = parens $ a <+> "as" <+> b
printExprAlg a = pretty $ show a

printOp Equals = "=="
printOp IsType = "is"
printOp And = "&&"

printWhenCases (WhenCase conds body) = joinWith "&&" (ensureOneElement conds) <+> "->" <+> body
   where 
      ensureOneElement [] = ["true"]
      ensureOneElement a = a
printWhenCases (ElseCase body) = "else ->" <+> body