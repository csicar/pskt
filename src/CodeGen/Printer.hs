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

nest' = nest 2

joinWith :: Doc a -> [Doc a] -> Doc a
joinWith c = concatWith f
   where
   f a b = a <> c <> softline <> b

joinWith' :: Doc a -> [Doc a] -> Doc a
joinWith' c = concatWith f
   where
   f a b = a <> c <> softline' <> b

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

printString :: PSString -> Text
printString = T.concatMap go . prettyPrintStringJS
      where 
         go '$' = "${'$'}"
         go c = T.singleton c

printLiteral :: Literal (KtExpr, Doc ()) -> Doc ()
printLiteral (NumericLiteral (Left a))= pretty (show a)
printLiteral (NumericLiteral (Right a))= pretty (show a)
printLiteral (StringLiteral a) = pretty (printString a)
printLiteral (CharLiteral a) = pretty (show a)
printLiteral (BooleanLiteral a) = if a then "true" else "false"
printLiteral (ArrayLiteral []) = printExprAlg (Call (emptyList, printExpr emptyList) [])
   where emptyList = varRefUnqual $ MkKtIdent "emptyList<Any>"
printLiteral (ArrayLiteral as) = printExprAlg (Call (listOf, printExpr listOf) as)
   where listOf = varRefUnqual (MkKtIdent "listOf")
printLiteral (ObjectLiteral []) = printExprAlg $ Call (emptyMap, printExpr emptyMap) []
   where emptyMap = varRefUnqual $ MkKtIdent "emptyMap<String, Any>"
printLiteral (ObjectLiteral as) = printExprAlg $ 
      -- TODO: printExpr $ pairOf recalculates the printvalue of valExpr. fix that
      Call (mapOf, printExpr mapOf) $ (\(key, (valExpr, val)) -> (pairOf key valExpr, printExpr $ pairOf key valExpr)) <$> as
   where 
      mapOf = varRefUnqual (MkKtIdent "mapOf")
      pairOf key = ktPair (ktString key)


printExpr :: KtExpr -> Doc ()
printExpr = para printExprAlg

extendsDoc = \case  
   [] -> ""
   extends -> ":" <+> hsep extends

printExprAlg :: KtExprF (KtExpr, Doc ()) -> Doc ()
printExprAlg (Package ns) = "package" <+> joinWith' "." (pretty . runProperName <$> ns)
printExprAlg (Import ns val) = "import" <+> joinWith' "." (pretty . runProperName <$> ns) <> "." <> printKtIdent val
printExprAlg (Stmt []) = "{}"
printExprAlg (Stmt stmts) = braceNested $ vsep $ (<> ";")  . group . snd <$> stmts
printExprAlg (ObjectDecl ident extends (_, body)) = "object" <+> maybe "" printKtIdent ident <+> extendsDoc (snd <$> extends) <+> body
printExprAlg (ClassDecl mods name args extends (_, body)) =
      hsep (printKtModifier <$> mods) <+> "class" <+> printKtIdent name 
         <+> parens (commaSep $ ("val"<+>) . (<+> ": Any") . printKtIdent <$> args) <+> extendsDoc (snd <$> extends) <+> body
printExprAlg (VarRef qualIdent) = printQualified printKtIdent qualIdent
printExprAlg (Call (_, a) args) = group $ nest' (a <> "(" <> commaSep ((softline' <>) . snd <$> args)) <> softline' <> ")"
printExprAlg (VariableIntroduction ident (_, a)) = "val" <+> printKtIdent ident <+> "=" <+> a
printExprAlg (Lambda arg (Fix (Stmt stmts), body)) = braces $ nest' $ 
   " " <> printKtIdent arg <+> ": Any ->" <> line' <> nest' (vsep (printExpr <$> stmts))
printExprAlg (Lambda arg (_, body)) = braces $ (<> line') $ nest' $
   " " <> printKtIdent arg <+> ": Any ->" <> line' <+> body
printExprAlg (Fun mName args (_, body)) = 
   "fun" <+> maybe "" printKtIdent mName <> parens (commaSep $ (<+> ": Any") . printKtIdent <$> args) <> ": Any =" <+> body
printExprAlg (Property (_, a) (_, b)) = align $ nest' $ a <> line' <> "."  <> b
printExprAlg (FunRef qualIdent) = parens $ "::" <> printQualified printKtIdent qualIdent
printExprAlg (Const lit) = printLiteral lit
printExprAlg (WhenExpr cases) = "when" <+> braceNested (vsep $ printWhenCases . fmap snd <$> cases)
printExprAlg (Binary op (_, a) (_, b)) = parens $ a <+> printOp op <+> b
printExprAlg (ObjectAccess (_, obj) (_, key)) = obj <> brackets key <> "!!"
printExprAlg (Cast (_, a) (_, b)) = parens $ a <+> "as" <+> b
printExprAlg (Annotated (_, ann) (_, expr)) = "@" <> ann <> line <> expr
printExprAlg a = pretty $ show a

printOp Equals = "=="
printOp IsType = "is"
printOp And = "&&"
printOp To = "to"
printOp Add = "+"

printWhenCases (WhenCase conds body) = group (joinWith "&&" (ensureOneElement conds) <+> "->") <+> body
   where 
      ensureOneElement [] = ["true"]
      ensureOneElement a = a
printWhenCases (ElseCase body) = "else ->" <+> body