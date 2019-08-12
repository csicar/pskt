module CodeGen.CoreImp where

import Prelude hiding (print)
import Data.Text (Text)
import Data.Maybe (catMaybes, fromMaybe, Maybe(..))
import Data.List (nub)
import Debug.Trace (trace)
import Language.PureScript.CoreFn.Expr
import Language.PureScript.CoreFn.Module
import Language.PureScript.CoreFn.Meta
import Language.PureScript.CoreFn.Ann
import Language.PureScript.AST.Literals
import Language.PureScript.Names
import Language.PureScript.CoreFn.Traversals
import Language.PureScript.PSString (prettyPrintStringJS, PSString)
import Data.Text.Prettyprint.Doc


class PrintKt a where
   print :: a -> Doc ann

instance PrintKt PSString where
   print = pretty . prettyPrintStringJS

instance (PrintKt ann) => PrintKt (Literal ann)  where
   print (NumericLiteral (Left a)) = pretty a
   print (NumericLiteral (Right a)) = pretty a
   print (StringLiteral psString) = print psString
   print (CharLiteral char) = squotes $ pretty char
   print (BooleanLiteral True) = "true"
   print (BooleanLiteral False) = "false"
   print (ArrayLiteral arr) = "arrayOf" <> parens (concatWith (\a b -> a <> "," <+> b) $ print <$> arr)
   print (ObjectLiteral attrs) = "mapOf" <> parens (concatWith (\a b -> a <> "," <+> b) $ printAssoc <$> attrs)
      where printAssoc (psString, val) = print psString <+> "to" <+> print val

instance PrintKt ModuleName where
   print mod = pretty $ runModuleName mod

instance PrintKt a => PrintKt (Qualified a) where
   print (Qualified Nothing ident) = print ident
   print (Qualified (Just mod) ident) = print mod <> "." <> print ident
   
castFunction :: Doc a -> Doc a
castFunction expr = parens $ (parens expr) <+> "as (Any) -> Any"

castMap :: Doc a -> Doc a
castMap expr = parens $ (parens expr) <+> "as May<Any, Any>"

nest' = nest 4

printTy :: ProperName TypeName -> Doc a
printTy name = "sealed class" <+> pretty (runProperName name)


instance PrintKt (Expr Ann) where
   print (Literal _ lit) = print lit
   print (Var _ qualName) = print qualName
   print (Abs _ arg body) = vsep
      [ nest' $ vsep 
         [  "{" <+> print arg <+> ": Any ->"
         ,  print body
         ]
      , "}"
      ]
   -- Constructor Call
   print (App _ left@(Var (_, _, _, Just (IsConstructor _ _)) _) right) = (print left) <> parens (print right) 
   print (App _ left right) = castFunction (print left) <> parens (print right)
   print (Accessor _ loc val) = castMap (print val) <> brackets (print loc)

printDecl _ (Constructor _ tyName ctorName args) = Just $ "data class" <+> (pretty (runProperName ctorName))
   <> parens (vsep (map printConstructorArg args))
   <+> ":" <+> (pretty $ runProperName tyName) <> "()"
   where
      printConstructorArg arg = "val" <+> print arg <> ": Any"
-- printDecl ident (Abs _ arg body) = Just $ vsep
--       [ nest' $ vsep
--          [ "fun" <+> print ident <> parens (print arg <> ": Any") <> ": Any {"
--          ,  "return" <+> print body
--          ]
--       , "}"
--       ]
printDecl ident a = Nothing


instance PrintKt Ident where
   print = pretty . runIdent

instance PrintKt (Bind Ann) where
   print (NonRec _ ident val) = "val" <+> print ident <+> "=" <+> nest' (print val <> line)

printTopLevelBind bind@(NonRec _ ident val) = fromMaybe (print bind) (printDecl ident val)

collectTypeDecls :: [Bind a] -> [ProperName TypeName]
collectTypeDecls list = catMaybes $ nub $ map go list
   where
      go (NonRec _ _ (Constructor _ tyName _ _)) = Just tyName
      go _ = Nothing

printForeignImport :: ModuleName -> Ident -> Doc a
printForeignImport modName ident = "import Foreign." <> (print modName) <> "." <> (print ident) <+> "as" <+> (print ident)

instance PrintKt (Module Ann) where
   print mod = vsep
      [ header
      , line
      , foreignImports
      , line
      , vsep $ printTy <$> typeDecls
      , line
      , body
      ]
      where
         header = "package" <+> (print $ moduleName mod) <> ";"
         foreignImports = vsep $ printForeignImport (moduleName mod) <$> (moduleForeign mod)
         typeDecls = collectTypeDecls (moduleDecls mod)
         body = vsep $ printTopLevelBind <$> (moduleDecls mod)

moduleToText :: Module Ann -> SimpleDocStream ann
moduleToText = layoutPretty defaultLayoutOptions . print