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
import Language.PureScript.CoreFn.Binders
import Language.PureScript.PSString (prettyPrintStringJS, PSString)
import Data.Text.Prettyprint.Doc
import Language.PureScript.AST.SourcePos (displayStartEndPos)

data Env a = Env {mod :: Module a}

class PrintKt a where
   print :: Env Ann -> a -> Doc ann

instance PrintKt PSString where
   print env = pretty . prettyPrintStringJS

instance (PrintKt ann) => PrintKt (Literal ann)  where
   print env (NumericLiteral (Left a)) = pretty a
   print env (NumericLiteral (Right a)) = pretty a
   print env (StringLiteral psString) = print env psString
   print env (CharLiteral char) = squotes $ pretty char
   print env (BooleanLiteral True) = "true"
   print env (BooleanLiteral False) = "false"
   print env (ArrayLiteral arr) = "listOf" <> parens (concatWith (\a b -> a <> "," <+> b) $ print env <$> arr)
   print env (ObjectLiteral attrs) = "mapOf" <> parens (concatWith (\a b -> a <> "," <+> b) $ printAssoc <$> attrs)
      where printAssoc (psString, val) = print env psString <+> "to" <+> print env val

instance PrintKt ModuleName where
   print env mod = pretty $ runModuleName mod

instance PrintKt a => PrintKt (Qualified a) where
   print env (Qualified Nothing ident) = print env ident
   print env@(Env envMod) (Qualified (Just mod) ident) | moduleName envMod == mod = print env ident
   print env (Qualified (Just mod) ident) = print env mod <> "." <> print env ident
   
castFunction :: Doc a -> Doc a
castFunction expr = "fn" <> parens expr
-- castFunction expr = parens $ (parens expr) <+> "as (Any) -> Any"

castMap :: Doc a -> Doc a
castMap expr = parens $ (parens expr) <+> "as Map<Any, Any>"

nest' = nest 4

printTy :: ProperName TypeName -> Doc a
printTy name = "sealed class" <+> pretty (runProperName name)

instance PrintKt (Binder Ann) where
   -- true -> ...
   print env (NullBinder _) = "_"
   print env (LiteralBinder _ lit) = print env lit
   print env (VarBinder a ident) = print env ident

instance PrintKt (CaseAlternative Ann) where
   print env (CaseAlternative [bind] (Right expr)) = print env bind <+> "->" <+> print env expr


instance PrintKt (Expr Ann) where
   print env (Literal _ lit) = print env lit
   print env (Var _ qualName) = print env qualName
   print env (Abs _ arg body) = vsep
      [ nest' $ vsep 
         [  "{" <+> print env arg <+> ": Any ->"
         ,  print env body
         ]
      , "}"
      ]
   -- Constructor Call
   print env (App _ left@(Var (_, _, _, Just (IsConstructor _ _)) _) right) = (print env left) <> parens (print env right) 
   print env (App _ left right) = castFunction (print env left) <> parens (print env right)
   print env (Accessor _ loc val) = castMap (print env val) <> brackets (print env loc)
   print env (Case (sp, _, _, _) expr cases) = vsep
      [ nest' $ vsep
         [ "when" <> binders <+> "{"
         , vsep $ addElse $ print env <$> cases
         ]
      , "}"
      ]
         where
            binders = case expr of
               [] -> ""
               [expr] -> parens $ print env expr
            showModule (Env mod) = print env $ moduleName mod
            addElse x = x ++ ["else ->" <+> "throw IllegalArgumentException(\"Failed pattern match in module" <> showModule env <+> pretty (displayStartEndPos sp) <> "\")"]


printDecl env _ (Constructor _ tyName ctorName args) = Just $ "data class" <+> (pretty (runProperName ctorName))
   <> parens (vsep (map printConstructorArg args))
   <+> ":" <+> (pretty $ runProperName tyName) <> "()"
   where
      printConstructorArg arg = "val" <+> print env arg <> ": Any"
-- printDecl env ident (Abs _ arg body) = Just $ vsep
--       [ nest' $ vsep
--          [ "fun" <+> print env ident <> parens (print env arg <> ": Any") <> ": Any {"
--          ,  "return" <+> print env body
--          ]
--       , "}"
--       ]
printDecl env ident a = Just $ "@JvmField" <> line <> "val"<+> print env ident <+> "=" <+> nest' (print env a <> line)


instance PrintKt Ident where
   print env = pretty . runIdent

instance PrintKt (Bind Ann) where
   print env (NonRec _ ident val) = "val" <+> print env ident <+> "=" <+> nest' (print env val <> line)

printTopLevelBind env bind@(NonRec _ ident val) = fromMaybe (print env bind) (printDecl env ident val)

collectTypeDecls :: [Bind a] -> [ProperName TypeName]
collectTypeDecls list = catMaybes $ nub $ map go list
   where
      go (NonRec _ _ (Constructor _ tyName _ _)) = Just tyName
      go _ = Nothing

printForeignImport :: Env Ann -> ModuleName -> Ident -> Doc a
printForeignImport env modName ident = "import Foreign." <> (print env modName) <> "." <> (print env ident) <+> "as" <+> (print env ident)

instance PrintKt (Module Ann) where
   print env mod = vsep
      [ header
      , ""
      , foreignImports
      , ""
      , "fun fn(o: Any):(Any) -> Any {return (o as (Any) -> Any)}"
      , object
      , ""
      ]
      where
         header = "package" <+> (print env $ moduleName mod) <> ";"
         foreignImports = vsep $ printForeignImport env (moduleName mod) <$> (moduleForeign mod)
         typeDecls = collectTypeDecls (moduleDecls mod)
         body = vsep $ printTopLevelBind env <$> (moduleDecls mod)
         object = vsep 
            [ nest' $ vsep
               ["object Module {"
               , vsep $ printTy <$> typeDecls
               , body
               ]
            , "}"
            ]

moduleToText :: Module Ann -> SimpleDocStream ann
moduleToText mod = layoutPretty defaultLayoutOptions $ print (Env mod) mod