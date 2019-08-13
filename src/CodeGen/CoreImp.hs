module CodeGen.CoreImp where
--- Convert CoreFn to KtCore
import Prelude hiding (print)
import Data.Text (Text)
import Data.Maybe (catMaybes, fromMaybe, Maybe(..))
import Data.List (nub)
import Debug.Trace (trace)
import Language.PureScript.CoreFn.Expr
import Control.Monad.Supply.Class (MonadSupply, fresh)
import Control.Monad (forM, replicateM, void)
import Language.PureScript.CoreFn.Module
import Language.PureScript.CoreFn.Meta
import Language.PureScript.CoreFn.Ann
import Control.Monad.Supply
import Language.PureScript.AST.Literals
import Language.PureScript.Names
import Language.PureScript.CoreFn.Traversals
import Language.PureScript.CoreFn.Binders
import Language.PureScript.PSString (prettyPrintStringJS, PSString)
import Data.Text.Prettyprint.Doc
import Text.Pretty.Simple (pShow)
import Language.PureScript.AST.SourcePos (displayStartEndPos)
import CodeGen.KtCore

instance PrintKt PSString where
   print env = pretty . prettyPrintStringJS

instance (PrintKt ann) => PrintKt (Literal ann)  where
   print env (NumericLiteral (Left a)) = pretty a
   print env (NumericLiteral (Right a)) = pretty a
   print env (StringLiteral psString) = print env psString
   print env (CharLiteral char) = squotes $ pretty char
   print env (BooleanLiteral True) = "true"
   print env (BooleanLiteral False) = "false"
   print env (ArrayLiteral arr) = "listOf" <> parens (commaSep $ print env <$> arr)
   print env (ObjectLiteral attrs) = "mapOf" <> parens (commaSep $ printAssoc <$> attrs)
      where printAssoc (psString, val) = print env psString <+> "to" <+> print env val

castFunction :: Doc a -> Doc a
castFunction expr = "fn" <> parens expr
-- castFunction expr = parens $ (parens expr) <+> "as (Any) -> Any"

castMap :: Doc a -> Doc a
castMap expr = parens $ (parens expr) <+> "as Map<Any, Any>"



printTypeDecl :: ProperName TypeName -> Doc a
printTypeDecl name = "sealed class" <+> pretty (runProperName name) <+> ": Destructurable"

instance PrintKt (Binder Ann) where
   print env (NullBinder _) = "_"
   print env (LiteralBinder _ lit) = print env lit
   print env (VarBinder a ident) = print env ident
   print env (ConstructorBinder _ ty ctor binders) = parens $ commaSep $ print env <$> binders


generateBinder :: forall m. (Monad m, MonadSupply m) => Env Ann -> Ident -> Binder Ann -> m [KtExpr]
generateBinder env valName (ConstructorBinder a tyName ctorName binders) = do
    generatedBinders <- forM binders $ \binder -> do
      binderName <- freshIdent'
      decls <- generateBinder env binderName binder
      return (binderName, decls)
    let decls = snd <$> generatedBinders
    let binderNames = fst <$> generatedBinders
    return $ [Destructuring binderNames (Cast (VarRef valName) (VarRef (Ident "Destructurable")))] ++ concat decls
generateBinder env valName (VarBinder a ident) = return [VariableIntroduction ident (VarRef valName)]
-- f "ads" = 2
-- f __1 =
--    if __1 == asd then 2 
generateBinder env valName (LiteralBinder _ lit) =
  return [ If (Binary Equals (VarRef valName) (CoreImp $ pretty $ show lit)) (VarRef valName) Nothing]



instance PrintKt (Expr Ann) where
   print env (Literal _ lit) = print env lit
   print env (Var (_, _, _, Just (IsConstructor _ _)) qualName) = print env qualName <> ".create"
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
   print env a@(Case (sp, _, _, _) expr cases) = print env $ WhenExpr (addElse $ concat $ caseForExpr <$> cases)
      where
         caseForExpr (CaseAlternative [binder] (Right thenValue)) =
            [WhenCase (zipWith compareBinder [binder] expr) (vsep (print env <$> binderDoc) <> line <> print env thenValue)]
            where
               exprToIdent [(Var _ (Qualified _ ident))] = ident
               binderDoc = evalSupply 0 $ generateBinder env (exprToIdent expr) binder
         caseForExpr (CaseAlternative binders (Left guardedValues)) = [WhenCase [] ""] 
         compareBinder :: Binder Ann -> Expr Ann -> Doc ()
         compareBinder (NullBinder _) compareVal = "true"
         compareBinder (ConstructorBinder _ ty ctor ctorArgs) compareVal =
            (print env compareVal) <+> "is" <+> print env ctor
         compareBinder (LiteralBinder _ lit) compareVal =
            (print env compareVal) <+> "==" <+> (print env lit)
         compareBinder (VarBinder a ident) compareVal = 
            print env compareVal <+> "==" <+> print env ident
         showModule (Env mod) = print env $ moduleName mod
         addElse x = x ++ 
            [ WhenCase ["else"]
               ( "throw IllegalArgumentException(\"Failed pattern match in module" <+> showModule env <+> pretty (displayStartEndPos sp) <> "\")"
               )
            ]
   print env (Let _ binds inExpr) = braces $ vsep $ (print env <$> binds) ++ [print env inExpr]
   print env a = trace (show a) ""

data Declaration inModule inTopLevel = Declaration {runInModule:: inModule, runInTopLevel:: inTopLevel}
   -- put decl in object Module { ... }
   -- put decl at toplevel of file
justInModule a = Declaration [a] [] 
justInTopLevel a = Declaration [] [a]

printDecl env _ (Constructor _ tyName ctorName args) = justInTopLevel $ "data class" <+> printedCtorName
   <> parens (commaSep $ addParameterIfEmpty $ printConstructorArg <$> args)
   <+> ":" <+> (pretty $ runProperName tyName) <> "() {" <+> curriedConstructor <+> "}"
   where
      printedCtorName = (pretty (runProperName ctorName))
      -- required because "data class Ctor()" is not valid in newer versions of kotlin
      addParameterIfEmpty [] = ["val empty : Boolean = true"]
      addParameterIfEmpty args = args
      printConstructorArg arg = "val" <+> print env arg <> ": Any"
      curriedConstructor = "companion object" <+> braceNested 
         ( "val create = " <+> 
            ( curriedConstructorArgs (printedCtorName <> (parens $ commaSep $ print env <$> args)) args
            )
         )
      curriedConstructorArgs body [] = body
      curriedConstructorArgs body (arg:args) = braceNested $ print env arg <+> ": Any ->" <+> (curriedConstructorArgs body args)
printDecl env ident a = justInModule $ "@JvmField" <> line <> "val"<+> print env ident <+> "=" <+> nest' (print env a <> line)



instance PrintKt (Bind Ann) where
   print env (NonRec _ ident val) = "val" <+> print env ident <+> "=" <+> nest' (print env val <> line)

printTopLevelBind env bind@(NonRec _ ident val) = (printDecl env ident val)

collectTypeDecls :: [Bind a] -> [ProperName TypeName]
collectTypeDecls list = catMaybes $ nub $ map go list
   where
      go (NonRec _ _ (Constructor _ tyName _ _)) = Just tyName
      go _ = Nothing

printForeignImport :: Env Ann -> ModuleName -> Ident -> Doc ()
printForeignImport env modName ident = "import Foreign." <> (print env modName) <> "." <> (print env ident) <+> "as" <+> (print env ident)

instance PrintKt (Module Ann) where
   print env mod = vsep
      [ header
      , ""
      , foreignImports
      , ""
      , "fun fn(o: Any):(Any) -> Any {return (o as (Any) -> Any)}"
      , printInterfaceDestructurable
      , ""
      , vsep $ printTypeDecl <$> typeDecls
      , topLevelDecls
      , ""
      , object
      , ""
      ]
      where
         header = "package" <+> (print env $ moduleName mod) <> ";"
         foreignImports = vsep $ printForeignImport env (moduleName mod) <$> (moduleForeign mod)
         typeDecls = collectTypeDecls (moduleDecls mod)
         declarations = printTopLevelBind env <$> (moduleDecls mod)
         body = vsep $ concat $ runInModule <$> declarations
         topLevelDecls = vsep $ concat $ runInTopLevel <$> declarations
         object = vsep 
            [ nest' $ vsep
               ["object Module {"
               
               , body
               ]
            , "}"
            ]

printInterfaceDestructurable =
   "interface Destructurable " <> braceNested
      ( vsep ["operator fun component"<> (pretty (n :: Integer)) <> "() : Any {throw Exception(\"unimplemented\")}" | n <- [0..10]]
      )
moduleToText :: Module Ann -> SimpleDocStream ()
moduleToText mod = layoutPretty defaultLayoutOptions $ print (Env mod) mod