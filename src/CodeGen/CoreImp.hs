module CodeGen.CoreImp where
--- Convert CoreFn to KtCore
import Prelude hiding (print)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Maybe (catMaybes, fromMaybe, Maybe(..))
import Data.List (nub)
import Debug.Trace (trace)
import Debug.Pretty.Simple (pTrace, pTraceShow, pTraceShowId)
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


binderToAccessor :: Env Ann -> Binder Ann -> KtExpr -> [KtExpr]
binderToAccessor env (VarBinder _ ident) parent = [VariableIntroduction ident parent]
binderToAccessor env (NamedBinder _ ident binder) parent = [VariableIntroduction ident parent] ++ binderToAccessor env binder parent
binderToAccessor env (ConstructorBinder _ tyName ctorName binder) parent = concat $ zipWith go binder [0..]
   where
      go binder index = binderToAccessor env binder propAccess
         where propAccess = Property (Cast parent (CoreImp $ print env ctorName)) (VarRef $ Ident $ "value" <> T.pack (show index))
binderToAccessor env (LiteralBinder _ (ArrayLiteral binders)) parent = concat $ zipWith go binders [(0 :: Int)..]
   where
      go binder index = binderToAccessor env binder arrayAccess
         where arrayAccess = ArrayAccess parent (CoreImp $ pretty index)
binderToAccessor env (LiteralBinder _ (ObjectLiteral binders)) parent = concat $ map go binders
   where
      go (key, binder) = binderToAccessor env binder objectAccess
         where objectAccess = ObjectAccess parent (CoreImp $ pretty $ show key)
binderToAccessor _ (LiteralBinder _ _) _ = []

binderToCond :: Env Ann -> Binder Ann -> KtExpr -> [KtExpr]
binderToCond env (LiteralBinder _ (StringLiteral psstring)) parentRef = [Binary Equals parentRef (CoreImp $ pretty $ show psstring) ]
binderToCond env (LiteralBinder _ (NumericLiteral (Left int))) parentRef = [Binary Equals parentRef (CoreImp $ pretty int) ]
binderToCond env (LiteralBinder _ (NumericLiteral (Right num))) parentRef = [Binary Equals parentRef (CoreImp $ pretty num) ]
binderToCond env (LiteralBinder _ (BooleanLiteral bool)) parentRef = [Binary Equals (CoreImp $ str) parentRef]
      where str = if bool then "true" else "false"
binderToCond env (LiteralBinder _ (ArrayLiteral binders)) parentRef = 
   [Binary Equals arraySize (CoreImp $ pretty $ length binders)] ++ concat (zipWith go binders [(0 :: Int)..])
   where
      arraySize = Property (Cast parentRef (CoreImp "List<*>")) (CoreImp "size")
      go binder index = binderToCond env binder arrayAccess
         where arrayAccess = ArrayAccess parentRef (CoreImp $ pretty index)
binderToCond env (VarBinder _ _) parentRef = []
binderToCond env (ConstructorBinder _ tyName ctorName binder) compareVal = 
   [Binary IsType compareVal (CoreImp $ print env ctorName)] ++ concat (zipWith go binder [0..])
   where
      go binder index = binderToCond env binder propAccess
         where propAccess = Property (Cast compareVal (CoreImp $ print env ctorName)) (VarRef $ Ident $ "value" <> T.pack (show index))

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
            [WhenCase (zipWith compareBinder [binder] expr) (vsep (print env <$> binderToAccessor env binder (VarRef $ exprToIdent expr)) <> line <> print env thenValue)]
            where
               exprToIdent [(Var _ (Qualified _ ident))] = ident
         caseForExpr (CaseAlternative binders (Left guardedValues)) = [WhenCase [] ""] 
         compareBinder :: Binder Ann -> Expr Ann -> Doc ()
         compareBinder binder compareVal = print env $ foldr (Binary And) (CoreImp "true") (binderToCond env binder (CoreImp $ print env compareVal))
         -- compareBinder (NullBinder _) compareVal = "true"
         -- compareBinder (ConstructorBinder _ ty ctor ctorArgs) compareVal =
         --    (print env compareVal) <+> "is" <+> print env ctor
         -- compareBinder (LiteralBinder _ lit) compareVal =
         --    (print env compareVal) <+> "==" <+> (print env lit)
         -- compareBinder (VarBinder a ident) compareVal = 
         --    print env compareVal <+> "==" <+> print env ident
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