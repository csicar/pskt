module CodeGen.CoreImp where
--- Convert CoreFn to KtCore
import Protolude hiding (Const, moduleName)
import Protolude (unsnoc)
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
import Data.Function (on)
import Data.List (partition)
import Language.PureScript.Names
import Language.PureScript.CoreFn.Traversals
import Language.PureScript.CoreFn.Binders
import Language.PureScript.PSString (prettyPrintStringJS, PSString)
import Data.Text.Prettyprint.Doc
import Text.Pretty.Simple (pShow)
import Debug.Pretty.Simple (pTraceShowId, pTraceShow)
import Language.PureScript.AST.SourcePos (displayStartEndPos)
import CodeGen.Constants
import CodeGen.KtCore
import Data.Maybe  (fromJust)

moduleToText = undefined

moduleToKt' mod = evalSupply 0 (moduleToKt mod)


data DataTypeDecl = DataTypeDecl
   { typeName :: ProperName TypeName
   , constructors :: [DataCtorDecl]
   }

data DataCtorDecl = DataCtorDecl
   { ctorName :: ProperName ConstructorName
   , parameter :: [Ident]
   }

moduleToKt :: MonadSupply m => Module Ann -> m KtExpr
moduleToKt mod = Stmt <$> sequence
   [ return $ packageDecl (moduleName mod)
   , moduleToObject mod
   ]
   where
      splitModule (ModuleName parts) = splitLast psNamespace parts

      packageDecl :: ModuleName -> KtExpr
      packageDecl modName = Package 
         $ fst 
         $ splitModule modName

      moduleToObject :: MonadSupply m => Module Ann -> m KtExpr
      moduleToObject mod = do
         let (normalDecls, classDecls) = splitDeclarations (moduleDecls mod)
         decls <- mapM classDeclsToKt classDecls
         body <- mapM bindToKt normalDecls 
         objectName <- identFromNameSpace objectName
         return $ ObjectDecl objectName $ Stmt $ concat decls ++ body
         where
            objectName = snd $ splitModule (moduleName mod)

      classDeclsToKt :: MonadSupply m => DataTypeDecl -> m [KtExpr]
      classDeclsToKt (DataTypeDecl tyName constructors) = do
         ktName <- identFromTypeName tyName
         ktCtors <- mapM (ctorToKt (VarRef $ Qualified Nothing ktName)) constructors
         let classDecl = ClassDecl [Sealed] ktName [] [] $ Stmt (fst <$> ktCtors)
         return $ classDecl : (snd <$> ktCtors)

      ctorToKt :: MonadSupply m => KtExpr -> DataCtorDecl -> m (KtExpr, KtExpr)
      ctorToKt parentName (DataCtorDecl ctorName param) = do
         let parentRef = Call parentName []
         ktParam <- mapM ktIdentFromIdent param 
         ktName <- identFromCtorName ctorName
         return 
            ( ClassDecl [] ktName ktParam [parentRef] (Stmt [])
            , VariableIntroduction ktName (lambdaFor ktName [] ktParam)
            )
         where 
            lambdaFor ktName ktParam [] = Call (Property parentName (VarRef $ Qualified Nothing ktName)) (VarRef . Qualified Nothing <$> ktParam)
            lambdaFor ktName ktParam (l:ls) = Lambda l (lambdaFor ktName (ktParam ++ [l]) ls)

      splitDeclarations :: [Bind Ann] -> ([Bind Ann], [DataTypeDecl])
      splitDeclarations binds = (normalBind, typeDecls)
         where 
            (normalBind, ctorBind) = partition (isNothing . getTypeName) binds
            getTypeName (NonRec _ _ (Constructor _ tyName _ _)) = Just tyName
            getTypeName _ = Nothing
            typeDecls = (\binds -> DataTypeDecl (fromJust $ head binds >>= getTypeName) (groupToDecl <$> binds)) <$> groupBy ((==) `on` getTypeName) ctorBind
            groupToDecl :: Bind Ann -> DataCtorDecl
            groupToDecl (NonRec _ _ (Constructor _ _ ctorName idents)) = DataCtorDecl ctorName idents

      bindToKt :: MonadSupply m => Bind Ann -> m KtExpr
      --TODO: split binder into (Constructor ...) and others
      bindToKt (NonRec _ ident val) = do
            ktVal <- exprToKt val
            ktIdent <- ktIdentFromIdent ident
            return $ VariableIntroduction ktIdent ktVal

      exprToKt :: MonadSupply m => Expr Ann -> m KtExpr
      exprToKt (Var _ qualIdent) = qualifiedIdentToKt qualIdent
      exprToKt (Abs _ arg body) = do 
         ktArg <- ktIdentFromIdent arg
         ktBody <- exprToKt body
         return $ Lambda ktArg ktBody
      exprToKt (Literal _ literal) = Const <$> forMLiteral literal exprToKt
      exprToKt (App _ a b) = do
         aKt <- exprToKt a
         bKt <- exprToKt b
         return $ Call aKt [bKt]

      


splitLast :: a -> [a] -> ([a], a)
splitLast pre [] = ([pre], pre)
splitLast pre [l] = ([pre], l)
splitLast pre (l:ls) = (\(a, b) -> (pre :a, b)) $ splitLast l ls