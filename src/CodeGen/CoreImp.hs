module CodeGen.CoreImp where
--- Convert CoreFn to KtCore
import Prelude (undefined, error)
import Protolude hiding (Const, const, moduleName, undefined)
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
import CodeGen.Transformations
import Data.Functor.Foldable (cata, embed)


moduleToKt' mod = evalSupply 0 (moduleToKt mod)


data DataTypeDecl = DataTypeDecl
   { typeName :: ProperName TypeName
   , constructors :: [DataCtorDecl]
   }

data DataCtorDecl = DataCtorDecl
   { ctorName :: ProperName ConstructorName
   , parameter :: [Ident]
   }

data Replacement = Replacement
   { ident :: KtIdent
   , replacement :: KtExpr
   }

moduleToKt :: MonadSupply m => Module Ann -> m [KtExpr]
moduleToKt mod = sequence
   [ pure $ packageDecl (moduleName mod)
   , pure $ Import [ProperName "Foreign", ProperName "PsRuntime"] (MkKtIdent "app")
   , pure $ Import [ProperName "Foreign", ProperName "PsRuntime"] (MkKtIdent "appRun")
   , normalize <$> moduleToObject mod
   ]
   where
      packageDecl :: ModuleName -> KtExpr
      packageDecl (ModuleName mn) = Package $ psNamespace : mn

      moduleToObject :: MonadSupply m => Module Ann -> m KtExpr
      moduleToObject mod = do
         let (normalDecls, classDecls) = splitDeclarations (moduleDecls mod)
         foreigns <- foreignToKt `mapM` moduleForeign mod
         decls <- mapM classDeclsToKt classDecls
         body <- mapM (bindToKt ktJvmValue) normalDecls 
         let objectName = MkKtIdent "Module"
         return $ ObjectDecl (Just objectName) [] $ Stmt $ foreigns ++ concat decls ++ concatMap (\(normalDefs, _, recDefs) -> normalDefs ++ recDefs) body

      foreignToKt :: MonadSupply m => Ident -> m KtExpr
      foreignToKt ident = do
         ktIdent <- ktIdentFromIdent ident
         let foreignModule = let (ModuleName pns) = moduleName mod in ModuleName $ ProperName "Foreign" : pns
         pure $ VariableIntroduction ktIdent $ VarRef (Qualified (Just foreignModule) ktIdent)

      classDeclsToKt :: MonadSupply m => DataTypeDecl -> m [KtExpr]
      classDeclsToKt (DataTypeDecl tyName constructors) = do
         ktName <- identFromTypeName tyName
         ktCtors <- mapM (ctorToKt (varRefUnqual ktName)) constructors
         let classDecl = ClassDecl [Sealed] ktName [] [] $ Stmt (fst <$> ktCtors)
         return $ classDecl : (snd <$> ktCtors)

      ctorToKt :: MonadSupply m => KtExpr -> DataCtorDecl -> m (KtExpr, KtExpr)
      ctorToKt parentName (DataCtorDecl ctorName param) = do
         let parentRef = Call parentName []
         ktParam <- mapM ktIdentFromIdent param 
         ktName <- identFromCtorName ctorName
         return $
            case param of
               [] ->
                  ( ObjectDecl (Just ktName) [parentRef] (Stmt [])
                  , VariableIntroduction ktName (Property parentName (varRefUnqual ktName))
                  )
               _ -> 
                  ( ClassDecl [Data] ktName ktParam [parentRef] (Stmt [])
                  , VariableIntroduction ktName (lambdaFor ktName [] ktParam)
                  )
         where 
            lambdaFor ktName ktParam [] = Call (Property parentName (varRefUnqual ktName)) (varRefUnqual <$> ktParam)
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

      bindToKt :: MonadSupply m => (KtExpr -> KtExpr) -> Bind Ann -> m ([KtExpr], [KtIdent], [KtExpr]) -- (normalDef, normalRef, recursive)
      --TODO: split binder into (Constructor ...) and others
      bindToKt modDecls (NonRec _ ident val) = do
            ktVal <- exprToKt val
            ktIdent <- ktIdentFromIdent ident
            return ([ modDecls $ VariableIntroduction ktIdent ktVal ], [ktIdent], [])
      bindToKt modDecls (Rec bindings) = do
         converted <- mapM go bindings
         let names = (\((normalName, _), (recursiveName, _)) -> (normalName, recursiveName)) <$> converted
         let normalDecls = snd . fst <$> converted
         let recursiveDecls = snd . snd <$> converted

         pure (replaceRecNames names <$> normalDecls, fst . fst <$> converted, replaceRecNames names <$> recursiveDecls)
         where
            genRecTxt name = "_rec_"<> name
            genRecName (MkKtIdent name) = MkKtIdent $ "_" <> genRecTxt name
            replaceRecNames :: [(KtIdent, KtIdent)] -> KtExpr -> KtExpr
            replaceRecNames replacements expr = foldr replaceRecName expr replacements
            replaceRecName :: (KtIdent, KtIdent) -> KtExpr -> KtExpr
            replaceRecName (original, new) = cata alg where
               alg (VarRefF (Qualified modName' name)) 
                  | (name == original) && maybe True (== moduleName mod) modName' = 
                     Call (varRefUnqual new) []
               alg a = embed a
            go :: MonadSupply m => ((a, Ident), Expr Ann) -> m ((KtIdent, KtExpr), (KtIdent, KtExpr)) -- ((normalName, normalVal), (recName, recValue))
            -- (decls, (normalIdent, recIdent))
            go ((_, ident), val) = do
               ktVal <- exprToKt val
               ktIdent <- ktIdentFromIdent ident
               let recFuncName = genRecName ktIdent
               let normalVar = modDecls $ VariableIntroduction ktIdent $ Call (FunRef (Qualified Nothing recFuncName)) []
               return ((ktIdent, normalVar), (recFuncName, Fun (Just recFuncName) [] ktVal))
               -- return ([ ktFun' (Just recFuncName) [] ktVal, normalVar ], (ktIdent, recFuncName))
            -- recursion with anything but a abs
            -- for this, the value is turned into a argumentless function and called to get the value
            -- go ((_, ident), a) = return $ pTraceShow bindings undefined

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
         return $ Call (Property aKt (varRefUnqual $ MkKtIdent "app")) [bKt]
      exprToKt (Case _ compareVals caseAlternatives) = WhenExpr . concat <$> mapM (caseToKt compareVals) caseAlternatives
      exprToKt (Accessor _ key obj) = do
         ktObj <- exprToKt obj
         return $ ObjectAccess (Cast ktObj $ varRefUnqual mapType) (ktString key)
      exprToKt (Let _ binds body) = do
         ktBinds <- mapM (bindToKt identity) binds 
         let normalBinds = concatMap (\(normalBind, _, _) -> normalBind) ktBinds
         let normalRefs = concatMap (\(_, normalName, _) -> normalName) ktBinds
         let recBinds = concatMap (\(_, _, recBind) -> recBind) ktBinds
         ktBody <- exprToKt body
         let ktObj = ObjectDecl Nothing [] $ Stmt (normalBinds ++ recBinds)
         return $ Call (Property ktObj (varRefUnqual $ MkKtIdent "run")) 
            [ Stmt $ 
               ((\normalRef -> VariableIntroduction normalRef $ Property (varRefUnqual $ MkKtIdent "this") (varRefUnqual normalRef)) <$> normalRefs)
               ++ [ktBody]
            ]--(ktStmt $ ktBinds ++ [ktBody]) [] -- TODO: limit to situations where wrapping in call is necessary
      exprToKt a = pTraceShow a undefined
      
      caseToKt :: MonadSupply m => [Expr Ann] -> CaseAlternative Ann -> m [WhenCase KtExpr]
      caseToKt compareVals (CaseAlternative binders caseResult) = do
         ktCompareVals <- mapM exprToKt compareVals
         (guards, replacements) <- transposeTuple <$> zipWithM binderToKt ktCompareVals binders
         let assignments = replacementToAssignment <$> concat replacements
         case caseResult of
            (Right result) -> do
               ktBody <- exprToKt result
               pure [WhenCase (concat guards) (Stmt $ assignments ++ [ktBody])]
            (Left guardedExpr) -> traverse genGuard guardedExpr
               where 
                  genGuard (cond, val) = do
                     ktCond <- ktAsBool . replaceBindersWithReferences (concat replacements) <$> exprToKt cond
                     ktVal <- exprToKt val
                     pure $ WhenCase (concat guards ++ [ktCond]) (Stmt $ assignments ++ [ktVal])

      replaceBindersWithReferences :: [Replacement] -> KtExpr -> KtExpr
      replaceBindersWithReferences replacements expr = foldr (cata . alg) expr replacements
         where
            alg (Replacement ident ref) (VarRefF (Qualified Nothing ident')) | ident == ident' = ref
            alg _ a = embed a

      binderToKt :: MonadSupply m => KtExpr -> Binder Ann -> m ([KtExpr], [Replacement]) -- ([binder], [{identToReplace, exprThatReplacesIt}])
      binderToKt compareVal (VarBinder _ ident) = do
         ktIdent <- ktIdentFromIdent ident
         pure 
            ( []
            , [Replacement ktIdent compareVal]
            )
      binderToKt compareVal (LiteralBinder _ literal) = do
         literalValue <- forMLitKey literal
            (binderToKt . ArrayAccess compareVal . ktInt)
            (binderToKt . ObjectAccess compareVal . ktString)
         pure 
            ( specificGuard literal : fold (fst <$> literalValue)
            , fold (snd <$> literalValue)
            )
         where
            specificGuard (ArrayLiteral a) = 
               Binary Equals (getLength compareVal) (ktInt $ fromIntegral $ length a)
            specificGuard (ObjectLiteral a) = 
               Binary Equals (getEntryCount compareVal) (ktInt $ fromIntegral $ length a)
            specificGuard (NumericLiteral a) = Binary Equals compareVal $ Const $ NumericLiteral a
            specificGuard (StringLiteral a) = Binary Equals compareVal $ Const $ StringLiteral a
            specificGuard (CharLiteral a) = Binary Equals compareVal $ Const $ CharLiteral a
            specificGuard (BooleanLiteral a) = Binary Equals compareVal $ Const $ BooleanLiteral a
      binderToKt _ NullBinder{} = pure ([], [])
      binderToKt compareVal (ConstructorBinder (_, _, _, Just (IsConstructor _ ctorParams)) tyName ctorName subBinders) = do
         ktTypeIdent <- qualifiedToKt identFromTypeName tyName
         (Qualified _ ktCtorName) <- qualifiedToKt identFromCtorName ctorName
         ktCtorParams <- mapM ktIdentFromIdent ctorParams
         subBindersExprs <- zipWithM (\ident binder -> binderToKt (Property compareVal (varRefUnqual ident)) binder) ktCtorParams subBinders
         pure
            ( Binary IsType compareVal (Property (VarRef ktTypeIdent) (varRefUnqual ktCtorName)) : concat (fst <$> subBindersExprs)
            , concat $ snd <$> subBindersExprs
            )
      binderToKt compareVal (ConstructorBinder (_, _, _, Just IsNewtype) tyName ctorName [subBinder]) = do
         (guards, stmts) <- binderToKt compareVal subBinder 
         pure
            ( guards
            , stmts
            )
      binderToKt compareVal (NamedBinder _ ident subBinder) = do
         ktIdent <- ktIdentFromIdent ident
         (guards, replacements) <- binderToKt compareVal subBinder
         pure 
            ( guards
            , Replacement ktIdent compareVal : replacements
            )
      binderToKt compareVal binder = pure $ pTraceShow (compareVal, binder) undefined

      replacementToAssignment :: Replacement -> KtExpr
      replacementToAssignment (Replacement ident val) = VariableIntroduction ident val

transposeTuple :: [(a, b)] -> ([a], [b])
transposeTuple ls = (fst <$> ls, snd <$> ls)

splitLast :: a -> [a] -> ([a], a)
splitLast pre [] = ([pre], pre)
splitLast pre [l] = ([pre], l)
splitLast pre (l:ls) = (\(a, b) -> (pre :a, b)) $ splitLast l ls