module CodeGen.Transformations where
import Prelude (undefined)
import Protolude hiding (Const, moduleName, undefined)
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
import CodeGen.MagicDo
import Data.Functor.Foldable
import Data.Maybe  (fromJust)
import qualified Language.PureScript.Constants as C

normalize :: KtExpr -> KtExpr
normalize = identity
  . removeDoubleStmt
  . primUndefToUnit
  . addElseCases
  . removeDoubleStmt
  . inlineDeferApp
  -- poor man's fixpoint
  . magicDoEffect
  . magicDoEffect
  . magicDoEffect
  . magicDoEffect
  . magicDoEffect
  . magicDoEffect
  . magicDoEffect
  . magicDoEffect
  . magicDoEffect
  . magicDoEffect
  . magicDoEffect
  . magicDoEffect
  . magicDoEffect
  . magicDoEffect
  . magicDoEffect
  . magicDoEffect
  . magicDoEffect
  . magicDoEffect
  . magicDoEffect
  . magicDoEffect
  . magicDoEffect
  . magicDoEffect
  . magicDoEffect
  . magicDoEffect
  . magicDoEffect
  . magicDoEffect
  . magicDoEffect
  . magicDoEffect
  . magicDoEffect
  . magicDoEffect
  . magicDoEffect
  . magicDoEffect
  . magicDoEffect
  . magicDoEffect
  . magicDoEffect
  . removeDoubleStmt
  . removeUnnecessaryWhen
  . addElseCases
  . inline

removeDoubleStmt :: KtExpr -> KtExpr
removeDoubleStmt = cata alg where
  alg :: KtExprF KtExpr -> KtExpr
  alg (StmtF ls) = Stmt $ concatMap extract ls
  alg a = embed a

  extract (Stmt ls) = ls
  extract a = [a]

removeUnnecessaryWhen :: KtExpr -> KtExpr
removeUnnecessaryWhen = cata alg where
  alg :: KtExprF KtExpr -> KtExpr
  alg (WhenExprF [ElseCase a]) = a
  alg a = embed a

-- {<stmt>; /*defer */{<body>}.appRun()} -> {<stmt>; <body>}
inlineDeferApp :: KtExpr -> KtExpr
inlineDeferApp = cata alg where
  alg :: KtExprF KtExpr -> KtExpr
  alg (StmtF sts) = Stmt $ go <$> sts
    where
      go (Run (Defer a)) = a
      go (Run (Stmt body)) = inlineDeferApp $ Stmt $ mapLast Run body
      go a = a
  alg a = embed a

mapLast :: (a -> a) -> [a] -> [a]
mapLast f ls = reverse $ case reverse ls of
  [] -> []
  (end: rest) -> f end : rest

-- If a when case does not cover all cases, an else branch is needed
-- since Kotlin can sometimes not infer, that all cases are covered, this adds a default case
addElseCases :: KtExpr -> KtExpr
addElseCases = cata alg where
  alg :: KtExprF KtExpr -> KtExpr
  alg (WhenExprF cases) = WhenExpr $ reverse $ case reverse cases of
      [] -> []
      (WhenCase [] b : cs) -> ElseCase b : cs
      cs -> ElseCase errorMsg : cs
      where 
        errorMsg = ktAsAny (Call (varRefUnqual $ MkKtIdent "error") [ktString "Error in Pattern Match"])
  alg a = embed a

-- <mod>.<f>.app(<mod>.<cls>).app(<a>).app(<b>)
pattern CallOn2 mod cls mod' f a b = 
  (CallAppF
    ((CallApp
      ((CallApp
        ((VarRef (Qualified (Just mod') f)))
        ((VarRef (Qualified (Just mod) cls)))
      ))
      a
    ))
    b
  )

inline :: KtExpr -> KtExpr
inline = cata alg where
  alg :: KtExprF KtExpr -> KtExpr
  alg (CallOn2 Semigroup (MkKtIdent "semigroupString") Semigroup (MkKtIdent "append") a b)
    = Binary Add (ktAsString a) (ktAsString b)
  alg (CallOn2 Semigroup (MkKtIdent "semigroupArray") Semigroup (MkKtIdent "append") a b)
    = Binary Add (ktAsList a) (ktAsList b)
  alg a = embed a

-- `Prim.undefined` is used for arguments that are not used by the reciever (from what I can tell)
-- we'll give `Prim.undefined` the value Unit
primUndefToUnit :: KtExpr -> KtExpr
primUndefToUnit = cata alg where
  alg (VarRefF (Qualified (Just PrimModule) (MkKtIdent ident))) | ident == C.undefined = VarRef $ Qualified Nothing (MkKtIdent "Unit")
  alg a = embed a