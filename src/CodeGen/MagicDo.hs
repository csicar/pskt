module CodeGen.MagicDo where
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
import Data.Functor.Foldable
import Data.Maybe  (fromJust)
import qualified Language.PureScript.Constants as C

pattern QualRef a b = VarRef (Qualified (Just a) (MkKtIdent b))

topDown ::(KtExpr -> KtExpr) -> KtExpr -> KtExpr
topDown f xs = embed $ topDown f <$> project (f xs)

rename :: (Text -> Text) -> KtExpr -> KtExpr
rename f = cata alg where
  alg (VarRefF (Qualified nm (MkKtIdent name))) = VarRef (Qualified nm (MkKtIdent $ f name))
  alg a = embed a

-- val main = PS.Control.Bind.Module.discard
--                .app(PS.Control.Bind.Module.discardUnit)
--                .app(PS.Effect.Module.bindEffect)
--                .app(PS.Effect.Console.Module.log.app("asd"))
--                .app({ _ : Any ->
--        PS.Effect.Console.Module.log.app("kkk")
--     })

magicDoEffect :: KtExpr -> KtExpr
magicDoEffect = cata alg where
  alg :: KtExprF KtExpr -> KtExpr

  -- Desugar pure
  -- PS.Control.Applicative.Module.pure
  --              .app(PS.Effect.Module.applicativeEffect)
  --              .app(<a>).appRun();
  -- ==> <a>
  alg (RunF (CallApp (CallApp (QualRef Applicative "pure") (QualRef Effect "applicativeEffect")) a)) = a
  
  -- Desugar discard
  -- PS.Control.Bind.Module.discard
  --              .app(PS.Control.Bind.Module.discardUnit)
  --              .app(PS.Effect.Module.bindEffect)
  --              .app(<val>)
  --              .app({ _ : Any -> <body> })
  -- ==> { <val>.appRun(); <body>.appRun() }
  alg (CallAppF (CallApp 
    (CallApp 
      (CallApp (QualRef Bind "discard") (QualRef Bind "discardUnit")) 
      (QualRef Effect "bindEffect")
    ) 
    val
    ) (Lambda (MkKtIdent "_") body)) =
       Defer $ Stmt [Run val, Run  body]

  -- Desugar bind
  -- PS.Control.Bind.Module.bind
  --   .app(PS.Effect.Module.bindEffect)
  --   .app(<val>)
  --   .app({ <arg> : Any -> <body> })
  -- ==> { val <arg> = <val>.appRun(); <body>.appRun() }
  alg (CallAppF (CallApp 
      (CallApp (QualRef Bind "bind") (QualRef Effect "bindEffect"))
      val
    ) (Lambda arg body)) = 
       Defer $ Stmt [VariableIntroduction arg (Run val), Run body]
  
  -- Desugar untilE
  alg (RunF (CallApp (QualRef Effect "untilE") cond)) =
    Stmt [While (Unary Not (Run cond)) (Stmt []), Unit]

  alg (RunF (CallApp (CallApp (QualRef Effect "whileE") cond) body)) =
    Stmt [While (Run cond) (Stmt [Run body]), Unit]
  
  alg other = embed other

mkDefer (Defer a) = Defer a
mkDefer a = Defer a