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

-- val main = PS.Control.Bind.Module.discard
--                .app(PS.Control.Bind.Module.discardUnit)
--                .app(PS.Effect.Module.bindEffect)
--                .app(PS.Effect.Console.Module.log.app("asd"))
--                .app({ _ : Any ->
--        PS.Control.Bind.Module.discard.app(PS.Control.Bind.Module.discardUnit)
--          .app(PS.Effect.Module.bindEffect)
--          .app(PS.Effect.Console.Module.log.app("kkk"))
--          .app({ _ : Any ->
--            1
--         })
--     });
-- log.app("asd")
-- log.app("kkk")

pattern QualRef a b = VarRef (Qualified (Just a) (MkKtIdent b))

magicDoEffect :: KtExpr -> KtExpr
magicDoEffect = cata alg where
  alg :: KtExprF KtExpr -> KtExpr
  -- PS.Control.Applicative.Module.pure
  --              .app(PS.Effect.Module.applicativeEffect)
  --              .app(<a>);
  -- ==> <a>
  alg (CallAppF (CallApp (QualRef Applicative "pure") (QualRef Effect "applicativeEffect")) a) = a
  -- PS.Control.Bind.Module.discard
  --              .app(PS.Control.Bind.Module.discardUnit)
  --              .app(PS.Effect.Module.bindEffect)
  --              .app(<val>)
  --              .app({ _ : Any -> <body> })
  -- ==> <val>; <body>
  alg (CallAppF (CallApp 
    (CallApp 
      (CallApp (QualRef Bind "discard") (QualRef Bind "discardUnit")) 
      (QualRef Effect "bindEffect")
    ) val) (Lambda (MkKtIdent "_") body)) = Stmt [val, body]
  -- PS.Control.Bind.Module.bind
  --   .app(PS.Effect.Module.bindEffect)
  --   .app(<val>)
  --   .app({ <arg> : Any -> <body> })
  -- ==> val <arg> = <val>; <body>
  alg (CallAppF (CallApp (CallApp (QualRef Bind "bind") (QualRef Effect "bindEffect")) val) (Lambda arg body)) = 
    Stmt [ VariableIntroduction arg val, body]
  
  alg other = embed other