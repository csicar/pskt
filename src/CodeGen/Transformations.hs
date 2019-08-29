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
import Data.Functor.Foldable
import Data.Maybe  (fromJust)

normalize :: KtExpr -> KtExpr
normalize = addElseCases
  . classesEnsureArgument

addElseCases :: KtExpr -> KtExpr
addElseCases = cata alg where
  alg :: KtExprF KtExpr -> KtExpr
  alg (WhenExpr cases) = ktWhenExpr $ reverse $ case reverse cases of
      [] -> []
      (WhenCase [] b : cs) -> ElseCase b : cs
      cs -> cs ++ [ ElseCase errorMsg ]
      where 
        errorMsg = ktCall (varRefUnqual $ MkKtIdent "error") [ktString "Error in Pattern Match"]
  alg a = Fix a

classesEnsureArgument :: KtExpr -> KtExpr
classesEnsureArgument = cata (Fix . alg) where
  alg a = a