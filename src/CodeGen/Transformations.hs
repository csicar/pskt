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
import qualified Language.PureScript.Constants as C
import CodeGen.MagicDo

normalize :: KtExpr -> KtExpr
normalize = identity
  . addElseCases
  . primUndefToUnit
  -- . convertToApply
  . inline
  . magicDoEffect

convertToApply :: KtExpr -> KtExpr
convertToApply = cata alg where
  alg (Call a [b]) =  ktCall (ktProperty a (varRefUnqual $ MkKtIdent "app")) [b]
  alg a = Fix a

-- If a when case does not cover all cases, an else branch is needed
-- since Kotlin can sometimes not infer, that all cases are covered, this adds a default case
addElseCases :: KtExpr -> KtExpr
addElseCases = cata alg where
  alg :: KtExprF KtExpr -> KtExpr
  alg (WhenExpr cases) = ktWhenExpr $ reverse $ case reverse cases of
      [] -> []
      (WhenCase [] b : cs) -> ElseCase b : cs
      cs -> ElseCase errorMsg : cs
      where 
        errorMsg = ktAsAny (ktCall (varRefUnqual $ MkKtIdent "error") [ktString "Error in Pattern Match"])
  alg a = Fix a

-- <mod>.<f>.app(<mod>.<cls>).app(<a>).app(<b>)
pattern CallOn2 mod cls mod' f a b = 
  (CallApp
    (Fix (CallApp
      (Fix (CallApp
        (Fix (VarRef (Qualified (Just mod') f)))
        (Fix (VarRef (Qualified (Just mod) cls)))
      ))
      a
    ))
    b
  )

inline :: KtExpr -> KtExpr
inline = cata alg where
  alg :: KtExprF KtExpr -> KtExpr
  alg (CallOn2 Semigroup (MkKtIdent "semigroupString") Semigroup (MkKtIdent "append") a b)
    = ktAdd (ktAsString a) (ktAsString b)
  alg (CallOn2 Semigroup (MkKtIdent "semigroupArray") Semigroup (MkKtIdent "append") a b)
    = ktAdd (ktAsList a) (ktAsList b)
  alg a = Fix a

-- `Prim.undefined` is used for arguments that are not used by the reciever (from what I can tell)
-- we'll give `Prim.undefined` the value Unit
primUndefToUnit :: KtExpr -> KtExpr
primUndefToUnit = cata (Fix . alg) where
  alg (VarRef (Qualified (Just PrimModule) (MkKtIdent ident))) | ident == C.undefined = VarRef $ Qualified Nothing (MkKtIdent "Unit")
  alg a = a