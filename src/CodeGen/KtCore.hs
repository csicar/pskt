module CodeGen.KtCore where

import Prelude hiding (print)
import Data.Text (Text)
import Data.Maybe (catMaybes, fromMaybe, Maybe(..))
import Data.List (nub)
import Debug.Trace (trace)
import qualified Data.Text as T
import Control.Monad (forM, replicateM, void)
import Control.Monad.Supply.Class (MonadSupply, fresh)
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
import Text.Pretty.Simple (pShow)
import Language.PureScript.AST.SourcePos (displayStartEndPos)

data WhenExpr a
  = WhenExpr [WhenCase a]

data WhenCase a
  -- Conditions, return value
  = WhenCase [a] a

data Env a = Env {mod :: Module a}

data BinOp
  = Equals
  | IsType
  | And

instance PrintKt BinOp where
  print _ Equals = "=="
  print _ IsType = "is"
  print _ And = "&&"

data KtExpr
  = If KtExpr KtExpr (Maybe KtExpr)
  | VariableIntroduction Ident KtExpr
  | Destructuring [Ident] KtExpr
  | Binary BinOp KtExpr KtExpr
  | VarRef Ident
  | Cast KtExpr KtExpr
  | CoreImp (Doc())

instance PrintKt KtExpr where
  print env (If a b c) = "if" <> parens (print env a) <> braceNested (print env b) <+> fromMaybe "" (("else" <+>) . braceNested . print env <$> c)
  print env (VariableIntroduction ident expr) = "val" <+> (print env ident) <+> "=" <+> (print env expr)
  print env (Destructuring binders expr) = "val" <+> parens (commaSep $ print env <$> binders) <+> "=" <+> (print env expr)
  print env (Binary op left right) = print env left <+> print env op <+> print env right
  print env (VarRef ident) = print env ident
  print env (Cast a b) = print env a <+> "as" <+> print env b
  print env (CoreImp a) = a

nest' = nest 4

joinWith :: Doc a -> [Doc a] -> Doc a
joinWith c d = concatWith f d
   where
   f a b = a <> c <> softline <> b

commaSep :: [Doc a] -> Doc a
commaSep = joinWith ", "

braceNested :: Doc a -> Doc a
braceNested d = vsep [nest' $ vsep ["{", d], "}"]



class PrintKt a where
  print :: Env Ann -> a -> Doc ()

instance PrintKt (WhenCase (Doc ())) where
  print env (WhenCase guards expr) = joinWith " &&" guards <+> "->" <+> braceNested expr

instance PrintKt (WhenExpr (Doc ())) where
  print env (WhenExpr cases) = vsep 
    [ nest 2 $ vsep 
      [ "when {" 
      , vsep (print env <$> cases)
       ]
    , "}"
    ]


instance PrintKt a => PrintKt (Qualified a) where
   print env (Qualified Nothing ident) = print env ident
   print env@(Env envMod) (Qualified (Just mod) ident) | moduleName envMod == mod = print env ident
   print env (Qualified (Just mod) ident) = print env mod <> "." <> print env ident

instance PrintKt ModuleName where
   print env mod = pretty $ runModuleName mod

instance PrintKt (ProperName ConstructorName) where
   print _ = pretty . runProperName

instance PrintKt Ident where
  print env (Ident i) = pretty i
  print env (GenIdent Nothing n) = "__" <> pretty (show n)
  print env (GenIdent (Just name) n) = "__" <> pretty name <> pretty (show n)
  print env UnusedIdent = "__"