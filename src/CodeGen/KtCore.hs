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

newtype WhenExpr a = WhenExpr [WhenCase a]

data WhenCase a
  -- Conditions, return value
  = WhenCase [a] a

data Env a = Env {mod :: Module a, recursiveIdent :: [ Ident ]}

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
  | RecVarIntro Ident KtExpr
  | Destructuring [Ident] KtExpr
  | Binary BinOp KtExpr KtExpr
  | Property KtExpr KtExpr
  | ArrayAccess KtExpr KtExpr
  | ObjectAccess KtExpr KtExpr
  | VarRef Ident
  | Cast KtExpr KtExpr
  | Fun (Maybe Ident) Ident KtExpr
  | CoreImp (Doc())

instance PrintKt KtExpr where
  print env (If a b c) = "if" <> parens (print env a) <> braceNested (print env b) <+> fromMaybe "" (("else" <+>) . braceNested . print env <$> c)
  print env (VariableIntroduction ident expr) = "val" <+> print env ident <+> "=" <+> print env expr
  print env (RecVarIntro ident expr) = "lateinit var" <+> print env ident <> ": Any;" <+> print env ident <+> "=" <+> print env expr
  print env (Destructuring binders expr) = "val" <+> parens (commaSep $ print env <$> binders) <+> "=" <+> (print env expr)
  print env (Binary op left right) = print env left <+> print env op <+> print env right
  print env (Property left right) = print env left <> "." <> print env right
  print env (ArrayAccess left right) = print env (Cast left (CoreImp "List<Any>")) <>  brackets (print env right)
  print env (ObjectAccess left right) = print env (Cast left (CoreImp "Map<String, Any>")) <>  brackets (print env right) <> "!!"
  print env (VarRef ident) = print env ident
  print env (Cast a b) = parens $ print env a <+> "as" <+> print env b
  print env (Fun name arg body) = "fun" <+> (fromMaybe "" $ (print env) <$> name) <> parens (print env arg <+> ": Any") <+> "=" <> softline'
  print env (CoreImp a) = a

nest' = nest 4

joinWith :: Doc a -> [Doc a] -> Doc a
joinWith c = concatWith f
   where
   f a b = a <> c <> softline <> b

commaSep :: [Doc a] -> Doc a
commaSep = joinWith ", "

braceNested :: Doc a -> Doc a
braceNested d = vsep [nest' $ vsep ["{", d], "}"]



class PrintKt a where
  print :: Env Ann -> a -> Doc ()

instance PrintKt (WhenCase (Doc ())) where
  print env (WhenCase guards expr) = guardExpr <+> "->" <+> braceNested expr
    where 
      guardExpr = case guards of
        [] -> "true"
        _ -> joinWith " &&" guards

instance PrintKt (WhenExpr (Doc ())) where
  print env (WhenExpr cases) = vsep 
    [ nest' $ vsep 
      [ "when {" 
      , vsep (print env <$> cases)
       ]
    , "}"
    ]


instance PrintKt a => PrintKt (Qualified a) where
   print env (Qualified Nothing ident) = print env ident
   print env@(Env envMod _) (Qualified (Just mod) ident) | moduleName envMod == mod = print env ident
   print env (Qualified (Just mod) ident) = case show sub of
      "" -> "Unit"
      _ -> print env mod <> ".Module." <> print env ident
    where sub = print env ident

instance PrintKt ModuleName where
   print env mod = pretty $ runModuleName mod

instance PrintKt (ProperName a) where
   print _ = pretty . runProperName

instance PrintKt Ident where
  print env (Ident "$__unused") = "__unused"
  print env (Ident "undefined") = ""
  print (Env _ recIdents) ident@(Ident i) | ident `elem` recIdents = pretty i <> "__rec"
  print env (Ident i) = pretty i
  print env (GenIdent Nothing n) = "__" <> pretty (show n)
  print env (GenIdent (Just name) n) = "__" <> pretty name <> pretty (show n)
  print env UnusedIdent = "__unused"