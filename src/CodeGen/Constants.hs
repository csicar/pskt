module CodeGen.Constants where

import Protolude
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

psNamespace :: ProperName Namespace
psNamespace = ProperName "PS"

moduleNamespace :: ProperName Namespace
moduleNamespace = ProperName "Module"

pattern PrimModule = ModuleName [ProperName "PS", ProperName "Prim", ProperName "Module"]

pattern Semigroup = ModuleName [ProperName "PS", ProperName "Data", ProperName "Semigroup", ProperName "Module"]

pattern Applicative = ModuleName [ProperName "PS", ProperName "Control", ProperName "Applicative", ProperName "Module"]

pattern Effect = ModuleName [ProperName "PS", ProperName "Effect", ProperName "Module"]
pattern Bind = ModuleName [ProperName "PS", ProperName "Control", ProperName "Bind", ProperName "Module"]

isReserved :: Text -> Bool
isReserved = (`elem` allReserved)

allReserved = hardKeywords ++ softKeywords ++ modifierKeywords ++ specialIdentifier

hardKeywords = 
  [ "as"
  , "as?"
  , "break"
  , "class"
  , "continue"
  , "do"
  , "else"
  , "false"
  , "for"
  , "fun"
  , "if"
  , "in"
  , "!in"
  , "interface"
  , "is"
  , "!is"
  , "null"
  , "object"
  , "package"
  , "return"
  , "super"
  , "this"
  , "throw"
  , "true"
  , "try"
  , "typealias"
  , "typeof"
  , "val"
  , "var"
  , "when"
  , "while"
  ]

softKeywords =
  [ "by"
  , "catch"
  , "constructor"
  , "delegate"
  , "dynamic"
  , "field"
  , "file"
  , "finally"
  , "get"
  , "import"
  , "init"
  , "param"
  , "property"
  , "receiver"
  , "set"
  , "setparam"
  , "where"
  ]

modifierKeywords =
  [ "actual"
  , "abstract"
  , "annotation"
  , "companion"
  , "const"
  , "crossinline"
  , "data"
  , "enum"
  , "expect"
  , "external"
  , "final"
  , "infix"
  , "inline"
  , "inner"
  , "internal"
  , "lateinit"
  , "noinline"
  , "open"
  , "operator"
  , "out"
  , "override"
  , "private"
  , "protected"
  , "public"
  , "reified"
  , "sealed"
  , "suspend"
  , "tailrec"
  , "vararg"
  ]
specialIdentifier =
  ["field"
  , "it"
  ]

operatorsAndSpecialsymbols =
  [ "+"
  , "-"
  , "*"
  , "/"
  , "%"
  , "*"
  , "="
  , "+="
  , "-="
  , "*="
  , "/="
  , "%="
  , "++"
  , "--"
  , "&&"
  , "||"
  , "!"
  , "=="
  , "!="
  , "equals()"
  , "==="
  , "!=="
  , "<"
  , ">"
  , "<="
  , ">="
  , "compareTo()"
  , "["
  , "]"
  , "get"
  , "set"
  , "!!"
  , "?."
  , "?:"
  , "::"
  , ".."
  , ":"
  , "?"
  , "->"
  , "@"
  , ";"
  , "$"
  , "_"
  ]