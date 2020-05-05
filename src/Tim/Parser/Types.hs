{-# LANGUAGE PatternSynonyms #-}

module Tim.Parser.Types where

import qualified Data.List.NonEmpty as List
import Data.String.Cases
import qualified Data.String.Cases as String
import RIO
import qualified Tim.Lexer.Types as Lexers

type Code = [Syntax]

-- | The final result of the parser
data AST = Code Code -- ^ Whole of a code
         | Rhs Rhs -- ^ a term
  deriving (Show, Eq)

data FuncParam = UnboundFuncParam String.Snake -- ^ a variable that is not bound by a type: `x`
               | BoundFuncParam String.Snake Type -- ^ bound by a type: `x: Int`
               | VarFuncParams -- ^ variadic parameters: `...`
  deriving (Show, Eq)

data FuncName = UnqualifiedFuncName Pascal -- ^ F, G
              | ScopedFuncName Lexers.Scope String.Snake -- ^ s:f, g:F
              | DictFuncName DictVar -- ^ foo.bar
              | AutoloadFuncName (List.NonEmpty String.Snake) -- ^ foo#bar#baz to `AutoloadPathFuncName ["foo", "bar", "baz"]`.
  deriving (Show, Eq)

data FuncOpt = NoAbortFuncOpt
             | NoClosureFuncOpt
             | NoRangeFuncOpt
             | NoDictFuncOpt
  deriving (Show, Eq)

-- | Time script's commands (extended Vim's commands)
data Syntax = Let Lhs (Maybe Type) Rhs -- ^ 'let foo: Bar = expr' or 'let foo = expr'
            | Function
                FuncName -- ^ The function name
                [FuncParam]
                (Maybe Type) -- ^ The return type (can be omitted)
                [FuncOpt]
                [Syntax]  -- ^ Function declaretions (not `:function /{pattern}`)
            | Bar Syntax Syntax -- ^ `cmd1 | cmd2`
  deriving (Show, Eq)

-- | The left hand side
data Lhs = LVar Variable
         | LDestuct (List.NonEmpty Variable) -- ^ [x, y] of (`let [x, y] = zs`)
  deriving (Show, Eq)

-- | The right hand side
data Rhs = RVar Variable
         | RLit Literal
         | RParens Rhs -- ^ enclosed terms => `(10)`, `('str')`
  deriving (Show, Eq)

data Literal = Nat Natural
             | Int Int
             | Float Double
             | String StringLit
             | List [Literal]
             | Dict (Map StringLit Literal) -- ^ {'foo': 10}
  deriving (Show, Eq)


-- | The string literal, like `'foo'` `"bar"`.
data StringLit = SingleQuoted Text
               | DoubleQuoted Text
  deriving (Show, Eq, Ord)


-- | Please see the parser implementation
data Type = Con Camel
          | App Type Type
          | Arrow Type Type
          | Union Type Type
  deriving (Show, Eq)

infixr 3 `Arrow`
infixr 4 `Union`

-- | The parser's variable identifiers
data Variable = UnqualifiedVar String.Snake -- ^ simple_idents
              | ScopedVar Lexers.Scope ScopedName -- ^ s:coped, l:
              | DictVar DictVar -- ^ `foo.bar.baz`, `g:foo.bar`
              | RegisterVar Lexers.Register -- ^ @+, @u
              | OptionVar Lexers.Option -- ^ &nu, &number
  deriving (Show, Eq)

data ScopedName = EmptyScopedName -- ^ To allow g:, s:, l:, ...
                | NonEmptyScopedName String.Snake
  deriving (Show, Eq)

-- |
-- Two or more element acceses chains.
-- - foo.bar
-- - foo[bar]
-- - foo.bar.baz
-- - foo[bar][baz]
-- - foo.bar[baz]
data DictVar = IndexAccessDictVar DictSelf Variable -- ^ `foo.bar`
             | PropertyAccessDictVar DictSelf String.Snake -- ^ `foo[bar]`
             | IndexAccessChainDictVar DictVar Variable
             | PropertyAccessChainDictVar DictVar String.Snake
  deriving (Show, Eq)

-- | A part of 'Variable' for 'DictVar'
data DictSelf = UnqualifiedVarDictSelf String.Snake
              | ScopedVarDictSelf Lexers.Scope String
  deriving (Show, Eq)
