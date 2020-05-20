{-# LANGUAGE NoDataKinds #-}
{-# LANGUAGE PatternSynonyms #-}

module Tim.Parser.Types where

import qualified Data.List.NonEmpty as List
import Data.String.Cases
import qualified Data.String.Cases as String
import RIO hiding (String)
import qualified Tim.Lexer.Types as Lexer

type Code = [Syntax]

-- | The final result of the parser
data AST = Code Code -- ^ Whole of a code
         | Rhs Rhs -- ^ a term
  deriving (Show, Eq)

data FuncParam = FuncParamUnbound String.Snake -- ^ a variable that is not bound by a type: `x`
               | FuncParamBound String.Snake Type -- ^ bound by a type: `x: Int`
               | FuncParamVariadic -- ^ variadic parameters: `...`
  deriving (Show, Eq)

data FuncName = FuncNameUnqualified String.Snake -- ^ F, G
              | FuncNameScoped ScopedVar -- ^ s:f, g:F
              | FuncNameDict DictVar -- ^ foo.bar
              | FuncNameAutoload AutoloadVar
  deriving (Show, Eq)

data FuncOpt = FuncOptNoAbort
             | FuncOptNoClosure
             | FuncOptNoRange
             | FuncOptNoDict
  deriving (Show, Eq)

-- | Time script's commands (extended Vim's commands)
data Syntax = Let Lhs (Maybe Type) Rhs -- ^ 'let foo: Bar = expr' or 'let foo = expr'
            | Return Rhs
            | Function -- ^ Function declarations (not `:function /{pattern}`)
                FuncName -- ^ The function name
                [FuncParam]
                (Maybe Type) -- ^ The return type (can be omitted)
                [FuncOpt]
                Code -- ^ function details
            | Bar Syntax Syntax -- ^ `cmd1 | cmd2`
  deriving (Show, Eq)

-- | The left hand side
data Lhs = LhsVar Variable
         | LhsDestuctVar (List.NonEmpty Variable) -- ^ [x, y] of (`let [x, y] = zs`)
  deriving (Show, Eq)

-- | The right hand side
data Rhs = RhsVar Variable
         | RhsLit Literal
         | RhsParens Rhs -- ^ enclosed terms => `(10)`, `('str')`
  deriving (Show, Eq)

data Literal = LiteralNat Natural
             | LiteralInt Int
             | LiteralFloat Double
             | LiteralString String
             | LiteralList [Literal]
             | LiteralDict (Map String Literal) -- ^ {'foo': 10}
  deriving (Show, Eq)


-- | The string literal, like `'foo'` `"bar"`.
data String = StringLiteral Text -- ^ literal-string
            | StringDouble Text -- ^ string (be quoted by double quotes)
  deriving (Show, Eq, Ord)


-- | Please see the parser implementation
data Type = TypeCon Camel
          | TypeApp Type Type
          | TypeArrow Type Type
          | TypeUnion Type Type
  deriving (Show, Eq)

infixr 3 `TypeArrow`
infixr 4 `TypeUnion`

-- | The parser's variable identifiers
data Variable = VariableUnqualified String.Snake -- ^ simple_idents
              | VariableScoped ScopedVar -- ^ s:coped, l:, a:000
              | VariableAutoload AutoloadVar
              | VariableDict DictVar -- ^ `foo.bar.baz`, `g:foo.bar`
              | VariableRegister Lexer.Register -- ^ @+, @u
              | VariableOption Lexer.Option -- ^ &nu, &number
  deriving (Show, Eq)

data ScopedVar = ScopeVarG ScopedName
               | ScopeVarS ScopedName
               | ScopeVarL ScopedName
               | ScopeVarV ScopedName
               | ScopeVarB ScopedName
               | ScopeVarW ScopedName
               | ScopeVarT ScopedName
               | ScopeVarA AScopeName
  deriving (Show, Eq)

data ScopedName = ScopedNameEmpty -- ^ To allow g:, s:, l:, ...
                | ScopedNameNonEmpty String.Snake
  deriving (Show, Eq)

data AScopeName = AScopeNameVarAll -- ^ a:000
                | AScopeNameVarNum Natural -- ^ a:0, a:1, ...
                | AScopeNameName ScopedName -- ^ a:foo, a:bar
  deriving (Show, Eq)

-- |
-- Two or more element acceses chains.
-- - foo.bar
-- - foo[bar]
-- - foo.bar.baz
-- - foo[bar][baz]
-- - foo.bar[baz]
data DictVar = DictVarIndexAccess DictSelf Rhs -- ^ `foo.bar`
             | DictVarPropertyAccess DictSelf String.Snake -- ^ `foo[bar]`
             | DictVarIndexAccessChain DictVar Rhs
             | DictVarPropertyAccessChain DictVar String.Snake
  deriving (Show, Eq)

-- | A part of 'Variable' for 'DictVar'
data DictSelf = DictSelfUnqualified String.Snake
              | DictSelfScoped ScopedVar
  deriving (Show, Eq)

-- |
-- Autoload variables.
-- - foo#bar
-- - foo#bar#baz
-- - x#
data AutoloadVar = AutoloadVar
  { names :: List.NonEmpty String.Snake -- ^ names excluding the last. e.g. `foo`, `bar` of `foo#bar#baz`
  , lastName :: OmittableSnake -- ^ the last name. e.g. `baz` of `foo#bar#baz`, `` (the empty) of `x#`
  } deriving (Show, Eq)

data OmittableSnake = OmittableSnakeOmitted -- ^ an empty string
                    | OmittableSnakeSnake String.Snake -- ^ a non-empty string
  deriving (Show, Eq)
