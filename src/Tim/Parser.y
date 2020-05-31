{
{-# LANGUAGE CPP #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE QuasiQuotes #-}

-- |
-- Parses codes, commands, and expressions.
--
-- NOTICE:
-- This only parses syntaxes, doesn't check valid syntaxes strictly.
-- Please use Tim.Checker if you want.
--
-- e.g.
-- These are parsed successfully.
-- `let x: String = 10` (invalid assigning)
-- `1.0` (1.0 is not a command, commands are not allowed at top level)
module Tim.Parser
  ( parse
  ) where

import Control.Applicative ((<|>))
import Control.Arrow ((>>>))
import Control.Exception.Safe (displayException)
import Control.Monad.Except (throwError)
import Data.Char.Cases (DigitChar(..))
import Data.Function ((&))
import Data.List.NonEmpty (NonEmpty(..), (<|))
import Data.Map.Strict (Map)
import Data.String.Cases
import Data.String.Here (i)
import Data.Text (Text)
import Data.Text.Prettyprint.Doc (pretty)
import Prelude
import RIO.List
import Tim.Lexer.Types (Token, Register(..), Option(..), Scope(..))
import Tim.Megaparsec
import Tim.Parser.Types hiding (String)
import Tim.Processor
import qualified Data.List.NonEmpty as List
import qualified Data.List.NonEmpty as NonEmptyList
import qualified Data.Map.Strict as Map
import qualified Data.String.Cases as String
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as P
import qualified Text.Megaparsec.Char.Lexer as P
import qualified Tim.Lexer.Types as Token
import qualified Tim.Parser.Types as Parser
}

%error { parseError }
%errorhandlertype explist
%monad { Parser }
%name parseAST
%tokentype { (Token, TokenPos) }

%token
  ':'           {  (Token.Colon, _)                                    }
  '='           {  (Token.Assign, _)                                   }
  nat           {  (Token.Literal (Token.Nat $$), _)                   }
  int           {  (Token.Literal (Token.Int $$), _)                   }
  float         {  (Token.Literal (Token.Float $$), _)                 }
  stringLiteral {  (Token.Literal (Token.String Token.SingleQ $$), _)  }
  stringDouble  {  (Token.Literal (Token.String Token.DoubleQ $$), _)  }
  '['           {  (Token.ListBegin, _)                                }
  ']'           {  (Token.ListEnd, _)                                  }
  '{'           {  (Token.DictBegin, _)                                }
  '}'           {  (Token.DictEnd, _)                                  }
  '('           {  (Token.ParenBegin, _)                               }
  ')'           {  (Token.ParenEnd, _)                                 }
  ','           {  (Token.Comma, _)                                    }
  '.'           {  (Token.Dot, _)                                      }
  "->"          {  (Token.Arrow, _)                                    }
  '|'           {  (Token.Bar, _)                                      }
  '#'           {  (Token.Sharp, _)                                    }
  newline       {  (Token.NewLine, _)                                  }

  -- Important commands identifiers
  let         {  (KeywordLet, _)          }
  function    {  (KeywordFunction, _)     }
  endfunction {  (KeywordEndFunction, _)  }
  return      {  (KeywordReturn, _)       }

  -- variable identifiers
  varG       {  (ScopedIdent G "", _)    }
  varS       {  (ScopedIdent S "", _)    }
  varL       {  (ScopedIdent L "", _)    }
  varV       {  (ScopedIdent V "", _)    }
  varB       {  (ScopedIdent B "", _)    }
  varW       {  (ScopedIdent W "", _)    }
  varT       {  (ScopedIdent T "", _)    }
  -- varA (the identifier 'a:') is parsed by parseAScopeVar
  varScopedG {  (ScopedIdent G $$, pos)  }
  varScopedS {  (ScopedIdent S $$, pos)  }
  varScopedL {  (ScopedIdent L $$, pos)  }
  varScopedA {  (ScopedIdent A $$, pos)  }
  varScopedV {  (ScopedIdent V $$, pos)  }
  varScopedB {  (ScopedIdent B $$, pos)  }
  varScopedW {  (ScopedIdent W $$, pos)  }
  varScopedT {  (ScopedIdent T $$, pos)  }

  varRegUnnamed   {  (RegisterIdent Unnamed, _)          }
  varRegSmallDel  {  (RegisterIdent SmallDelete, _)      }
  varRegReadOnlyC {  (RegisterIdent ReadOnlyColon, _)    }
  varRegReadonlyD {  (RegisterIdent ReadOnlyDot, _)      }
  varRegReadOnlyP {  (RegisterIdent ReadOnlyPercent, _)  }
  varRegBuffer    {  (RegisterIdent Buffer, _)           }
  varRegExpr      {  (RegisterIdent Expression, _)       }
  varRegClipS     {  (RegisterIdent ClipboardStar, _)    }
  varRegClipP     {  (RegisterIdent ClipboardPlus, _)    }
  varRegBlackHole {  (RegisterIdent BlackHole, _)        }
  varRegSeached   {  (RegisterIdent Searched, _)         }
  varRegNum       {  (RegisterIdent (Numeric $$), _)     } -- 1-9
  varRegAlpha     {  (RegisterIdent (Alphabetic $$), _)  } -- a-zA-Z

  varOption  {  (OptionIdent $$, _)   }
  varLOption {  (LOptionIdent $$, _)  }
  varGOption {  (GOptionIdent $$, _)  }

  noAbort   {  (KeywordNoAbort, _)    }
  noClosure {  (KeywordNoClosure, _)  }
  noRange   {  (KeywordNoRange, _)    }
  noDict    {  (KeywordNoDict, _)     }

  -- An another identifier, e.g.
  -- - An unscoped variable identifier
  -- - A type identifier
  ident {  (Token.Ident (Token.unIdent -> $$), pos)  }

%right '|'
%right "->"

%%

AST :: { AST }
  : Code {  Code $1  }
  | Rhs  {  Rhs $1   }

Code :: { Code }
  : {- empty -}                      {  []       }
  | OptNewLines Syntax OptNewLines   {  [$2]     }
  | OptNewLines Syntax NewLines Code {  $2 : $4  }

-- | Zero or more line-breaks
OptNewLines :: { () }
  : {- empty -}         {  ()  }
  | newline OptNewLines {  ()  }

-- | One or more
NewLines :: { () }
  : newline OptNewLines {  ()  }

Syntax :: { Syntax }
  : Let      {  $1  }
  | Return   {  $1  }
  | Function {  $1  }

Return :: { Syntax }
  : return Rhs {  Return $2  }

Function :: { Syntax }
  : function FuncName '(' FuncParams ')' FuncReturnType FuncOpts OptNewLines Code OptNewLines endfunction {  Function $2 $4 $6 $7 $9  }

FuncName :: { FuncName }
  : UnqualifiedFuncName {  FuncNameUnqualified $1  }
  | ScopedVar           {  FuncNameScoped $1       }
  | DictVar             {  FuncNameDict $1         }
  | AutoloadVar         {  FuncNameAutoload $1     }

FuncParams :: { [FuncParam] }
  : {- empty -}              {  []       }
  | FuncParam                {  $1 : []  }
  | FuncParam ',' FuncParams {  $1 : $3  }

FuncParam :: { FuncParam }
  : UnqualifiedName ':' Type {  FuncParamBound $1 $3  }
  | UnqualifiedName          {  FuncParamUnbound $1   }
  | '.' '.' '.'              {  FuncParamVariadic     }

FuncReturnType :: { Maybe Type }
  : {- empty -} {  Nothing  }
  | ':' Type    {  Just $2  }

FuncOpts :: { [FuncOpt] }
  : {- empty -}      {  []       }
  | FuncOpt FuncOpts {  $1 : $2  }

FuncOpt :: { FuncOpt }
  : '[' '[' noAbort   ']' ']' {  FuncOptNoAbort    }
  | '[' '[' noClosure ']' ']' {  FuncOptNoClosure  }
  | '[' '[' noRange   ']' ']' {  FuncOptNoRange    }
  | '[' '[' noDict    ']' ']' {  FuncOptNoDict     }

Let :: { Syntax }
  : let Lhs ':' Type '=' Rhs {  Let $2 (Just $4) $6  }
  | let Lhs '=' Rhs          {  Let $2 Nothing $4    }

Lhs :: { Lhs }
  : Variable            {  LhsVar $1         }
  | '[' DestructVar ']' {  LhsDestuctVar $2  }

-- Destructive assignee variables
DestructVar :: { List.NonEmpty Variable }
  : Variable                 {  ($1 :| [])  }
  | Variable ',' DestructVar {  $1 <| $3    }

Type :: { Type }
  : Type "->" Type {  TypeArrow $1 $3  }
  | Type '|'  Type {  TypeUnion $1 $3  }
  | Camel          {  TypeCon $1       }
  | '(' Type ')'   {  $2               }
  | TypeApp        {  $1               }

-- lefty bias
TypeApp :: { Type }
  : Type Camel        {  TypeApp $1 (TypeCon $2)  }
  | Type '(' Type ')' {  TypeApp $1 $3            }

Camel :: { Camel }
  : ident {% runParsecParserInParser pos String.parseCamel $1  }

Variable :: { Variable }
  : VariableScoped      {  $1  }
  | VariableAutoload    {  $1  }
  | VariableDict        {  $1  }
  | VariableRegister    {  $1  }
  | VariableOption      {  $1  }
  | VariableUnqualified {  $1  }

VariableScoped :: { Variable }
  : ScopedVar {  VariableScoped $1  }

ScopedVar :: { ScopedVar }
  : varG       {  ScopeVarG ScopedNameEmpty                                                          }
  | varS       {  ScopeVarS ScopedNameEmpty                                                          }
  | varL       {  ScopeVarL ScopedNameEmpty                                                          }
  | varV       {  ScopeVarV ScopedNameEmpty                                                          }
  | varB       {  ScopeVarB ScopedNameEmpty                                                          }
  | varW       {  ScopeVarW ScopedNameEmpty                                                          }
  | varT       {  ScopeVarT ScopedNameEmpty                                                          }
  | varScopedG {% fmap (ScopeVarG . ScopedNameNonEmpty) $ runParsecParserInParser pos parseSnake $1  }
  | varScopedS {% fmap (ScopeVarS . ScopedNameNonEmpty) $ runParsecParserInParser pos parseSnake $1  }
  | varScopedL {% fmap (ScopeVarL . ScopedNameNonEmpty) $ runParsecParserInParser pos parseSnake $1  }
  | varScopedV {% fmap (ScopeVarV . ScopedNameNonEmpty) $ runParsecParserInParser pos parseSnake $1  }
  | varScopedB {% fmap (ScopeVarB . ScopedNameNonEmpty) $ runParsecParserInParser pos parseSnake $1  }
  | varScopedW {% fmap (ScopeVarW . ScopedNameNonEmpty) $ runParsecParserInParser pos parseSnake $1  }
  | varScopedT {% fmap (ScopeVarT . ScopedNameNonEmpty) $ runParsecParserInParser pos parseSnake $1  }
  | varScopedA {% fmap ScopeVarA $ runParsecParserInParser pos parseAScopeVar $1                     }

VariableAutoload :: { Variable }
  : AutoloadVar {  VariableAutoload $1  }

AutoloadVar :: { AutoloadVar }
  : AutoloadVarNames '#'                 {  AutoloadVar (NonEmptyList.reverse $1) OmittableSnakeOmitted     }
  | AutoloadVarNames '#' UnqualifiedName {  AutoloadVar (NonEmptyList.reverse $1) (OmittableSnakeSnake $3)  }

AutoloadVarNames :: { List.NonEmpty Snake }
  : UnqualifiedName                      {  $1 :| []  }
  | AutoloadVarNames '#' UnqualifiedName {  $3 <| $1  }

VariableDict :: { Variable }
  : DictVar {  VariableDict $1  }

DictVar :: { DictVar }
  : DictSelf '[' Rhs ']'         {  DictVarIndexAccess $1 $3          }
  | DictSelf '.' UnqualifiedName {  DictVarPropertyAccess $1 $3       }
  | DictVar '[' Rhs ']'          {  DictVarIndexAccessChain $1 $3     }
  | DictVar '.' UnqualifiedName  {  DictVarPropertyAccessChain $1 $3  }

DictSelf :: { DictSelf }
  : UnqualifiedName {  DictSelfUnqualified $1  }
  | ScopedVar       {  DictSelfScoped $1       }

VariableRegister :: { Variable }
  : varRegUnnamed   {  VariableRegister Unnamed          }
  | varRegSmallDel  {  VariableRegister SmallDelete      }
  | varRegReadOnlyC {  VariableRegister ReadOnlyColon    }
  | varRegReadonlyD {  VariableRegister ReadOnlyDot      }
  | varRegReadOnlyP {  VariableRegister ReadOnlyPercent  }
  | varRegBuffer    {  VariableRegister Buffer           }
  | varRegExpr      {  VariableRegister Expression       }
  | varRegClipS     {  VariableRegister ClipboardStar    }
  | varRegClipP     {  VariableRegister ClipboardPlus    }
  | varRegBlackHole {  VariableRegister BlackHole        }
  | varRegSeached   {  VariableRegister Searched         }
  | varRegNum       {  VariableRegister $ Numeric $1     }
  | varRegAlpha     {  VariableRegister $ Alphabetic $1  }

VariableOption :: { Variable }
  : varLOption {  VariableOption $ LocalScopedOption $1   }
  | varGOption {  VariableOption $ GlobalScopedOption $1  }
  | varOption  {  VariableOption $ UnscopedOption $1      }

VariableUnqualified :: { Variable }
  : UnqualifiedName {  VariableUnqualified $1  }

UnqualifiedName :: { Snake }
  : ident {% runParsecParserInParser pos parseSnake $1  }

UnqualifiedFuncName :: { UpperSnake }
  : ident {% runParsecParserInParser pos parseUpperSnake $1 }

Rhs :: { Rhs }
  : Variable            {  RhsVar $1          }
  | Literal             {  RhsLit $1          }
  | FuncCallee FuncArgs {  RhsFuncCall $1 $2  }
  | '(' Rhs ')'         {  RhsParens $2       }

Literal :: { Literal }
  : nat               {  LiteralNat $1     }
  | int               {  LiteralInt $1     }
  | float             {  LiteralFloat $1   }
  | String            {  LiteralString $1  }
  | '[' ListInner ']' {  LiteralList $2    }
  | '{' DictInner '}' {  LiteralDict $2    }

FuncCallee :: { FuncCallee }
  : FuncName        {  FuncCalleeFuncName $1     }
  | UnqualifiedName {  FuncCalleeUnqualified $1  }

FuncArgs :: { [Rhs] }
  : '(' FuncArgsInner ')' { $2 }

FuncArgsInner :: { [Rhs] }
  : {- empty -}           {  []       }
  | Rhs                   {  [$1]     }
  | Rhs ',' FuncArgsInner {  $1 : $3  }

String :: { Parser.String }
  : stringLiteral {  StringLiteral $1  }
  | stringDouble  {  StringDouble $1   }

ListInner :: { [Literal] }
  : {- empty -}           {  []       }
  | Literal               {  [$1]     }
  | Literal ',' ListInner {  $1 : $3  }

DictInner :: { Map Parser.String Literal }
  : {- empty -}                      {  Map.empty            }
  | String ':' Literal               {  Map.singleton $1 $3  }
  | String ':' Literal ',' DictInner {  Map.insert $1 $3 $5  }

{
pattern ScopedIdent :: Scope -> String -> Token
pattern ScopedIdent s x = Token.Ident (Token.QualifiedIdent (Token.Scoped s x))

pattern RegisterIdent :: Register -> Token
pattern RegisterIdent r = Token.Ident (Token.QualifiedIdent (Token.Register r))

pattern OptionIdent :: LowerString -> Token
pattern OptionIdent x = Token.Ident (Token.QualifiedIdent (Token.Option (Token.UnscopedOption x)))

pattern LOptionIdent :: LowerString -> Token
pattern LOptionIdent x = Token.Ident (Token.QualifiedIdent (Token.Option (Token.LocalScopedOption x)))

pattern GOptionIdent :: LowerString -> Token
pattern GOptionIdent x = Token.Ident (Token.QualifiedIdent (Token.Option (Token.GlobalScopedOption x)))


pattern KeywordLet :: Token
pattern KeywordLet = Token.Ident (Token.UnqualifiedIdent (String.NonEmpty 'l' "et"))

pattern KeywordFunction :: Token
pattern KeywordFunction = Token.Ident (Token.UnqualifiedIdent (String.NonEmpty 'f' "unction"))

pattern KeywordEndFunction :: Token
pattern KeywordEndFunction = Token.Ident (Token.UnqualifiedIdent (String.NonEmpty 'e' "ndfunction"))

pattern KeywordReturn :: Token
pattern KeywordReturn = Token.Ident (Token.UnqualifiedIdent (String.NonEmpty 'r' "eturn"))

pattern KeywordNoAbort :: Token
pattern KeywordNoAbort = Token.Ident (Token.UnqualifiedIdent (NonEmpty 'n' "o-abort"))

pattern KeywordNoClosure :: Token
pattern KeywordNoClosure = Token.Ident (Token.UnqualifiedIdent (NonEmpty 'n' "o-closure"))

pattern KeywordNoRange :: Token
pattern KeywordNoRange = Token.Ident (Token.UnqualifiedIdent (NonEmpty 'n' "o-range"))

pattern KeywordNoDict :: Token
pattern KeywordNoDict = Token.Ident (Token.UnqualifiedIdent (NonEmpty 'n' "o-dict"))


parse :: [(Token, TokenPos)] -> Either ParseError AST
parse = runParser . parseAST

-- TODO: Show all [(Token, TokenPos)] if --verbose specified on cli.
parseError :: ([(Token, TokenPos)], [String]) -> Parser a
parseError ((got, pos) : _, expected) =
  throwError . flip ParseError (OnAToken pos) $ flattenMargins [i|
    got a token `${show $ pretty got}`,
    but ${expected} are expected at here.
  |]
parseError ([], expected) =
  throwError $ ParseError [i|got EOF, but ${makePluralForm expected} are expected at here.|] EOF
  where
    -- ["a", "b"]      -> "a or b"
    -- ["a", "b", "c"] -> "a, b, or c"
    makePluralForm [] = []
    makePluralForm [x] = x
    makePluralForm (x : y : words) =
      case uncons $ reverse words of
        Nothing ->  [i|${x} or ${y}|]
        Just (tail, reverse -> body) -> [i|${foldl comma "" body}, or ${tail}|]

    comma :: String -> String -> String
    comma x y = [i|${x}, ${y}|]

flattenMargins :: String -> String
flattenMargins = replace . unlines . filter (/= "") . map (dropWhile (== ' ')) . lines
  where
    replace [] = []
    replace ('\n' : xs) = ' ' : replace xs
    replace (x : xs) = x : replace xs

runParsecParserInParser :: TokenPos -> CodeParsec a -> String -> Parser a
runParsecParserInParser pos parser input =
  case P.runParser parser "time-script" input of
    Right x -> pure x
    Left  e -> throwError $ ParseError (displayException e) (OnAToken pos)

parseAScopeVar :: CodeParsec AScopeName
parseAScopeVar =
    varAll <|>
    varNum <|>
    nonEmptyName <|>
    emptyName
  where
    varAll = AScopeNameVarAll <$ P.string "000"
    varNum = AScopeNameVarNum <$> P.decimal
    nonEmptyName = AScopeNameName . ScopedNameNonEmpty <$> parseSnake
    emptyName = AScopeNameName ScopedNameEmpty <$ P.string ""
}
