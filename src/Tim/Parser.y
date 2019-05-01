{
{-# LANGUAGE CPP #-}
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

import Control.Arrow ((>>>))
import Control.Monad.Except (throwError)
import Data.Function ((&))
import Data.List.NonEmpty (NonEmpty(..), (<|))
import Data.Map.Strict (Map)
import Data.String.Here (i)
import Data.Text (Text)
import Data.Text.Prettyprint.Doc (pretty)
import Prelude
import Text.Megaparsec (parseMaybe)
import Tim.Char (DigitChar(..))
import Tim.Lexer.Types (Token)
import Tim.Parser.Types
import Tim.Processor
import Tim.String (LowerString, Camel (..))
import qualified Data.Map.Strict as Map
import qualified Tim.Char as Char
import qualified Tim.Lexer.Types as Token
import qualified Tim.String as String
import qualified Tim.String.Parser as P
}

%error { parseError }
%errorhandlertype explist
%monad { Processor }
%name parseAST
%tokentype { (Token, TokenPos) }

%token
  ':'           { (Token.Colon, _)                                   }
  '='           { (Token.Assign, _)                                  }
  nat           { (Token.Literal (Token.Nat $$), _)                  }
  int           { (Token.Literal (Token.Int $$), _)                  }
  float         { (Token.Literal (Token.Float $$), _)                }
  singleQString { (Token.Literal (Token.String Token.SingleQ $$), _) }
  doubleQString { (Token.Literal (Token.String Token.DoubleQ $$), _) }
  '['           { (Token.ListBegin, _)                               }
  ']'           { (Token.ListEnd, _)                                 }
  '{'           { (Token.DictBegin, _)                               }
  '}'           { (Token.DictEnd, _)                                 }
  '('           { (Token.ParenBegin, _)                              }
  ')'           { (Token.ParenEnd, _)                                }
  ','           { (Token.Comma, _)                                   }
  "->"          { (Token.Arrow, _)                                   }
  '|'           { (Token.Bar, _)                                     }
  lineBreak     { (Token.LineBreak, _)                               }

  -- Important command identifiers
  let { (Token.Ident (String.NonEmpty 'l' "et"), _) }

  -- variable identifiers
  varScopedG { (Token.Ident (String.unNonEmpty -> 'g' : ':' : $$), _) }
  varScopedS { (Token.Ident (String.unNonEmpty -> 's' : ':' : $$), _) }
  varScopedL { (Token.Ident (String.unNonEmpty -> 'l' : ':' : $$), _) }
  varScopedA { (Token.Ident (String.unNonEmpty -> 'a' : ':' : $$), _) }
  varScopedV { (Token.Ident (String.unNonEmpty -> 'v' : ':' : $$), _) }
  varScopedB { (Token.Ident (String.unNonEmpty -> 'b' : ':' : $$), _) }
  varScopedW { (Token.Ident (String.unNonEmpty -> 'w' : ':' : $$), _) }
  varScopedT { (Token.Ident (String.unNonEmpty -> 't' : ':' : $$), _) }

  varRegUnnamed   { (Token.Ident (String.unNonEmpty -> '@' : '"' : $$), _) }
  varRegSmallDel  { (Token.Ident (String.unNonEmpty -> '@' : '-' : $$), _) }
  varRegReadOnlyC { (Token.Ident (String.unNonEmpty -> '@' : ':' : $$), _) }
  varRegReadonlyD { (Token.Ident (String.unNonEmpty -> '@' : '.' : $$), _) }
  varRegReadOnlyP { (Token.Ident (String.unNonEmpty -> '@' : '%' : $$), _) }
  varRegBuffer    { (Token.Ident (String.unNonEmpty -> '@' : '#' : $$), _) }
  varRegExpr      { (Token.Ident (String.unNonEmpty -> '@' : '=' : $$), _) }
  varRegClipS     { (Token.Ident (String.unNonEmpty -> '@' : '*' : $$), _) }
  varRegClipP     { (Token.Ident (String.unNonEmpty -> '@' : '+' : $$), _) }
  varRegBlackHole { (Token.Ident (String.unNonEmpty -> '@' : '_' : $$), _) }
  varRegSeached   { (Token.Ident (String.unNonEmpty -> '@' : '/' : $$), _) }
  varReg0         { (Token.Ident (String.unNonEmpty -> '@' : '0' : $$), _) }
  varReg1         { (Token.Ident (String.unNonEmpty -> '@' : '1' : $$), _) }
  varReg2         { (Token.Ident (String.unNonEmpty -> '@' : '2' : $$), _) }
  varReg3         { (Token.Ident (String.unNonEmpty -> '@' : '3' : $$), _) }
  varReg4         { (Token.Ident (String.unNonEmpty -> '@' : '4' : $$), _) }
  varReg5         { (Token.Ident (String.unNonEmpty -> '@' : '5' : $$), _) }
  varReg6         { (Token.Ident (String.unNonEmpty -> '@' : '6' : $$), _) }
  varReg7         { (Token.Ident (String.unNonEmpty -> '@' : '7' : $$), _) }
  varReg8         { (Token.Ident (String.unNonEmpty -> '@' : '8' : $$), _) }
  varReg9         { (Token.Ident (String.unNonEmpty -> '@' : '9' : $$), _) }
  varRegAlpha     { (Token.Ident (String.unNonEmpty -> '@' : $$), pos)     } -- a-zA-Z

  varOptionScopedL  { (Token.Ident (String.unNonEmpty -> '&' : 'l' : ':' : $$), pos) }
  varOptionScopedG  { (Token.Ident (String.unNonEmpty -> '&' : 'g' : ':' : $$), pos) }
  varOptionUnscoped { (Token.Ident (String.unNonEmpty -> '&' : $$), pos)             }

  varSimpleLocal { (Token.Ident (String.unNonEmpty -> $$), pos) }

  -- An another identifier
  ident { (Token.Ident (String.unNonEmpty -> $$), pos) }

%%

AST :: { AST }
  : Code { Code $1 }
  | Rhs  { Rhs $1  }

Code :: { Code }
  : {- empty -}           { []      }
  | Syntax lineBreak Code { $1 : $3 }

Syntax :: { Syntax }
  : let Lhs ':' Type '=' Rhs { Let $2 (Just $4) $6 }
  | let Lhs '=' Rhs          { Let $2 Nothing $4   }

Lhs :: { Lhs }
  : Variable         { LVar $1     }
  | '[' DestVars ']' { LDestuct $2 }

Type :: { Type }
  : Camel          { Name $1     }
  | Type TypeArgs  { App $1 $2   }
  | '(' Type ')'   { Parens $2   }
  | Type "->" Type { Arrow $1 $3 }
  | Type '|' Type  { Union $1 $3 }

TypeArgs :: { [Type] }
  : Type          { [$1]    }
  | Type TypeArgs { $1 : $2 }

Camel :: { Camel }
  : ident {% resolveParsingCamel pos $ P.parseCamel $1 }

Variable :: { Variable }
  : varScopedG      { Scoped G $1              }
  | varScopedS      { Scoped S $1              }
  | varScopedL      { Scoped L $1              }
  | varScopedA      { Scoped A $1              }
  | varScopedV      { Scoped V $1              }
  | varScopedB      { Scoped B $1              }
  | varScopedW      { Scoped W $1              }
  | varScopedT      { Scoped T $1              }
  | varRegUnnamed   { Register Unnamed         }
  | varRegSmallDel  { Register SmallDelete     }
  | varRegReadOnlyC { Register ReadOnlyColon   }
  | varRegReadonlyD { Register ReadOnlyDot     }
  | varRegReadOnlyP { Register ReadOnlyPercent }
  | varRegBuffer    { Register Buffer          }
  | varRegExpr      { Register Expression      }
  | varRegClipS     { Register ClipboardStar   }
  | varRegClipP     { Register ClipboardPlus   }
  | varRegBlackHole { Register BlackHole       }
  | varRegSeached   { Register Searched        }
  | varReg0         { Register $ Numeric D0    }
  | varReg1         { Register $ Numeric D1    }
  | varReg2         { Register $ Numeric D2    }
  | varReg3         { Register $ Numeric D3    }
  | varReg4         { Register $ Numeric D4    }
  | varReg5         { Register $ Numeric D5    }
  | varReg6         { Register $ Numeric D6    }
  | varReg7         { Register $ Numeric D7    }
  | varReg8         { Register $ Numeric D8    }
  | varReg9         { Register $ Numeric D9    }
  | varRegAlpha       {% fmap Register $ readRegAlpha $1 pos                         }
  | varOptionScopedL  {% fmap (Option . LocalScopedOption) $ readLowerString $1 pos  }
  | varOptionScopedG  {% fmap (Option . GlobalScopedOption) $ readLowerString $1 pos }
  | varOptionUnscoped {% fmap (Option . UnscopedOption) $ readLowerString $1 pos     }
  | varSimpleLocal { SimpleLocal $1 }

-- Destructive assignee variables
DestVars :: { NonEmpty Variable }
  : Variable              { ($1 :| []) }
  | Variable ',' DestVars { $1 <| $3   }

Rhs :: { Rhs }
  : Variable    { RVar $1    }
  | Literal     { RLit $1    }
  | '(' Rhs ')' { RParens $2 }

Literal :: { Literal }
  : nat               { Nat $1    }
  | int               { Int $1    }
  | float             { Float $1  }
  | StringLit         { String $1 }
  | '[' ListInner ']' { List $2   }
  | '{' DictInner '}' { Dict $2   }

StringLit :: { StringLit }
  : singleQString { SingleQuoted $1 }
  | doubleQString { DoubleQuoted $1 }

ListInner :: { [Literal] }
  : {- empty -}           { []      }
  | Literal               { [$1]    }
  | Literal ',' ListInner { $1 : $3 }

DictInner :: { Map StringLit Literal }
  : {- empty -}                         { Map.empty           }
  | StringLit ':' Literal               { Map.singleton $1 $3 }
  | StringLit ':' Literal ',' DictInner { Map.insert $1 $3 $5 }

{
parse :: [(Token, TokenPos)] -> Either Failure AST
parse = runProcessor . parseAST

-- TODO: Show all [(Token, TokenPos)] if --verbose specified on cli.
parseError :: ([(Token, TokenPos)], [String]) -> Processor a
parseError ((got, pos) : _, expected) =
  throwError . flip Failure (OnAToken pos) $ flattenMargins [i|
    got a token `${show $ pretty got}`
    at ${show $ pretty pos},
    but ${expected} are expected at here.
  |]
parseError ([], expected) =
  throwError $ Failure [i|got EOF, but ${expected} are expected at here.|] EOF

flattenMargins :: String -> String
flattenMargins = replace . unlines . filter (/= "") . map (dropWhile (== ' ')) . lines
  where
    replace [] = []
    replace ('\n' : xs) = ' ' : replace xs
    replace (x : xs) = x : replace xs

readRegAlpha :: String -> TokenPos -> Processor Register
readRegAlpha "" pos = throwTokenError pos "expected a register name, but no register name specified."
readRegAlpha (x : _) pos = do
  alpha <- Char.charToAlpha x & includeTokenStuff pos [i|invalid register name @${x}|]
  pure $ Alphabetic alpha

readLowerString :: String -> TokenPos -> Processor LowerString
readLowerString x pos = parseMaybe String.parseLowerString x
  & includeTokenStuff pos [i|expected a lower string (e.g. number, relativenumber), but a not lower string is gotten.|]

resolveParsingCamel :: TokenPos -> Either String Camel -> Processor Camel
resolveParsingCamel _ (Right x) = pure x
resolveParsingCamel pos (Left x) =
  throwError $ Failure x (OnAToken pos)
}
