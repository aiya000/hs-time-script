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

import Control.Arrow ((>>>))
import Control.Monad.Except (throwError)
import Data.Function ((&))
import Data.List.NonEmpty (NonEmpty(..), (<|))
import Data.Map.Strict (Map)
import Data.String.Here (i)
import Data.Text (Text)
import Data.Text.Prettyprint.Doc (pretty)
import Prelude
import Tim.Char (NumberChar(..))
import Tim.Lexer.Types (Token)
import Tim.Lexer.Types.Idents (pattern LetIdent)
import Tim.Parser.Types
import Tim.Processor
import Tim.String (LowerString)
import qualified Data.Map.Strict as Map
import qualified Text.Megaparsec as P
import qualified Tim.Char as Char
import qualified Tim.Lexer.Types as Token
import qualified Tim.Lexer.Types.Idents as Ident
import qualified Tim.String as String
}

%error { parseError }
%errorhandlertype explist
%monad { Processor }
%name parseAST
%tokentype { (Token, TokenPos) }

%token
  let            { whole@(Token.Command LetIdent, _)                        }
  ':'            { whole@(Token.Colon, _)                                   }
  typeIdent      { whole@(Token.Type $$, _)                                 }
  '='            { whole@(Token.Assign, _)                                  }
  nat            { whole@(Token.Literal (Token.Nat $$), _)                  }
  int            { whole@(Token.Literal (Token.Int $$), _)                  }
  float          { whole@(Token.Literal (Token.Float $$), _)                }
  singleQString  { whole@(Token.Literal (Token.String Token.SingleQ $$), _) }
  doubleQString  { whole@(Token.Literal (Token.String Token.DoubleQ $$), _) }
  '['            { whole@(Token.ListBegin, _)                               }
  ']'            { whole@(Token.ListEnd, _)                                 }
  '{'            { whole@(Token.DictBegin, _)                               }
  '}'            { whole@(Token.DictEnd, _)                                 }
  '('            { whole@(Token.ParenBegin, _)                              }
  ')'            { whole@(Token.ParenEnd, _)                                }
  ','            { whole@(Token.Comma, _)                                   }
  lineBreak      { whole@(Token.LineBreak, _)                               }

  varScopedG { whole@(Token.Var (String.unNonEmpty -> 'g' : ':' : $$), _) }
  varScopedS { whole@(Token.Var (String.unNonEmpty -> 's' : ':' : $$), _) }
  varScopedL { whole@(Token.Var (String.unNonEmpty -> 'l' : ':' : $$), _) }
  varScopedA { whole@(Token.Var (String.unNonEmpty -> 'a' : ':' : $$), _) }
  varScopedV { whole@(Token.Var (String.unNonEmpty -> 'v' : ':' : $$), _) }
  varScopedB { whole@(Token.Var (String.unNonEmpty -> 'b' : ':' : $$), _) }
  varScopedW { whole@(Token.Var (String.unNonEmpty -> 'w' : ':' : $$), _) }
  varScopedT { whole@(Token.Var (String.unNonEmpty -> 't' : ':' : $$), _) }

  varRegUnnamed   { whole@(Token.Var (String.unNonEmpty -> '@' : '"' : $$), _) }
  varRegSmallDel  { whole@(Token.Var (String.unNonEmpty -> '@' : '-' : $$), _) }
  varRegReadOnlyC { whole@(Token.Var (String.unNonEmpty -> '@' : ':' : $$), _) }
  varRegReadonlyD { whole@(Token.Var (String.unNonEmpty -> '@' : '.' : $$), _) }
  varRegReadOnlyP { whole@(Token.Var (String.unNonEmpty -> '@' : '%' : $$), _) }
  varRegBuffer    { whole@(Token.Var (String.unNonEmpty -> '@' : '#' : $$), _) }
  varRegExpr      { whole@(Token.Var (String.unNonEmpty -> '@' : '=' : $$), _) }
  varRegClipS     { whole@(Token.Var (String.unNonEmpty -> '@' : '*' : $$), _) }
  varRegClipP     { whole@(Token.Var (String.unNonEmpty -> '@' : '+' : $$), _) }
  varRegBlackHole { whole@(Token.Var (String.unNonEmpty -> '@' : '_' : $$), _) }
  varRegSeached   { whole@(Token.Var (String.unNonEmpty -> '@' : '/' : $$), _) }
  varReg0         { whole@(Token.Var (String.unNonEmpty -> '@' : '0' : $$), _) }
  varReg1         { whole@(Token.Var (String.unNonEmpty -> '@' : '1' : $$), _) }
  varReg2         { whole@(Token.Var (String.unNonEmpty -> '@' : '2' : $$), _) }
  varReg3         { whole@(Token.Var (String.unNonEmpty -> '@' : '3' : $$), _) }
  varReg4         { whole@(Token.Var (String.unNonEmpty -> '@' : '4' : $$), _) }
  varReg5         { whole@(Token.Var (String.unNonEmpty -> '@' : '5' : $$), _) }
  varReg6         { whole@(Token.Var (String.unNonEmpty -> '@' : '6' : $$), _) }
  varReg7         { whole@(Token.Var (String.unNonEmpty -> '@' : '7' : $$), _) }
  varReg8         { whole@(Token.Var (String.unNonEmpty -> '@' : '8' : $$), _) }
  varReg9         { whole@(Token.Var (String.unNonEmpty -> '@' : '9' : $$), _) }
  varRegAlpha     { whole@(Token.Var (String.unNonEmpty -> '@' : $$), _)       } -- a-zA-Z

  varOptionScopedL  { whole@(Token.Var (String.unNonEmpty -> '&' : 'l' : ':' : $$), _) }
  varOptionScopedG  { whole@(Token.Var (String.unNonEmpty -> '&' : 'g' : ':' : $$), _) }
  varOptionUnscoped { whole@(Token.Var (String.unNonEmpty -> '&' : $$), _)             }

  varSimpleLocal { whole@(Token.Var (String.unNonEmpty -> $$), _) }

%%

AST :: { AST }
  : Code { Code $1 }
  | Rhs  { Rhs $1  }

Code :: { Code }
  : {- empty -}           { []      }
  | Syntax lineBreak Code { $1 : $3 }

Syntax :: { Syntax }
  : let Lhs ':' typeIdent '=' Rhs { Let $2 (Just $4) $6 }
  | let Lhs '=' Rhs               { Let $2 Nothing $4   }

Lhs :: { Lhs }
  : VarIdent         { LVar $1     }
  | '[' DestVars ']' { LDestuct $2 }

VarIdent :: { VarIdent }
  : varScopedG      { Scoped G $1                  }
  | varScopedS      { Scoped S $1                  }
  | varScopedL      { Scoped L $1                  }
  | varScopedA      { Scoped A $1                  }
  | varScopedV      { Scoped V $1                  }
  | varScopedB      { Scoped B $1                  }
  | varScopedW      { Scoped W $1                  }
  | varScopedT      { Scoped T $1                  }
  | varRegUnnamed   { Register Unnamed             }
  | varRegSmallDel  { Register SmallDelete         }
  | varRegReadOnlyC { Register ReadOnlyColon       }
  | varRegReadonlyD { Register ReadOnlyDot         }
  | varRegReadOnlyP { Register ReadOnlyPercent     }
  | varRegBuffer    { Register Buffer              }
  | varRegExpr      { Register Expression          }
  | varRegClipS     { Register ClipboardStar       }
  | varRegClipP     { Register ClipboardPlus       }
  | varRegBlackHole { Register BlackHole           }
  | varRegSeached   { Register Searched            }
  | varReg0         { Register $ Numeric N0         }
  | varReg1         { Register $ Numeric N1         }
  | varReg2         { Register $ Numeric N2         }
  | varReg3         { Register $ Numeric N3         }
  | varReg4         { Register $ Numeric N4         }
  | varReg5         { Register $ Numeric N5         }
  | varReg6         { Register $ Numeric N6         }
  | varReg7         { Register $ Numeric N7         }
  | varReg8         { Register $ Numeric N8         }
  | varReg9         { Register $ Numeric N9         }
  | varRegAlpha       {% fmap Register $ readRegAlpha $1 (snd whole)                       }
  | varOptionScopedL  {% fmap (Option . LocalScopedOption) $ readLowerString $1 (snd whole)  }
  | varOptionScopedG  {% fmap (Option . GlobalScopedOption) $ readLowerString $1 (snd whole) }
  | varOptionUnscoped {% fmap (Option . UnscopedOption) $ readLowerString $1 (snd whole)     }
  | varSimpleLocal    { SimpleLocal $1 }

-- Destructive assignee variables
DestVars :: { NonEmpty VarIdent }
  : VarIdent              { ($1 :| []) }
  | VarIdent ',' DestVars { $1 <| $3   }

Rhs :: { Rhs }
  : VarIdent    { RVar $1    }
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

flattenMargins :: String -> String
flattenMargins = replace . unlines . filter (/= "") . map (dropWhile (== ' ')) . lines
  where
    replace [] = []
    replace ('\n' : xs) = ' ' : replace xs
    replace (x : xs) = x : replace xs

-- TODO: Show all [(Token, TokenPos)] if --verbose specified on cli.
parseError :: ([(Token, TokenPos)], [String]) -> Processor a
parseError (((got, pos):_), expected) =
  throwError . flip Failure (OnAToken pos) $ flattenMargins [i|
    got a token `${show $ pretty got}`
    at ${show $ pretty pos},
    but ${expected} are expected at here.
  |]
parseError ([], expected) =
  throwError $ Failure [i|got EOF, but ${expected} are expected at here.|] EOF

readRegAlpha :: String -> TokenPos -> Processor Register
readRegAlpha "" pos = throwTokenError pos "expected a register name, but no register name specified."
readRegAlpha (x : _) pos = do
  alpha <- Char.charToAlpha x & includeTokenStuff pos [i|invalid register name @${x}|]
  pure $ Alphabetic alpha

readLowerString :: String -> TokenPos -> Processor LowerString
readLowerString x pos = P.parseMaybe String.parseLowerString x
  & includeTokenStuff pos [i|expected a lower string (e.g. number, relativenumber), but a not lower string is gotten.|]
}
